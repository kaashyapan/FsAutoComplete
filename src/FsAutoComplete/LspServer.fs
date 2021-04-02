module FsAutoComplete.LspServer

open FsAutoComplete
open StreamJsonRpc
open System.IO
open FSharp.Control.Tasks.Affine
open LanguageServerProtocol.Types
open System.Diagnostics
open Newtonsoft.Json.Linq
open Newtonsoft.Json
open FsAutoComplete.Logging
open System
open FsToolkit.ErrorHandling
open FsAutoComplete.CodeFix
open FsAutoComplete.CodeFix.Types
open System.Threading.Tasks
open System.Threading
open FSharp.Analyzers
open FsAutoComplete.LspHelpers
open FSharp.UMX
open Ionide.ProjInfo.ProjectSystem
open FSharp.Compiler.SourceCodeServices

module FcsRange = FSharp.Compiler.Text.Range
type FcsRange = FSharp.Compiler.Text.Range
module FcsPos = FSharp.Compiler.Text.Pos
type FcsPos = FSharp.Compiler.Text.Pos

/// I got really tired of adding the dang property to everything
type RpcMethodAttribute(name: string) =
  inherit JsonRpcMethodAttribute(name, UseSingleObjectParameterDeserialization = true)

type LspResult<'t> = Result<'t, StreamJsonRpc.LocalRpcException>
type AsyncLspResult<'t> = Async<LspResult<'t>>

type Async =
  static member startTaskWithCancel ctok a = Async.StartAsTask(a, cancellationToken = ctok)

module AsyncLspResult =
  let internalError msg = StreamJsonRpc.LocalRpcException(msg)

module CoreResponse =
  let toRpcError =
    function
    | CoreResponse.Res a -> a
    | CoreResponse.ErrorRes msg
    | CoreResponse.InfoRes msg -> raise (AsyncLspResult.internalError msg)

module AsyncResult =
  let ofCoreResponse (ar: Async<CoreResponse<'a>>) = ar |> Async.map CoreResponse.toRpcError

module Result =
  let ofCoreResponse (r: Result<CoreResponse<'a>, _>) =
    r |> Result.fold CoreResponse.toRpcError (fun e -> raise (AsyncLspResult.internalError e))

module Mapping =
  let mapWorkspacePeek interestingItems =
    let payload : CommandResponse.WorkspacePeekResponse = { Found = interestingItems |> List.map CommandResponse.mapInteresting }

    let responseMessage : CommandResponse.ResponseMsg<_> = { Kind = "workspacePeek"; Data = payload }
    responseMessage

type FSharpLspClient(rpc: JsonRpc) =
  let logger = LogProvider.getLoggerByType typeof<FSharpLspClient>

  abstract NotifyWorkspacePeek : interestingItems: list<Ionide.ProjInfo.ProjectSystem.WorkspacePeek.Interesting> -> Task
  default x.NotifyWorkspacePeek(interestingItems: list<Ionide.ProjInfo.ProjectSystem.WorkspacePeek.Interesting>) =
    rpc.NotifyWithParameterObjectAsync("fsharp/workspacePeek", Mapping.mapWorkspacePeek interestingItems)

  abstract TextDocumentPublishDiagnostics : uri: DocumentUri * diagnostics: Diagnostic [] -> Task
  default x.TextDocumentPublishDiagnostics(uri: DocumentUri, diagnostics: Diagnostic []) =
    rpc.NotifyWithParameterObjectAsync(
      "textDocument/publishDiagnostics",
      ({ Diagnostics = diagnostics; Uri = uri }: PublishDiagnosticsParams)
    )

  abstract NotifyFileParsed: localPath : string<LocalPath> -> Task
  default x.NotifyFileParsed(localPath: string<LocalPath>) =
    rpc.NotifyWithParameterObjectAsync("fsharp/fileParsed", { Content = UMX.untag localPath })

  abstract NotifyWorkspace: project: ProjectResponse -> Task
  default x.NotifyWorkspace(project: ProjectResponse) =
    let payload =
      match project with
      | ProjectResponse.Project (x, _) -> CommandResponse.project JsonSerializer.writeJson x
      | ProjectResponse.ProjectError (_, errorDetails) -> CommandResponse.projectError JsonSerializer.writeJson errorDetails
      | ProjectResponse.ProjectLoading (projectFileName) -> CommandResponse.projectLoading JsonSerializer.writeJson projectFileName
      | ProjectResponse.WorkspaceLoad (finished) -> CommandResponse.workspaceLoad JsonSerializer.writeJson finished
      | ProjectResponse.ProjectChanged (projectFileName) -> failwith "Not Implemented"

    rpc.NotifyWithParameterObjectAsync("fsharp/notifyWorkspace", { Content = payload })

  abstract NotifyCancelledRequest: message: string -> Task
  default x.NotifyCancelledRequest(message: string) = rpc.NotifyWithParameterObjectAsync("fsharp/notifyCancel", { Content = message })

type FSharpLspServer(sender: Stream, reader: Stream, backgroundServiceEnabled: bool, state: State) as this =

  let logger = LogProvider.getLoggerByType typeof<FSharpLspServer>
  // using a custom formatter so that we can add our own
  // * converters
  // * naming strategy
  // * ignoring of missing members on payloads for forward-compatibility
  // this lines up with the serializers used in the prior implementation
  let formatter : IJsonRpcMessageTextFormatter =
    let formatter = new JsonMessageFormatter()

    for converter in LanguageServerProtocol.Server.customConverters do
      formatter.JsonSerializer.Converters.Add converter

    formatter.JsonSerializer.MissingMemberHandling <- MissingMemberHandling.Ignore
    formatter.JsonSerializer.ContractResolver <- Newtonsoft.Json.Serialization.CamelCasePropertyNamesContractResolver()
    formatter :> _
  // LSP uses a header-delimited message stream, so we use the handler that understands that format
  let handler = new StreamJsonRpc.HeaderDelimitedMessageHandler(sender, reader, formatter)
  let rpc = new JsonRpc(handler, this)
  let mutable client = FSharpLspClient(rpc)

  let backgroundService : BackgroundServices.BackgroundService =
    if backgroundServiceEnabled then
      BackgroundServices.ActualBackgroundService() :> _
    else
      BackgroundServices.MockBackgroundService() :> _

  let mutable commands = new Commands(FSharpCompilerServiceChecker(backgroundServiceEnabled, false), state, backgroundService, false)

  let mutable commandDisposables = ResizeArray()

  let mutable clientCapabilities : ClientCapabilities option = None
  let mutable glyphToCompletionKind = LspHelpers.GlyphConversions.glyphToCompletionKindGenerator None
  let mutable glyphToSymbolKind = LspHelpers.GlyphConversions.glyphToSymbolKindGenerator None
  let subscriptions = ResizeArray<IDisposable>()

  let mutable config = LspHelpers.FSharpConfig.Default
  let mutable rootPath : string option = None
  let mutable codeFixes = fun p -> [||]

  //TODO: Thread safe version
  let lintFixes = System.Collections.Generic.Dictionary<DocumentUri, (LanguageServerProtocol.Types.Range * TextEdit) list>()

  let analyzerFixes =
    System.Collections.Generic.Dictionary<DocumentUri, System.Collections.Generic.Dictionary<DocumentUri, (LanguageServerProtocol.Types.Range * TextEdit) list>>
      ()

  let diagnosticCollections = System.Collections.Concurrent.ConcurrentDictionary<DocumentUri * string, Diagnostic []>()

  let sendDiagnostics (uri: DocumentUri) =
    let diags =
      diagnosticCollections
      |> Seq.collect
           (fun kv ->
             let (u, _) = kv.Key
             if u = uri then kv.Value else [||])
      |> Seq.sortBy (fun n -> n.Range.Start.Line)
      |> Seq.toArray

    logger.info (
      Log.setMessage "SendDiag for {file}: {diags} entries"
      >> Log.addContextDestructured "file" uri
      >> Log.addContextDestructured "diags" diags.Length
    )

    client.TextDocumentPublishDiagnostics(uri, diags) |> ignore<Task>

  let handleCommandEvents (n: NotificationEvent) =
    try
      match n with
      | NotificationEvent.FileParsed fn -> client.NotifyFileParsed fn |> ignore<Task>
      | NotificationEvent.Workspace ws ->
          logger.info (Log.setMessage "Workspace Notify {ws}" >> Log.addContextDestructured "ws" ws)
          client.NotifyWorkspace ws |> ignore<Task>

      | NotificationEvent.ParseError (errors, file) ->
          let uri = Path.LocalPathToUri file
          diagnosticCollections.AddOrUpdate((uri, "F# Compiler"), [||], (fun _ _ -> [||])) |> ignore

          let diags = errors |> Array.map (fcsErrorToDiagnostic)
          diagnosticCollections.AddOrUpdate((uri, "F# Compiler"), diags, (fun _ _ -> diags)) |> ignore
          sendDiagnostics uri

      | NotificationEvent.UnusedOpens (file, opens) ->
          let uri = Path.LocalPathToUri file
          diagnosticCollections.AddOrUpdate((uri, "F# Unused opens"), [||], (fun _ _ -> [||])) |> ignore

          let diags =
            opens
            |> Array.map
                 (fun n ->
                   { Diagnostic.Range = fcsRangeToLsp n
                     Code = None
                     Severity = Some DiagnosticSeverity.Hint
                     Source = "FSAC"
                     Message = "Unused open statement"
                     RelatedInformation = Some [||]
                     Tags = Some [| DiagnosticTag.Unnecessary |] })

          diagnosticCollections.AddOrUpdate((uri, "F# Unused opens"), diags, (fun _ _ -> diags)) |> ignore
          sendDiagnostics uri

      | NotificationEvent.UnusedDeclarations (file, decls) ->
          let uri = Path.LocalPathToUri file

          diagnosticCollections.AddOrUpdate((uri, "F# Unused declarations"), [||], (fun _ _ -> [||])) |> ignore

          let diags =
            decls
            |> Array.map
                 (fun (n, t) ->
                   { Diagnostic.Range = fcsRangeToLsp n
                     Code = (if t then Some "1" else None)
                     Severity = Some DiagnosticSeverity.Hint
                     Source = "FSAC"
                     Message = "This value is unused"
                     RelatedInformation = Some [||]
                     Tags = Some [| DiagnosticTag.Unnecessary |] })

          diagnosticCollections.AddOrUpdate((uri, "F# Unused declarations"), diags, (fun _ _ -> diags)) |> ignore

          sendDiagnostics uri

      | NotificationEvent.SimplifyNames (file, decls) ->
          let uri = Path.LocalPathToUri file
          diagnosticCollections.AddOrUpdate((uri, "F# simplify names"), [||], (fun _ _ -> [||])) |> ignore

          let diags =
            decls
            |> Array.map
                 (fun { Range = range; RelativeName = _relName } ->
                   { Diagnostic.Range = fcsRangeToLsp range
                     Code = None
                     Severity = Some DiagnosticSeverity.Hint
                     Source = "FSAC"
                     Message = "This qualifier is redundant"
                     RelatedInformation = Some [||]
                     Tags = Some [| DiagnosticTag.Unnecessary |] })

          diagnosticCollections.AddOrUpdate((uri, "F# simplify names"), diags, (fun _ _ -> diags)) |> ignore
          sendDiagnostics uri

      // | NotificationEvent.Lint (file, warnings) ->
      //     let uri = Path.LocalPathToUri file
      //     diagnosticCollections.AddOrUpdate((uri, "F# Linter"), [||], fun _ _ -> [||]) |> ignore

      //     let fs =
      //         warnings |> List.choose (fun w ->
      //             w.Warning.Details.SuggestedFix
      //             |> Option.bind (fun f ->
      //                 let f = f.Force()
      //                 let range = fcsRangeToLsp w.Warning.Details.Range
      //                 f |> Option.map (fun f -> range, {Range = range; NewText = f.ToText})
      //             )
      //         )

      //     lintFixes.[uri] <- fs
      //     let diags =
      //         warnings |> List.map(fun w ->
      //             // ideally we'd be able to include a clickable link to the docs page for this errorlint code, but that is not the case here
      //             // neither the Message or the RelatedInformation structures support markdown.
      //             let range = fcsRangeToLsp w.Warning.Details.Range
      //             { Diagnostic.Range = range
      //               Code = Some w.Code
      //               Severity = Some DiagnosticSeverity.Information
      //               Source = "F# Linter"
      //               Message = w.Warning.Details.Message
      //               RelatedInformation = None
      //               Tags = None }
      //         )
      //         |> List.toArray
      //     diagnosticCollections.AddOrUpdate((uri, "F# Linter"), diags, fun _ _ -> diags) |> ignore
      //     sendDiagnostics uri

      | NotificationEvent.Canceled (msg) -> client.NotifyCancelledRequest msg |> ignore<Task>
      | NotificationEvent.Diagnostics (p) -> client.TextDocumentPublishDiagnostics(p.Uri, p.Diagnostics) |> ignore<Task>

      | NotificationEvent.AnalyzerMessage (messages, file) ->

          let uri = Path.LocalPathToUri file
          diagnosticCollections.AddOrUpdate((uri, "F# Analyzers"), [||], (fun _ _ -> [||])) |> ignore

          match messages with
          | [||] -> diagnosticCollections.AddOrUpdate((uri, "F# Analyzers"), [||], (fun _ _ -> [||])) |> ignore
          | messages ->
              let fs =
                messages
                |> Seq.collect
                     (fun w ->
                       w.Fixes
                       |> List.map
                            (fun f ->
                              let range = fcsRangeToLsp f.FromRange
                              range, { Range = range; NewText = f.ToText }))
                |> Seq.toList

              let aName = messages.[0].Type

              if analyzerFixes.ContainsKey uri then () else analyzerFixes.[uri] <- new System.Collections.Generic.Dictionary<_, _>()

              analyzerFixes.[uri].[aName] <- fs

              let diag =
                messages
                |> Array.map
                     (fun m ->
                       let range = fcsRangeToLsp m.Range

                       let s =
                         match m.Severity with
                         | FSharp.Analyzers.SDK.Info -> DiagnosticSeverity.Information
                         | FSharp.Analyzers.SDK.Warning -> DiagnosticSeverity.Warning
                         | FSharp.Analyzers.SDK.Error -> DiagnosticSeverity.Error

                       { Diagnostic.Range = range
                         Code = None
                         Severity = Some s
                         Source = sprintf "F# Analyzers (%s)" m.Type
                         Message = m.Message
                         RelatedInformation = None
                         Tags = None })

              diagnosticCollections.AddOrUpdate((uri, "F# Analyzers"), diag, (fun _ _ -> diag)) |> ignore
              sendDiagnostics uri
    with _ -> ()

  /// centralize any state changes when the config is updated here
  let updateConfig (newConfig: LspHelpers.FSharpConfig) =
    let toCompilerToolArgument (path: string) = sprintf "--compilertool:%s" path
    config <- newConfig

    let hadAnalyzersBefore = SDK.Client.registeredAnalyzers.Count <> 0

    match config.AnalyzersPath with
    | [||] ->
        Loggers.analyzers.info (Log.setMessage "Analyzers unregistered")
        SDK.Client.registeredAnalyzers.Clear()
    | paths ->
        for path in paths do
          let (newlyFound, total) = SDK.Client.loadAnalyzers path

          Loggers.analyzers.info (
            Log.setMessage "Registered {count} analyzers from {path}"
            >> Log.addContextDestructured "count" newlyFound
            >> Log.addContextDestructured "path" path
          )

        let total = SDK.Client.registeredAnalyzers.Count

        Loggers.analyzers.info (Log.setMessage "{count} Analyzers registered overall" >> Log.addContextDestructured "count" total)

    let hasAnalyzersNow = SDK.Client.registeredAnalyzers.Count <> 0

    if hadAnalyzersBefore <> hasAnalyzersNow then
      let oldCommands = commands
      let oldDisposables = commandDisposables

      let newCommands =
        new Commands(FSharpCompilerServiceChecker(backgroundServiceEnabled, hasAnalyzersNow), state, backgroundService, hasAnalyzersNow)

      commands <- newCommands
      commandDisposables <- ResizeArray()
      commands.SetWorkspaceRoot rootPath
      commandDisposables.Add(commands.Notify.Subscribe handleCommandEvents)

      for (disposable: IDisposable) in oldDisposables do
        disposable.Dispose()

      (oldCommands :> IDisposable).Dispose()

    // only update the dotnet root if it's both a directory and exists
    let di = DirectoryInfo config.DotNetRoot

    if di.Exists then
      commands.SetDotnetSDKRoot di
    else
      // if we were mistakenly given the path to a dotnet binary
      // then use the parent directory as the dotnet root instead
      let fi = FileInfo(di.FullName)

      if fi.Exists && (fi.Name = "dotnet" || fi.Name = "dotnet.exe") then commands.SetDotnetSDKRoot(fi.Directory)

    commands.SetFSIAdditionalArguments
      [| yield! config.FSICompilerToolLocations |> Array.map toCompilerToolArgument; yield! config.FSIExtraParameters |]

    commands.SetLinterConfigRelativePath config.LinterConfig

    match config.AnalyzersPath with
    | [||] ->
        Loggers.analyzers.info (Log.setMessage "Analyzers unregistered")
        FSharp.Analyzers.SDK.Client.registeredAnalyzers.Clear()
    | paths ->
        for path in paths do
          let (newlyFound, total) = FSharp.Analyzers.SDK.Client.loadAnalyzers path

          Loggers.analyzers.info (
            Log.setMessage "Registered {count} analyzers from {path}"
            >> Log.addContextDestructured "count" newlyFound
            >> Log.addContextDestructured "path" path
          )

        let total = FSharp.Analyzers.SDK.Client.registeredAnalyzers.Count

        Loggers.analyzers.info (Log.setMessage "{count} Analyzers registered overall" >> Log.addContextDestructured "count" total)

  let parseFile (p: DidChangeTextDocumentParams) =
    async {
      if not commands.IsWorkspaceReady && rootPath.IsSome then
        logger.warn (Log.setMessage "ParseFile - Workspace not ready")
      else
        let doc = p.TextDocument
        let filePath = doc.GetFilePath() |> Utils.normalizePath
        let contentChange = p.ContentChanges |> Seq.tryLast

        match contentChange, doc.Version with
        | Some contentChange, Some version ->
            if contentChange.Range.IsNone && contentChange.RangeLength.IsNone then
              let content = contentChange.Text.Split('\n')
              let tfmConfig = config.UseSdkScripts
              logger.info (Log.setMessage "ParseFile - Parsing {file}" >> Log.addContextDestructured "file" filePath)
              do! (commands.Parse filePath content version (Some tfmConfig) |> Async.Ignore)

              // if config.Linter then do! (commands.Lint filePath |> Async.Ignore)
              if config.UnusedOpensAnalyzer then Async.Start(commands.CheckUnusedOpens filePath)
              if config.UnusedDeclarationsAnalyzer then Async.Start(commands.CheckUnusedDeclarations filePath) //fire and forget this analyzer now that it's syncronous
              if config.SimplifyNameAnalyzer then Async.Start(commands.CheckSimplifiedNames filePath)
            else
              logger.warn (Log.setMessage "ParseFile - Parse not started, received partial change")
        | _ -> logger.info (Log.setMessage "ParseFile - Found no change for {file}" >> Log.addContextDestructured "file" filePath)
    }
    |> Async.Start

  let parseFileDebuncer = Debounce(500, parseFile)

  do
    // hook up request/response logging for debugging
    rpc.TraceSource <- new TraceSource(typeof<FSharpLspServer>.Name, SourceLevels.Verbose)

    rpc.TraceSource.Listeners.Add(new SerilogTraceListener.SerilogTraceListener(typeof<FSharpLspServer>.Name))
    |> ignore<int>
    // start the pipes flowing
    rpc.StartListening()

    commandDisposables.Add <| commands.Notify.Subscribe handleCommandEvents

  /// provides access to the underlying RPC client representing the hosting application for this LSP server instance
  member val Client = client with get, set

  /// returns a hot task that resolves when the stream has terminated
  member x.WaitForClose = rpc.Completion

  ///Helper function for handling Position requests using **recent** type check results
  member x.positionHandler<'a, 'b when 'b :> ITextDocumentPositionParams>
    (f: 'b -> FcsPos -> ParseAndCheckResults -> string -> string [] -> Async<'a>)
    (arg: 'b)
    : Async<'a> =
    async {
      let pos = arg.GetFcsPos()
      let file = arg.GetFilePath() |> Utils.normalizePath

      return!
        match commands.TryGetFileCheckerOptionsWithLinesAndLineStr(file, pos) with
        | ResultOrString.Error s ->
            logger.error (
              Log.setMessage "PositionHandler - Getting file checker options for {file} failed"
              >> Log.addContextDestructured "error" s
              >> Log.addContextDestructured "file" file
            )

            raise (AsyncLspResult.internalError s)
        | ResultOrString.Ok (options, lines, lineStr) ->
            try
              let tyResOpt = commands.TryGetRecentTypeCheckResultsForFile(file, options)

              match tyResOpt with
              | None ->
                  logger.info (
                    Log.setMessage "PositionHandler - Cached typecheck results not yet available for {file}"
                    >> Log.addContextDestructured "file" file
                  )

                  raise (AsyncLspResult.internalError "Cached typecheck results not yet available")
              | Some tyRes -> f arg pos tyRes lineStr lines
            with e ->
              logger.error (
                Log.setMessage "PositionHandler - Operation failed for file {file}"
                >> Log.addContextDestructured "file" file
                >> Log.addExn e
              )

              raise (AsyncLspResult.internalError e.Message)
    }

  ///Helper function for handling Position requests using **latest** type check results
  member x.positionHandlerWithLatest<'a, 'b when 'b :> ITextDocumentPositionParams>
    (f: 'b -> FcsPos -> ParseAndCheckResults -> string -> string [] -> Async<'a>)
    (arg: 'b)
    : Async<'a> =
    async {
      let pos = arg.GetFcsPos()
      let file = arg.GetFilePath() |> Utils.normalizePath

      try
        let! tyResOpt = commands.TryGetLatestTypeCheckResultsForFile(file)

        match tyResOpt with
        | None ->
            logger.error (
              Log.setMessage "PositionHandler - Cached typecheck results for {file} not yet available and are required"
              >> Log.addContextDestructured "file" file
            )

            return raise (AsyncLspResult.internalError "Cached typecheck results not yet available")
        | Some tyRes ->
            match commands.TryGetFileCheckerOptionsWithLinesAndLineStr(file, pos) with
            | ResultOrString.Error s ->
                logger.error (
                  Log.setMessage "PositionHandler - Getting file checker options for {file} failed"
                  >> Log.addContextDestructured "error" s
                  >> Log.addContextDestructured "file" file
                )

                return raise (AsyncLspResult.internalError s)
            | ResultOrString.Ok (options, lines, lineStr) -> return! f arg pos tyRes lineStr lines
      with e ->
        logger.error (
          Log.setMessage "PositionHandler - Operation failed for file {file}"
          >> Log.addContextDestructured "file" file
          >> Log.addExn e
        )

        return raise (AsyncLspResult.internalError e.Message)
    }

  ///Helper function for handling file requests using **recent** type check results
  member x.fileHandler<'a> (f: string<LocalPath> -> ParseAndCheckResults -> string [] -> Async<'a>) (file: string<LocalPath>) : Async<'a> =
    async {
      return!
        match commands.TryGetFileCheckerOptionsWithLines(file) with
        | ResultOrString.Error s ->
            logger.error (
              Log.setMessage "FileHandler - Getting file checker options for {file} failed"
              >> Log.addContextDestructured "error" s
              >> Log.addContextDestructured "file" file
            )

            raise (AsyncLspResult.internalError s)
        | ResultOrString.Ok (options, lines) ->
            try
              let tyResOpt = commands.TryGetRecentTypeCheckResultsForFile(file, options)

              match tyResOpt with
              | None ->
                  logger.info (
                    Log.setMessage "FileHandler - Cached typecheck results not yet available for {file}"
                    >> Log.addContextDestructured "file" file
                  )

                  raise (AsyncLspResult.internalError "Cached typecheck results not yet available")
              | Some tyRes ->
                  async {
                    let! r = Async.Catch(f file tyRes lines)

                    match r with
                    | Choice1Of2 r -> return r
                    | Choice2Of2 e ->
                        logger.error (
                          Log.setMessage "FileHandler - Failed during child operation on file {file}"
                          >> Log.addContextDestructured "file" file
                          >> Log.addExn e
                        )

                        return raise (AsyncLspResult.internalError e.Message)
                  }
            with e ->
              logger.error (
                Log.setMessage "FileHandler - Operation failed for file {file}"
                >> Log.addContextDestructured "file" file
                >> Log.addExn e
              )

              raise (AsyncLspResult.internalError e.Message)
    }


  // the names have to match exactly, which can suck if they have / characters
  // also, you have to match the parameters _exactly_, which means it's safer usually to have
  // a parameter-object and turn on missing member handling ignoring in the json formatter.
  [<RpcMethod("initialize")>]
  member x.Initialize(p: InitializeParams, ctok: CancellationToken) =
    task {
      logger.info (Log.setMessage "Initialize Request {p}" >> Log.addContextDestructured "p" p)

      let actualRootPath =
        match p.RootUri with
        | Some rootUri -> Some(Path.FileUriToLocalPath rootUri)
        | None -> p.RootPath

      rootPath <- actualRootPath

      let c =
        p.InitializationOptions
        |> Option.bind (fun options -> if options.HasValues then Some options else None)
        |> Option.map LanguageServerProtocol.Server.deserialize<LspHelpers.FSharpConfigDto>
        |> Option.map LspHelpers.FSharpConfig.FromDto
        |> Option.defaultValue LspHelpers.FSharpConfig.Default

      updateConfig c

      clientCapabilities <- p.Capabilities
      glyphToCompletionKind <- LspHelpers.GlyphConversions.glyphToCompletionKindGenerator clientCapabilities
      glyphToSymbolKind <- LspHelpers.GlyphConversions.glyphToSymbolKindGenerator clientCapabilities

      let tryGetParseResultsForFile fileName pos =
        asyncResult {
          let! (projectOptions, fileLines, lineAtPos) = commands.TryGetFileCheckerOptionsWithLinesAndLineStr(fileName, pos)

          match! commands.TryGetLatestTypeCheckResultsForFile(fileName) with
          | None -> return! Error $"No typecheck results available for %A{fileName}"
          | Some tyRes -> return tyRes, lineAtPos, fileLines
        }

      let getFileLines = commands.TryGetFileCheckerOptionsWithLines >> Result.map snd

      let getRangeText fileName range = getFileLines fileName |> Result.map (fun lines -> LspHelpers.Conversions.getText lines range)

      let getLineText lines range = LspHelpers.Conversions.getText lines range
      let getProjectOptsAndLines = commands.TryGetFileCheckerOptionsWithLinesAndLineStr
      let tryGetProjectOptions = commands.TryGetFileCheckerOptionsWithLines >> Result.map fst

      let interfaceStubReplacements =
        Map.ofList
          [ "$objectIdent", config.InterfaceStubGenerationObjectIdentifier
            "$methodBody", config.InterfaceStubGenerationMethodBody ]

      let getInterfaceStubReplacements () = interfaceStubReplacements

      let unionCaseStubReplacements = Map.ofList [ "$1", config.UnionCaseStubGenerationBody ]

      let getUnionCaseStubReplacements () = unionCaseStubReplacements

      let recordStubReplacements = Map.ofList [ "$1", config.RecordStubGenerationBody ]

      let getRecordStubReplacements () = recordStubReplacements

      let abstractClassStubReplacements =
        Map.ofList
          [ "$objectIdent", config.AbstractClassStubGenerationObjectIdentifier
            "$methodBody", config.AbstractClassStubGenerationMethodBody ]

      let getAbstractClassStubReplacements () = abstractClassStubReplacements

      codeFixes <-
        fun p ->
          [| Run.ifEnabled (fun _ -> config.UnusedOpensAnalyzer) UnusedOpens.fix
             Run.ifEnabled
               (fun _ -> config.ResolveNamespaces)
               (ResolveNamespace.fix tryGetParseResultsForFile commands.GetNamespaceSuggestions)
             SuggestedIdentifier.fix
             RedundantQualifier.fix
             UnusedValue.fix getRangeText
             NewWithDisposables.fix getRangeText
             Run.ifEnabled
               (fun _ -> config.UnionCaseStubGeneration)
               (GenerateUnionCases.fix
                 getFileLines
                 tryGetParseResultsForFile
                 commands.GetUnionPatternMatchCases
                 getUnionCaseStubReplacements)
             ExternalSystemDiagnostics.linter
               (fun fileUri ->
                 match lintFixes.TryGetValue(fileUri) with
                 | (true, v) -> Some v
                 | (false, _) -> None)
             ExternalSystemDiagnostics.analyzers
               (fun fileUri ->
                 match analyzerFixes.TryGetValue(fileUri) with
                 | (true, v) -> Some(v.Values |> Seq.concat |> Seq.toList)
                 | (false, _) -> None)
             Run.ifEnabled
               (fun _ -> config.InterfaceStubGeneration)
               (GenerateInterfaceStub.fix tryGetParseResultsForFile commands.GetInterfaceStub getInterfaceStubReplacements)
             Run.ifEnabled
               (fun _ -> config.RecordStubGeneration)
               (GenerateRecordStub.fix tryGetParseResultsForFile commands.GetRecordStub getRecordStubReplacements)
             Run.ifEnabled
               (fun _ -> config.AbstractClassStubGeneration)
               (GenerateAbstractClassStub.fix tryGetParseResultsForFile commands.GetAbstractClassStub getAbstractClassStubReplacements)
             MissingEquals.fix getFileLines
             NegationToSubtraction.fix getFileLines
             DoubleEqualsToSingleEquals.fix getRangeText
             ColonInFieldType.fix
             ParenthesizeExpression.fix getRangeText
             RefCellAccesToNot.fix tryGetParseResultsForFile
             UseSafeCastInsteadOfUnsafe.fix getRangeText
             MakeDeclarationMutable.fix tryGetParseResultsForFile tryGetProjectOptions
             ChangeComparisonToMutableAssignment.fix tryGetParseResultsForFile
             ConvertInvalidRecordToAnonRecord.fix tryGetParseResultsForFile
             RemoveUnnecessaryReturnOrYield.fix tryGetParseResultsForFile getLineText
             RemoveUnnecessaryReturnOrYield.fix tryGetParseResultsForFile getLineText
             ChangeCSharpLambdaToFSharp.fix tryGetParseResultsForFile getLineText
             AddMissingFunKeyword.fix getFileLines getLineText
             MakeOuterBindingRecursive.fix tryGetParseResultsForFile getLineText
             AddMissingRecKeyword.fix getFileLines getLineText
             ConvertBangEqualsToInequality.fix getRangeText
             ReplaceBangWithValueFunction.fix tryGetParseResultsForFile getLineText
             RemoveUnusedBinding.fix tryGetParseResultsForFile
             AddTypeToIndeterminateValue.fix tryGetParseResultsForFile tryGetProjectOptions |]
          |> Array.map
               (fun fixer ->
                 async {
                   let! fixes = fixer p
                   return List.map (CodeAction.OfFix commands.TryGetFileVersion clientCapabilities.Value) fixes
                 })

      match p.RootPath, c.AutomaticWorkspaceInit with
      | None, _
      | _, false -> ()
      | Some p, true ->
          async {
            let ints = commands.WorkspacePeek p config.WorkspaceModePeekDeepLevel (List.ofArray config.ExcludeProjectDirectories)

            let serialized = CommandResponse.workspacePeek JsonSerializer.writeJson ints
            client.NotifyWorkspacePeek ints |> ignore<Task>

            let peeks =
              ints
              |> List.map LspHelpers.Workspace.mapInteresting
              |> List.sortByDescending
                   (fun x ->
                     match x with
                     | CommandResponse.WorkspacePeekFound.Solution sln -> LspHelpers.Workspace.countProjectsInSln sln
                     | CommandResponse.WorkspacePeekFound.Directory _ -> -1)

            match peeks with
            | [] -> ()
            | [ CommandResponse.WorkspacePeekFound.Directory projs ] ->
                commands.WorkspaceLoad projs.Fsprojs false config.ScriptTFM config.GenerateBinlog |> Async.Ignore |> Async.Start
            | CommandResponse.WorkspacePeekFound.Solution sln :: _ ->
                let projs = sln.Items |> List.collect LspHelpers.Workspace.foldFsproj |> List.map fst

                commands.WorkspaceLoad projs false config.ScriptTFM config.GenerateBinlog |> Async.Ignore |> Async.Start
            | _ ->
                //TODO: Above case always picks solution with most projects, should be changed
                ()

            return ()
          }
          |> Async.Start

      return
        { InitializeResult.Default with
            Capabilities =
              { ServerCapabilities.Default with
                  HoverProvider = Some true
                  RenameProvider = Some true
                  DefinitionProvider = Some true
                  TypeDefinitionProvider = Some true
                  ImplementationProvider = Some true
                  ReferencesProvider = Some true
                  DocumentHighlightProvider = Some true
                  DocumentSymbolProvider = Some true
                  WorkspaceSymbolProvider = Some true
                  DocumentFormattingProvider = Some true
                  DocumentRangeFormattingProvider = Some false
                  SignatureHelpProvider =
                    Some { TriggerCharacters = Some [| '('; ','; ' ' |]; RetriggerCharacters = Some [| ','; ')'; ' ' |] }
                  CompletionProvider =
                    Some
                      { ResolveProvider = Some true
                        TriggerCharacters = Some([| '.'; ''' |])
                        AllCommitCharacters = None //TODO: what chars shoudl commit completions?
                      }
                  CodeLensProvider = Some { CodeLensOptions.ResolveProvider = Some true }
                  CodeActionProvider = Some true
                  TextDocumentSync =
                    Some
                      { TextDocumentSyncOptions.Default with
                          OpenClose = Some true
                          Change = Some TextDocumentSyncKind.Full
                          Save = Some { IncludeText = Some true } }
                  FoldingRangeProvider = Some true
                  SelectionRangeProvider = Some true
                  SemanticTokensProvider =
                    Some
                      { Legend =
                          FsAutoComplete.LspHelpers.createTokenLegend<LspHelpers.ClassificationUtils.SemanticTokenTypes, LspHelpers.ClassificationUtils.SemanticTokenModifier>
                        Range = Some(LanguageServerProtocol.LspJsonConverters.U2.First true)
                        Full = Some(LanguageServerProtocol.LspJsonConverters.U2.First true) } } }
    }

  [<RpcMethod("fsharp/workspacePeek")>]
  member x.FSharpWorkspacePeek(p: LspHelpers.WorkspacePeekRequest) =
    task {
      let found = commands.WorkspacePeek p.Directory p.Deep (p.ExcludedDirs |> List.ofArray)
      let mapped = Mapping.mapWorkspacePeek found
      let serialized = JsonSerializer.writeJson mapped
      return { PlainNotification.Content = serialized }
    }

  [<RpcMethod("fsharp/workspaceLoad")>]
  member x.FSharpWorkspaceLoad(p: LspHelpers.WorkspaceLoadParms, ctok) =
    task {
      let fns = p.TextDocuments |> Array.map (fun fn -> fn.GetFilePath()) |> Array.toList

      let! fin =
        commands.WorkspaceLoad fns config.DisableInMemoryProjectReferences config.ScriptTFM config.GenerateBinlog
        |> Async.startTaskWithCancel ctok

      return { Content = CommandResponse.workspaceLoad FsAutoComplete.JsonSerializer.writeJson fin }
    }

  [<JsonRpcMethod("initialized")>]
  member _.Initialized() = task { return () }

  [<RpcMethod("workspace/didChangeConfiguration")>]
  member _.WorkspaceDidChangeConfiguration(workspaceSettings: DidChangeConfigurationParams) =
    task {
      let dto = LanguageServerProtocol.Server.deserialize<FSharpConfigRequest> workspaceSettings.Settings

      logger.info (Log.setMessage "WorkspaceDidChangeConfiguration Request: {parms}" >> Log.addContextDestructured "parms" dto)

      let c = config.AddDto dto.FSharp
      updateConfig c
      return ()
    }

  [<RpcMethod("textDocument/didOpen")>]
  member _.TextDocumentDidOpen(p: DidOpenTextDocumentParams, ctok) =
    task {
      let doc = p.TextDocument
      let filePath = doc.GetFilePath() |> Utils.normalizePath
      let content = doc.Text.Split('\n')
      let tfmConfig = config.UseSdkScripts
      logger.info (Log.setMessage "TextDocumentDidOpen Request: {parms}" >> Log.addContextDestructured "parms" filePath)

      commands.SetFileContent(filePath, content, Some doc.Version, config.ScriptTFM)

      if not commands.IsWorkspaceReady && rootPath.IsSome then
        do! commands.WorkspaceReady |> Async.AwaitEvent
        logger.info (Log.setMessage "TextDocumentDidOpen - workspace ready")

      do! commands.Parse filePath content doc.Version (Some tfmConfig) |> Async.Ignore |> Async.startTaskWithCancel ctok

      // if config.Linter then do! (commands.Lint filePath |> Async.Ignore)
      if config.UnusedOpensAnalyzer then Async.Start(commands.CheckUnusedOpens filePath, ctok)

      if config.UnusedDeclarationsAnalyzer then Async.Start(commands.CheckUnusedDeclarations filePath, ctok)

      if config.SimplifyNameAnalyzer then Async.Start(commands.CheckSimplifiedNames filePath, ctok)
    }

  [<RpcMethod("textDocument/didChange")>]
  member _.TextDocumentDidChange(p: DidChangeTextDocumentParams, ctok: CancellationToken) =
    task {
      let doc = p.TextDocument
      let filePath = doc.GetFilePath() |> Utils.normalizePath
      let contentChange = p.ContentChanges |> Seq.tryLast

      logger.info (Log.setMessage "TextDocumentDidChange Request: {parms}" >> Log.addContextDestructured "parms" filePath)

      match contentChange, doc.Version with
      | Some contentChange, Some version ->
          if contentChange.Range.IsNone && contentChange.RangeLength.IsNone then
            let content = contentChange.Text.Split('\n')
            commands.SetFileContent(filePath, content, Some version, config.ScriptTFM)
          else
            ()
      | _ -> ()

      parseFileDebuncer.Bounce p
    }

  [<RpcMethod("textDocument/didSave")>]
  member _.TextDocumentDidSave(p: DidSaveTextDocumentParams) = task { return () }

  [<JsonRpcMethodAttribute("fsharp/loadAnalyzers", UseSingleObjectParameterDeserialization = true)>]
  member _.FSharpLoadAnalyzers(path: {| Project: {| Uri: string |} |}) =
    task {
      logger.info (Log.setMessage "LoadAnalyzers Request: {parms}" >> Log.addContextDestructured "parms" path)

      try
        if config.EnableAnalyzers then
          Loggers.analyzers.info (
            Log.setMessage "Using analyzer roots of {roots}" >> Log.addContextDestructured "roots" config.AnalyzersPath
          )

          config.AnalyzersPath
          |> Array.iter
               (fun analyzerPath ->
                 match rootPath with
                 | None -> ()
                 | Some workspacePath ->
                     let dir =
                       if System.IO.Path.IsPathRooted analyzerPath
                       // if analyzer is using absolute path, use it as is
                       then
                         analyzerPath
                       // otherwise, it is a relative path and should be combined with the workspace path
                       else
                         System.IO.Path.Combine(workspacePath, analyzerPath)

                     Loggers.analyzers.info (Log.setMessage "Loading analyzers from {dir}" >> Log.addContextDestructured "dir" dir)

                     let (n, m) = dir |> FSharp.Analyzers.SDK.Client.loadAnalyzers

                     Loggers.analyzers.info (
                       Log.setMessage "From {name}: {dllNo} dlls including {analyzersNo} analyzers"
                       >> Log.addContextDestructured "name" analyzerPath
                       >> Log.addContextDestructured "dllNo" n
                       >> Log.addContextDestructured "analyzersNo" m
                     ))
        // otherwise, it is a relative path and should be combined with the workspace path
        else
          Loggers.analyzers.info (Log.setMessage "Analyzers disabled")
      with ex -> Loggers.analyzers.error (Log.setMessage "Loading failed" >> Log.addExn ex)
    }

  [<RpcMethod("textDocument/hover")>]
  member x.TextDocumentHover(p: TextDocumentPositionParams, ctok: CancellationToken) : Task<Hover option> =
    logger.info (Log.setMessage "TextDocumentHover Request: {parms}" >> Log.addContextDestructured "parms" p)

    p
    |> x.positionHandler
         (fun p pos tyRes lineStr lines ->
           match commands.ToolTip tyRes pos lineStr with
           | CoreResponse.InfoRes msg
           | CoreResponse.ErrorRes msg -> async { return raise (AsyncLspResult.internalError msg) }
           | CoreResponse.Res (tip, signature, footer, typeDoc) ->
               let formatCommentStyle =
                 if config.TooltipMode = "full" then TipFormatter.FormatCommentStyle.FullEnhanced
                 else if config.TooltipMode = "summary" then TipFormatter.FormatCommentStyle.SummaryOnly
                 else TipFormatter.FormatCommentStyle.Legacy

               match TipFormatter.formatTipEnhanced tip signature footer typeDoc formatCommentStyle with
               | (sigCommentFooter :: _) :: _ ->
                   let signature, comment, footer = sigCommentFooter
                   let markStr lang (value: string) = MarkedString.WithLanguage { Language = lang; Value = value }
                   let fsharpBlock (lines: string []) = lines |> String.concat "\n" |> markStr "fsharp"

                   let sigContent =
                     let lines = signature.Split '\n' |> Array.filter (not << String.IsNullOrWhiteSpace)

                     match lines |> Array.splitAt (lines.Length - 1) with
                     | (h, [| StartsWith "Full name:" fullName |]) ->
                         [| yield fsharpBlock h; yield MarkedString.String("*" + fullName + "*") |]
                     | _ -> [| fsharpBlock lines |]


                   let commentContent = comment |> MarkedString.String

                   let footerContent =
                     footer.Split '\n'
                     |> Array.filter (not << String.IsNullOrWhiteSpace)
                     |> Array.map (fun n -> MarkedString.String("*" + n + "*"))


                   let response =
                     { Contents = MarkedStrings [| yield! sigContent; yield commentContent; yield! footerContent |]; Range = None }

                   async.Return(Some response)
               | _ -> async.Return None)
    |> Async.startTaskWithCancel ctok

  [<RpcMethod("textDocument/completion")>]
  member x.TextDocumentCompletion(p: CompletionParams, ctok) =
    let ensureInBounds (lines: LineStr array) (line, col) =
      let lineStr = lines.[line]

      if line <= lines.Length && line >= 0 && col <= lineStr.Length + 1 && col >= 0 then
        ()
      else
        logger.info (
          Log.setMessage "TextDocumentCompletion Not OK:\n COL: {col}\n LINE_STR: {lineStr}\n LINE_STR_LENGTH: {lineStrLength}"
          >> Log.addContextDestructured "col" col
          >> Log.addContextDestructured "lineStr" lineStr
          >> Log.addContextDestructured "lineStrLength" lineStr.Length
        )

        raise (AsyncLspResult.internalError "not ok")

    task {
      logger.info (Log.setMessage "TextDocumentCompletion Request: {context}" >> Log.addContextDestructured "context" p)
      // Sublime-lsp doesn't like when we answer null so we answer an empty list instead
      let file = p.TextDocument.GetFilePath() |> Utils.normalizePath
      let pos = p.GetFcsPos()

      let (options, lines) =
        commands.TryGetFileCheckerOptionsWithLines file
        |> Result.mapError AsyncLspResult.internalError
        |> Result.fold id (fun e -> raise e)

      let line, col = p.Position.Line, p.Position.Character
      let lineStr = lines.[line]
      let word = lineStr.Substring(0, col)

      do ensureInBounds lines (line, col)

      if (lineStr.StartsWith "#"
          && (KeywordList.hashDirectives.Keys |> Seq.exists (fun k -> k.StartsWith word) || word.Contains "\n")) then
        let completionList = { IsIncomplete = false; Items = KeywordList.hashSymbolCompletionItems }
        return (Some completionList)
      else
        let! typeCheckResults =
          match p.Context with
          | None ->
              commands.TryGetRecentTypeCheckResultsForFile(file, options)
              |> Option.defaultWith (fun _ -> raise (AsyncLspResult.internalError "No typecheck results"))
              |> Task.FromResult
          | Some ctx ->
              if (ctx.triggerCharacter = Some '.') then
                commands.TryGetLatestTypeCheckResultsForFile(file)
                |> Async.map (Option.defaultWith (fun _ -> raise (AsyncLspResult.internalError "No typecheck results")))
                |> Async.startTaskWithCancel ctok
              else
                commands.TryGetRecentTypeCheckResultsForFile(file, options)
                |> Option.defaultWith (fun _ -> raise (AsyncLspResult.internalError "No typecheck results"))
                |> Task.FromResult

        match! commands.Completion typeCheckResults pos lineStr lines file None (config.KeywordsAutocomplete) (config.ExternalAutocomplete)
               |> Async.startTaskWithCancel ctok with
        | CoreResponse.Res (decls, keywords) ->
            let items =
              decls
              |> Array.mapi
                   (fun id d ->
                     let code =
                       if System.Text.RegularExpressions.Regex.IsMatch(d.Name, """^[a-zA-Z][a-zA-Z0-9']+$""") then d.Name
                       elif d.NamespaceToOpen.IsSome then d.Name
                       else FSharpKeywords.QuoteIdentifierIfNeeded d.Name

                     let label =
                       match d.NamespaceToOpen with
                       | Some no -> sprintf "%s (open %s)" d.Name no
                       | None -> d.Name

                     { CompletionItem.Create(d.Name) with
                         Kind = glyphToCompletionKind d.Glyph
                         InsertText = Some code
                         SortText = Some(sprintf "%06d" id)
                         FilterText = Some d.Name })

            let its = if not keywords then items else Array.append items KeywordList.keywordCompletionItems
            let completionList = { IsIncomplete = false; Items = its }
            return Some completionList
        | _ ->
            logger.info (Log.setMessage "TextDocumentCompletion - no completion results")
            return Some { IsIncomplete = true; Items = [||] }
    }

  [<RpcMethod("completionItem/resolve")>]
  member x.CompletionItemResolve(ci: CompletionItem, ctok) =
    task {
      logger.info (Log.setMessage "CompletionItemResolve Request: {parms}" >> Log.addContextDestructured "parms" ci)

      let! res = commands.Helptext ci.InsertText.Value |> Async.startTaskWithCancel ctok

      let res =
        match res with
        | CoreResponse.InfoRes msg
        | CoreResponse.ErrorRes msg -> ci
        | CoreResponse.Res (HelpText.Simple (name, str)) ->
            let d = Documentation.Markup(markdown str)
            { ci with Detail = Some name; Documentation = Some d }
        | CoreResponse.Res (HelpText.Full (name, tip, additionalEdit)) ->
            let (si, comment) = (TipFormatter.formatTip tip) |> List.collect id |> List.head

            let edits, label =
              match additionalEdit with
              | None -> None, ci.Label
              | Some { Namespace = ns; Position = fcsPos } ->
                  Some [| { TextEdit.NewText = $"open {ns}"; TextEdit.Range = fcsPosToProtocolRange fcsPos } |], $"{ci.Label} (open {ns})"

            let d = Documentation.Markup(markdown comment)
            { ci with Detail = Some si; Documentation = Some d; AdditionalTextEdits = edits; Label = label }

      return res
    }

  [<RpcMethod("textDocument/rename")>]
  member x.TextDocumentRename(p: RenameParams, ctok) =
    logger.info (Log.setMessage "TextDocumentRename Request: {parms}" >> Log.addContextDestructured "parms" p)

    p
    |> x.positionHandler
         (fun p pos tyRes lineStr lines ->
           async {
             let! res = commands.SymbolUseProject tyRes pos lineStr

             let res =
               match res with
               | CoreResponse.InfoRes msg
               | CoreResponse.ErrorRes msg -> raise (AsyncLspResult.internalError msg)
               | CoreResponse.Res (LocationResponse.Use (_, uses)) ->
                   let documentChanges =
                     uses
                     |> Array.groupBy (fun sym -> sym.FileName)
                     |> Array.map
                          (fun (fileName, symbols) ->
                            let edits =
                              symbols
                              |> Array.map
                                   (fun sym ->
                                     let range = fcsRangeToLsp sym.RangeAlternate

                                     let range =
                                       { range with
                                           Start =
                                             { Line = range.Start.Line; Character = range.End.Character - sym.Symbol.DisplayName.Length } }

                                     { Range = range; NewText = p.NewName })
                              |> Array.distinct

                            { TextDocument =
                                { Uri = Path.FilePathToUri fileName
                                  Version = commands.TryGetFileVersion(UMX.tag fileName) // from compiler, is safe
                                }
                              Edits = edits })

                   WorkspaceEdit.Create(documentChanges, clientCapabilities.Value) |> Some
               | CoreResponse.Res (LocationResponse.UseRange uses) ->
                   let documentChanges =
                     uses
                     |> Array.groupBy (fun sym -> sym.FileName)
                     |> Array.map
                          (fun (fileName, symbols) ->
                            let edits =
                              symbols
                              |> Array.map
                                   (fun sym ->
                                     let range = symbolUseRangeToLsp sym

                                     let range =
                                       { range with
                                           Start =
                                             { Line = range.Start.Line; Character = range.End.Character - sym.SymbolDisplayName.Length } }

                                     { Range = range; NewText = p.NewName })
                              |> Array.distinct

                            { TextDocument =
                                { Uri = Path.FilePathToUri fileName
                                  Version = commands.TryGetFileVersion(UMX.tag fileName) // from compiler, is safe
                                }
                              Edits = edits })

                   WorkspaceEdit.Create(documentChanges, clientCapabilities.Value) |> Some

             return res
           })
    |> Async.startTaskWithCancel ctok

  [<RpcMethod("textDocument/signatureHelp")>]
  member x.TextDocumentSignatureHelp(sigHelpParams: SignatureHelpParams, ctok: CancellationToken) =
    logger.info (Log.setMessage "TextDocumentSignatureHelp Request: {parms}" >> Log.addContextDestructured "parms" sigHelpParams)

    sigHelpParams
    |> x.positionHandlerWithLatest
         (fun p fcsPos tyRes lineStr lines ->
           async {
             let! (methods, commas) = commands.Methods tyRes fcsPos lines |> AsyncResult.ofCoreResponse

             let sigs =
               methods.Methods
               |> Array.map
                    (fun m ->
                      let (sign, comm) = TipFormatter.formatTip m.Description |> List.head |> List.head

                      let parameters =
                        m.Parameters
                        |> Array.map
                             (fun p ->
                               { ParameterInformation.Label = p.ParameterName
                                 Documentation = Some(Documentation.String p.CanonicalTypeTextForSorting) })

                      let d = Documentation.Markup(markdown comm)
                      { SignatureInformation.Label = sign; Documentation = Some d; Parameters = Some parameters })

             let activSig =
               let sigs = sigs |> Seq.sortBy (fun n -> n.Parameters.Value.Length)

               sigs
               |> Seq.findIndex (fun s -> s.Parameters.Value.Length >= commas)
               |> fun index -> if index + 1 >= (sigs |> Seq.length) then index else index + 1

             let res = { Signatures = sigs; ActiveSignature = Some activSig; ActiveParameter = Some commas }

             return Some res
           })
    |> Async.startTaskWithCancel ctok

  [<RpcMethod("textDocument/definition")>]
  member x.TextDocumentDefinition(p: TextDocumentPositionParams, ctok: CancellationToken) =
    logger.info (Log.setMessage "TextDocumentDefinition Request: {parms}" >> Log.addContextDestructured "parms" p)

    p
    |> x.positionHandler
         (fun p pos tyRes lineStr lines ->
           async {
             let! res = commands.FindDeclaration tyRes pos lineStr |> AsyncResult.ofCoreResponse
             return findDeclToLspLocation res |> GotoResult.Single |> Some
           })
    |> Async.startTaskWithCancel ctok

  [<RpcMethod("textDocument/typeDefinition")>]
  member x.TextDocumentTypeDefinition(p: TextDocumentPositionParams, ctok: CancellationToken) =
    logger.info (Log.setMessage "TextDocumentTypeDefinition Request: {parms}" >> Log.addContextDestructured "parms" p)

    p
    |> x.positionHandler
         (fun p pos tyRes lineStr lines ->
           async {
             let! res = commands.FindTypeDeclaration tyRes pos lineStr |> AsyncResult.ofCoreResponse
             return findDeclToLspLocation res |> GotoResult.Single |> Some
           })
    |> Async.startTaskWithCancel ctok

  [<RpcMethod("textDocument/references")>]
  member x.TextDocumentReferences(p: ReferenceParams, ctok: CancellationToken) =
    logger.info (Log.setMessage "TextDocumentReferences Request: {parms}" >> Log.addContextDestructured "parms" p)

    p
    |> x.positionHandler
         (fun p pos tyRes lineStr lines ->
           async {
             let! res = commands.SymbolUseProject tyRes pos lineStr |> AsyncResult.ofCoreResponse

             let references =
               match res with
               | LocationResponse.Use (_, uses) -> uses |> Array.map (fun n -> fcsRangeToLspLocation n.RangeAlternate)

               | LocationResponse.UseRange uses -> uses |> Array.map symbolUseRangeToLspLocation

             return Some references
           })
    |> Async.startTaskWithCancel ctok

  [<RpcMethod("textDocument/documentHighlight")>]
  member x.TextDocumentDocumentHighlight(p: TextDocumentPositionParams, ctok: CancellationToken) =
    logger.info (Log.setMessage "TextDocumentDocumentHighlight Request: {parms}" >> Log.addContextDestructured "parms" p)

    p
    |> x.positionHandler
         (fun p pos tyRes lineStr lines ->
           let (symbol, uses) = commands.SymbolUse tyRes pos lineStr |> CoreResponse.toRpcError

           let result = uses |> Array.map (fun s -> { DocumentHighlight.Range = fcsRangeToLsp s.RangeAlternate; Kind = None }) |> Some

           async.Return result)
    |> Async.startTaskWithCancel ctok

  [<RpcMethod("textDocument/implementation")>]
  member x.TextDocumentImplementation(p: TextDocumentPositionParams, ctok: CancellationToken) =
    logger.info (Log.setMessage "TextDocumentImplementation Request: {parms}" >> Log.addContextDestructured "parms" p)

    p
    |> x.positionHandler
         (fun p pos tyRes lineStr lines ->
           async {
             let! res = commands.SymbolImplementationProject tyRes pos lineStr |> AsyncResult.ofCoreResponse

             match res with
             | LocationResponse.Use (symbol, uses) ->
                 return uses |> Array.map (fun n -> fcsRangeToLspLocation n.RangeAlternate) |> GotoResult.Multiple |> Some
             | LocationResponse.UseRange uses -> return uses |> Array.map symbolUseRangeToLspLocation |> GotoResult.Multiple |> Some
           })
    |> Async.startTaskWithCancel ctok

  [<RpcMethod("textDocument/documentSymbol")>]
  member x.TextDocumentDocumentSymbol(p: DocumentSymbolParams, ctok: CancellationToken) =
    task {
      logger.info (Log.setMessage "TextDocumentDocumentSymbol Request: {parms}" >> Log.addContextDestructured "parms" p)
      let fn = p.TextDocument.GetFilePath() |> Utils.normalizePath

      let! decls =
        commands.Declarations fn None (commands.TryGetFileVersion fn)
        |> AsyncResult.ofCoreResponse
        |> Async.startTaskWithCancel ctok

      let symbols =
        decls
        |> Array.collect (fst >> fun top -> getSymbolInformations p.TextDocument.Uri glyphToSymbolKind top (fun s -> true))

      return Some symbols
    }

  [<RpcMethod("workspace/symbol")>]
  member x.WorkspaceSymbol(symbolRequest: WorkspaceSymbolParams, ctok: CancellationToken) =
    task {
      logger.info (Log.setMessage "WorkspaceSymbol Request: {parms}" >> Log.addContextDestructured "parms" symbolRequest)

      let! decls = commands.DeclarationsInProjects() |> AsyncResult.ofCoreResponse |> Async.startTaskWithCancel ctok

      let symbols =
        decls
        |> Array.collect
             (fun (n, p) ->
               let uri = Path.LocalPathToUri p
               getSymbolInformations uri glyphToSymbolKind n (applyQuery symbolRequest.Query))

      return Some symbols
    }

  [<RpcMethod("textDocument/formatting")>]
  member x.TextDocumentFormatting(p: DocumentFormattingParams, ctok: CancellationToken) =
    task {
      logger.info (Log.setMessage "TextDocumentFormatting Request: {parms}" >> Log.addContextDestructured "parms" p)
      let doc = p.TextDocument
      let fileName = doc.GetFilePath() |> Utils.normalizePath
      let! res = commands.FormatDocument fileName |> Async.startTaskWithCancel ctok

      match res with
      | Some (lines, formatted) ->
          let range =
            let zero = { Line = 0; Character = 0 }
            let endLine = Array.length lines - 1
            let endCharacter = Array.tryLast lines |> Option.map (fun line -> line.Length) |> Option.defaultValue 0
            { Start = zero; End = { Line = endLine; Character = endCharacter } }

          return Some([| { Range = range; NewText = formatted } |])
      | None -> return None
    }

  [<RpcMethod("textDocument/codeAction")>]
  member x.TextDocumentCodeAction(codeActionParams: CodeActionParams, ctok: CancellationToken) =
    task {
      logger.info (Log.setMessage "TextDocumentCodeAction Request: {parms}" >> Log.addContextDestructured "parms" codeActionParams)

      let fn = codeActionParams.TextDocument.GetFilePath() |> Utils.normalizePath

      match commands.TryGetFileCheckerOptionsWithLines fn with
      | Error s -> return None // no options, no checks
      | Ok _ ->

          let! actions =
            Async.Parallel(codeFixes codeActionParams) |> Async.map (List.concat >> Array.ofList) |> Async.startTaskWithCancel ctok

          return actions |> TextDocumentCodeActionResult.CodeActions |> Some
    }

  [<RpcMethod("textDocument/codeLens")>]
  member __.TextDocumentCodeLens(p: CodeLensParams, ctok: CancellationToken) =
    task {
      logger.info (Log.setMessage "TextDocumentCodeLens Request: {parms}" >> Log.addContextDestructured "parms" p)

      let fn = p.TextDocument.GetFilePath() |> Utils.normalizePath

      let! decls =
        commands.Declarations fn None (commands.TryGetFileVersion fn)
        |> AsyncResult.ofCoreResponse
        |> Async.startTaskWithCancel ctok

      if config.LineLens.Enabled <> "replaceCodeLens" then
        let sigs = decls |> Array.collect (fst >> getCodeLensInformation p.TextDocument.Uri "signature")

        let references =
          if config.EnableReferenceCodeLens then
            decls |> Array.collect (fst >> getCodeLensInformation p.TextDocument.Uri "reference")
          else
            [||]

        return Some [| yield! references; yield! sigs |]
      else
        return None
    }

  [<RpcMethod("codeLens/resolve")>]
  member x.CodeLensResolve(p: CodeLens, ctok: CancellationToken) =
    logger.info (Log.setMessage "CodeLensResolve Request: {parms}" >> Log.addContextDestructured "parms" p)

    let handler f (arg: CodeLens) =
      async {
        let pos = FcsPos.mkPos (arg.Range.Start.Line + 1) (arg.Range.Start.Character + 2)
        let data = arg.Data.Value.ToObject<string []>()
        let file = Path.FileUriToLocalPath data.[0] |> Utils.normalizePath

        return!
          match commands.TryGetFileCheckerOptionsWithLinesAndLineStr(file, pos) with
          | ResultOrString.Error s ->
              logger.error (
                Log.setMessage "CodeLensResolve - Getting file checker options failed for {file}"
                >> Log.addContextDestructured "file" file
                >> Log.addContextDestructured "error" s
              )

              let cmd = { Title = "No options"; Command = None; Arguments = None }
              { p with Command = Some cmd } |> async.Return
          | ResultOrString.Ok (options, _, lineStr) ->
              try
                async {
                  let! tyResOpt = commands.TryGetLatestTypeCheckResultsForFile(file)

                  return!
                    match tyResOpt with
                    | None ->
                        logger.warn (
                          Log.setMessage "CodeLensResolve - Cached typecheck results not yet available for {file}"
                          >> Log.addContextDestructured "file" file
                        )

                        let cmd = { Title = "No typecheck results"; Command = None; Arguments = None }
                        { p with Command = Some cmd } |> async.Return
                    | Some tyRes ->
                        async {
                          let! r = Async.Catch(f arg pos tyRes lineStr data.[1] file)

                          match r with
                          | Choice1Of2 r -> return r
                          | Choice2Of2 e ->
                              logger.error (
                                Log.setMessage "CodeLensResolve - Child operation failed for {file}"
                                >> Log.addContextDestructured "file" file
                                >> Log.addExn e
                              )

                              let cmd = { Title = ""; Command = None; Arguments = None }
                              return { p with Command = Some cmd }
                        }
                }
              with e ->
                logger.error (
                  Log.setMessage "CodeLensResolve - Operation failed on {file}" >> Log.addContextDestructured "file" file >> Log.addExn e
                )

                let cmd = { Title = ""; Command = None; Arguments = None }
                { p with Command = Some cmd } |> async.Return
      }


    handler
      (fun p pos tyRes lineStr typ file ->
        async {
          if typ = "signature" then
            match commands.SignatureData tyRes pos lineStr with
            | CoreResponse.InfoRes msg
            | CoreResponse.ErrorRes msg ->
                logger.error (
                  Log.setMessage "CodeLensResolve - error on file {file}"
                  >> Log.addContextDestructured "file" file
                  >> Log.addContextDestructured "error" msg
                )

                let cmd = { Title = ""; Command = None; Arguments = None }
                return { p with Command = Some cmd }
            | CoreResponse.Res (typ, parms, _) ->
                let formatted = SigantureData.formatSignature typ parms
                let cmd = { Title = formatted; Command = None; Arguments = None }
                return { p with Command = Some cmd }
          else
            let! res = commands.SymbolUseProject tyRes pos lineStr

            let res =
              match res with
              | CoreResponse.InfoRes msg
              | CoreResponse.ErrorRes msg ->
                  logger.error (
                    Log.setMessage "CodeLensResolve - error getting symbol use for {file}"
                    >> Log.addContextDestructured "file" file
                    >> Log.addContextDestructured "error" msg
                  )

                  let cmd = { Title = ""; Command = None; Arguments = None }
                  { p with Command = Some cmd }
              | CoreResponse.Res (LocationResponse.Use (sym, uses)) ->
                  let formatted = if uses.Length = 1 then "1 Reference" else sprintf "%d References" uses.Length
                  let locs = uses |> Array.map (fun n -> fcsRangeToLspLocation n.RangeAlternate)

                  let args = [| JToken.FromObject(Path.LocalPathToUri file); JToken.FromObject(fcsPosToLsp pos); JToken.FromObject locs |]

                  let cmd = { Title = formatted; Command = Some "fsharp.showReferences"; Arguments = Some args }
                  { p with Command = Some cmd }
              | CoreResponse.Res (LocationResponse.UseRange (uses)) ->
                  let formatted =
                    if uses.Length - 1 = 1 then "1 Reference"
                    elif uses.Length = 0 then "0 References"
                    else sprintf "%d References" (uses.Length - 1)

                  let locs = uses |> Array.map symbolUseRangeToLspLocation

                  let args = [| JToken.FromObject(Path.LocalPathToUri file); JToken.FromObject(fcsPosToLsp pos); JToken.FromObject locs |]

                  let cmd = { Title = formatted; Command = Some "fsharp.showReferences"; Arguments = Some args }
                  { p with Command = Some cmd }

            return res
        })
      p
    |> Async.startTaskWithCancel ctok

  [<RpcMethod("workspace/didChangeWatchedFiles")>]
  member x.WorkspaceDidChangeWatchedFiles(p: DidChangeWatchedFilesParams, ctok: CancellationToken) =
    task {
      logger.info (Log.setMessage "WorkspaceDidChangeWatchedFiles Request: {parms}" >> Log.addContextDestructured "parms" p)

      p.Changes
      |> Array.iter
           (fun c ->
             if c.Type = FileChangeType.Deleted then
               let uri = c.Uri
               diagnosticCollections.AddOrUpdate((uri, "F# Compiler"), [||], (fun _ _ -> [||])) |> ignore
               diagnosticCollections.AddOrUpdate((uri, "F# Unused opens"), [||], (fun _ _ -> [||])) |> ignore
               diagnosticCollections.AddOrUpdate((uri, "F# Unused declarations"), [||], (fun _ _ -> [||])) |> ignore
               diagnosticCollections.AddOrUpdate((uri, "F# simplify names"), [||], (fun _ _ -> [||])) |> ignore
               diagnosticCollections.AddOrUpdate((uri, "F# Linter"), [||], (fun _ _ -> [||])) |> ignore
               sendDiagnostics uri

             ())

      return ()
    }

  [<RpcMethod("workspace/didChangeConfiguration")>]
  member __.WorkspaceDidChangeConfiguration(p: DidChangeConfigurationParams, ctok: CancellationToken) =
    task {
      let dto = p.Settings |> LanguageServerProtocol.Server.deserialize<FSharpConfigRequest>
      logger.info (Log.setMessage "WorkspaceDidChangeConfiguration Request: {parms}" >> Log.addContextDestructured "parms" dto)

      let c = config.AddDto dto.FSharp
      updateConfig c
      logger.info (Log.setMessage "Workspace configuration changed" >> Log.addContextDestructured "config" c)
      return ()
    }

  [<RpcMethod("textDocument/foldingRange")>]
  member x.TextDocumentFoldingRange(rangeP: FoldingRangeParams, ctok: CancellationToken) =
    task {
      logger.info (Log.setMessage "TextDocumentFoldingRange Request: {parms}" >> Log.addContextDestructured "parms" rangeP)
      let file = rangeP.TextDocument.GetFilePath() |> Utils.normalizePath

      match! commands.ScopesForFile file |> Async.startTaskWithCancel ctok with
      | Ok scopes ->
          let ranges = scopes |> Seq.map Structure.toFoldingRange |> Set.ofSeq |> List.ofSeq
          return Some ranges
      | Result.Error error -> return raise (AsyncLspResult.internalError error)
    }

  [<RpcMethod("textDocument/selectionRange")>]
  member __.TextDocumentSelectionRange(selectionRangeP: SelectionRangeParams, ctok: CancellationToken) =
    task {
      logger.info (Log.setMessage "TextDocumentSelectionRange Request: {parms}" >> Log.addContextDestructured "parms" selectionRangeP)

      let rec mkSelectionRanges =
        function
        | [] -> None
        | r :: xs -> Some { Range = fcsRangeToLsp r; Parent = mkSelectionRanges xs }

      let file = selectionRangeP.TextDocument.GetFilePath() |> Utils.normalizePath
      let poss = selectionRangeP.Positions |> Array.map protocolPosToPos |> Array.toList
      let! ranges = commands.GetRangesAtPosition file poss |> AsyncResult.ofCoreResponse |> Async.startTaskWithCancel ctok
      let response = ranges |> List.choose mkSelectionRanges
      return Some response
    }

  [<RpcMethod("fsharp/signature")>]
  member x.FSharpSignature(p: TextDocumentPositionParams, ctok) =
    logger.info (Log.setMessage "FSharpSignature Request: {parms}" >> Log.addContextDestructured "parms" p)

    p
    |> x.positionHandler
         (fun p pos tyRes lineStr lines ->
           let tip = commands.Typesig tyRes pos lineStr |> CoreResponse.toRpcError
           { Content = CommandResponse.typeSig FsAutoComplete.JsonSerializer.writeJson tip } |> async.Return)
    |> Async.startTaskWithCancel ctok

  [<RpcMethod("fsharp/signatureData")>]
  member x.FSharpSignatureData(p: TextDocumentPositionParams, ctok) =
    logger.info (Log.setMessage "FSharpSignatureData Request: {parms}" >> Log.addContextDestructured "parms" p)

    let handler f (arg: TextDocumentPositionParams) =
      async {
        let pos = FcsPos.mkPos (p.Position.Line) (p.Position.Character + 2)
        let file = p.TextDocument.GetFilePath() |> Utils.normalizePath

        logger.info (
          Log.setMessage "FSharpSignatureData - Position request for {file} at {pos}"
          >> Log.addContextDestructured "file" file
          >> Log.addContextDestructured "pos" pos
        )

        return!
          match commands.TryGetFileCheckerOptionsWithLinesAndLineStr(file, pos) with
          | ResultOrString.Error s -> raise (AsyncLspResult.internalError "No options")
          | ResultOrString.Ok (options, _, lineStr) ->
              try
                async {
                  let! tyResOpt = commands.TryGetLatestTypeCheckResultsForFile(file)

                  return!
                    match tyResOpt with
                    | None -> raise (AsyncLspResult.internalError "No typecheck results")
                    | Some tyRes ->
                        async {
                          let! r = Async.Catch(f arg pos tyRes lineStr)

                          match r with
                          | Choice1Of2 r -> return r
                          | Choice2Of2 e -> return raise (AsyncLspResult.internalError e.Message)
                        }
                }
              with e -> raise (AsyncLspResult.internalError e.Message)
      }

    p
    |> handler
         (fun p pos tyRes lineStr ->
           let (typ, parms, generics) = commands.SignatureData tyRes pos lineStr |> CoreResponse.toRpcError

           { Content = CommandResponse.signatureData FsAutoComplete.JsonSerializer.writeJson (typ, parms, generics) }
           |> async.Return)
    |> Async.startTaskWithCancel ctok

  [<RpcMethod("fsharp/documentationGenerator")>]
  member x.FSharpDocumentationGenerator(p: TextDocumentPositionParams, ctok) =
    p
    |> x.positionHandler
         (fun p pos tyRes lineStr lines ->
           let (typ, parms, generics) = commands.SignatureData tyRes pos lineStr |> CoreResponse.toRpcError

           { Content = CommandResponse.signatureData FsAutoComplete.JsonSerializer.writeJson (typ, parms, generics) }
           |> async.Return)
    |> Async.startTaskWithCancel ctok

  [<RpcMethod("fsharp/lineLens")>]
  member __.FSharpLineLens(p: ProjectParms, ctok) =
    task {
      let fn = p.Project.GetFilePath() |> Utils.normalizePath

      let! decls =
        commands.Declarations fn None (commands.TryGetFileVersion fn)
        |> AsyncResult.ofCoreResponse
        |> Async.startTaskWithCancel ctok

      return { Content = CommandResponse.declarations FsAutoComplete.JsonSerializer.writeJson decls }
    }

  [<RpcMethod("lineLens/resolve")>]
  member x.LineLensResolve(p: TextDocumentPositionParams, ctok) =
    p
    |> x.positionHandler
         (fun p pos tyRes lineStr lines ->
           let (typ, parms, generics) = commands.SignatureData tyRes pos lineStr |> CoreResponse.toRpcError

           { Content = CommandResponse.signatureData FsAutoComplete.JsonSerializer.writeJson (typ, parms, generics) }
           |> async.Return)
    |> Async.startTaskWithCancel ctok

  [<RpcMethod("fsharp/compilerLocation")>]
  member x.FSharpCompilerLocation() =
    let (fsc, fsi, msbuild, sdk) = commands.CompilerLocation() |> CoreResponse.toRpcError

    { Content =
        CommandResponse.compilerLocation
          FsAutoComplete.JsonSerializer.writeJson
          fsc
          fsi
          msbuild
          (sdk |> Option.map (fun (di: DirectoryInfo) -> di.FullName)) }

  [<RpcMethod("fsharp/project")>]
  member x.FSharpProject(p: ProjectParms, ctok) =
    task {
      let fn = p.Project.GetFilePath()
      let! fin = commands.Project fn config.GenerateBinlog |> AsyncResult.ofCoreResponse |> Async.startTaskWithCancel ctok
      return { Content = CommandResponse.projectLoad FsAutoComplete.JsonSerializer.writeJson fin }
    }

  [<RpcMethod("fsharp/dotnetNewList")>]
  member x.FSharpDotnetNewList(p: DotnetNewListRequest, ctok) =
    task {
      let! funcs = commands.DotnetNewList() |> AsyncResult.ofCoreResponse |> Async.startTaskWithCancel ctok
      return { Content = CommandResponse.dotnetnewlist FsAutoComplete.JsonSerializer.writeJson funcs }
    }

  [<RpcMethod("fsharp/dotnetNewRun")>]
  member x.FSharpDotnetNewRun(p: DotnetNewRunRequest, ctok) =
    task {
      let! _ = commands.DotnetNewRun p.Template p.Name p.Output [] |> AsyncResult.ofCoreResponse |> Async.startTaskWithCancel ctok
      return { Content = "" }
    }

  [<RpcMethod("fsharp/dotnetAddProject")>]
  member x.FSharpDotnetAddProject(p: DotnetProjectRequest, ctok) =
    task {
      let! _ = commands.DotnetAddProject p.Target p.Reference |> AsyncResult.ofCoreResponse |> Async.startTaskWithCancel ctok
      return { Content = "" }
    }

  [<RpcMethod("fsharp/dotnetRemoveProject")>]
  member x.FSharpDotnetRemoveProject(p: DotnetProjectRequest, ctok) =
    task {
      let! _ = commands.DotnetRemoveProject p.Target p.Reference |> AsyncResult.ofCoreResponse |> Async.startTaskWithCancel ctok
      return { Content = "" }
    }

  [<RpcMethod("fsharp/dotnetSlnAdd")>]
  member x.FSharpDotnetSlnAdd(p: DotnetProjectRequest, ctok) =
    task {
      let! _ = commands.DotnetSlnAdd p.Target p.Reference |> AsyncResult.ofCoreResponse |> Async.startTaskWithCancel ctok
      return { Content = "" }
    }

  [<RpcMethod("fsproj/moveFileUp")>]
  member x.FsprojMoveFileUp(p: DotnetFileRequest, ctok) =
    task {
      let! _ = commands.FsProjMoveFileUp p.FsProj p.FileVirtualPath |> AsyncResult.ofCoreResponse |> Async.startTaskWithCancel ctok
      return { Content = "" }
    }

  [<RpcMethod("fsproj/moveFileDown")>]
  member x.FsprojMoveFileDown(p: DotnetFileRequest, ctok) =
    task {
      let! _ = commands.FsProjMoveFileDown p.FsProj p.FileVirtualPath |> AsyncResult.ofCoreResponse |> Async.startTaskWithCancel ctok
      return { Content = "" }
    }

  [<RpcMethod("fsproj/addFileAbove")>]
  member x.FsprojAddFileAbove(p: DotnetFile2Request, ctok) =
    task {
      let! _ =
        commands.FsProjAddFileAbove p.FsProj p.FileVirtualPath p.NewFile
        |> AsyncResult.ofCoreResponse
        |> Async.startTaskWithCancel ctok

      return { Content = "" }
    }

  [<RpcMethod("fsproj/addFileBelow")>]
  member x.FsprojAddFileBelow(p: DotnetFile2Request, ctok) =
    task {
      let! _ =
        commands.FsProjAddFileBelow p.FsProj p.FileVirtualPath p.NewFile
        |> AsyncResult.ofCoreResponse
        |> Async.startTaskWithCancel ctok

      return { Content = "" }
    }

  [<RpcMethod("fsproj/addFile")>]
  member x.FsprojAddFile(p: DotnetFileRequest, ctok) =
    task {
      let! _ = commands.FsProjAddFile p.FsProj p.FileVirtualPath |> AsyncResult.ofCoreResponse |> Async.startTaskWithCancel ctok
      return { Content = "" }
    }

  [<RpcMethod("fsharp/help")>]
  member x.FSharpHelp(p: TextDocumentPositionParams, ctok) =
    p
    |> x.positionHandler
         (fun p pos tyRes lineStr lines ->
           let t = commands.Help tyRes pos lineStr |> CoreResponse.toRpcError
           { Content = CommandResponse.help FsAutoComplete.JsonSerializer.writeJson t } |> async.Return)
    |> Async.startTaskWithCancel ctok

  [<RpcMethod("fsharp/documentation")>]
  member x.FSharpDocumentation(p: TextDocumentPositionParams, ctok) =
    p
    |> x.positionHandler
         (fun p pos tyRes lineStr lines ->
           let (tip, xml, signature, footer, cm) = commands.FormattedDocumentation tyRes pos lineStr |> CoreResponse.toRpcError

           { Content = CommandResponse.formattedDocumentation FsAutoComplete.JsonSerializer.writeJson (tip, xml, signature, footer, cm) }
           |> async.Return)
    |> Async.startTaskWithCancel ctok

  [<RpcMethod("fsharp/documentationSymbol")>]
  member x.FSharpDocumentationSymbol(p: DocumentationForSymbolRequest, ctok) =
    match commands.LastCheckResult with
    | None -> raise (AsyncLspResult.internalError "error")
    | Some tyRes ->
        let (xml, assembly, doc, signature, footer, cn) =
          commands.FormattedDocumentationForSymbol tyRes p.XmlSig p.Assembly |> Result.ofCoreResponse

        { Content =
            CommandResponse.formattedDocumentationForSymbol FsAutoComplete.JsonSerializer.writeJson xml assembly doc (signature, footer, cn) }
        |> async.Return

  [<RpcMethod("fsharp/loadAnalyzers")>]
  member x.FSharpLoadAnalyzers(ctok) =
    task {
      try
        if config.EnableAnalyzers then
          Loggers.analyzers.info (
            Log.setMessage "Using analyzer roots of {roots}" >> Log.addContextDestructured "roots" config.AnalyzersPath
          )

          config.AnalyzersPath
          |> Array.iter
               (fun analyzerPath ->
                 match rootPath with
                 | None -> ()
                 | Some workspacePath ->
                     let dir =
                       if System.IO.Path.IsPathRooted analyzerPath
                       // if analyzer is using absolute path, use it as is
                       then
                         analyzerPath
                       // otherwise, it is a relative path and should be combined with the workspace path
                       else
                         System.IO.Path.Combine(workspacePath, analyzerPath)

                     Loggers.analyzers.info (Log.setMessage "Loading analyzers from {dir}" >> Log.addContextDestructured "dir" dir)
                     let (n, m) = dir |> FSharp.Analyzers.SDK.Client.loadAnalyzers

                     Loggers.analyzers.info (
                       Log.setMessage "From {name}: {dllNo} dlls including {analyzersNo} analyzers"
                       >> Log.addContextDestructured "name" analyzerPath
                       >> Log.addContextDestructured "dllNo" n
                       >> Log.addContextDestructured "analyzersNo" m
                     ))
        // otherwise, it is a relative path and should be combined with the workspace path
        else
          Loggers.analyzers.info (Log.setMessage "Analyzers disabled")

        return ()
      with ex ->
        Loggers.analyzers.error (Log.setMessage "Loading failed" >> Log.addExn ex)
        return ()
    }

  member private x.handleSemanticTokens
    (getTokens: Async<option<(struct (FcsRange * SemanticClassificationType)) array>>)
    : Async<SemanticTokens option> =
    async {
      match! getTokens with
      | None -> return raise (AsyncLspResult.internalError "No highlights found")
      | Some rangesAndHighlights ->
          let lspTypedRanges =
            rangesAndHighlights
            |> Array.map
                 (fun (struct (fcsRange, fcsTokenType)) ->
                   let ty, mods = ClassificationUtils.map fcsTokenType
                   struct (fcsRangeToLsp fcsRange, ty, mods))

          match encodeSemanticHighlightRanges lspTypedRanges with
          | None -> return None
          | Some encoded -> return Some { Data = encoded; ResultId = None } // TODO: provide a resultId when we support delta ranges
    }

  [<RpcMethod("textDocument/semanticTokens/full")>]
  member x.TextDocumentSemanticTokensFull(p: SemanticTokensParams, ctok) : Task<SemanticTokens option> =
    logger.info (Log.setMessage "Semantic highlighing request: {parms}" >> Log.addContextDestructured "parms" p)
    let fn = p.TextDocument.GetFilePath() |> Utils.normalizePath
    let getTokens = commands.GetHighlighting(fn, None) |> AsyncResult.ofCoreResponse
    x.handleSemanticTokens (getTokens) |> Async.startTaskWithCancel ctok

  [<RpcMethod("textDocument/semanticTokens/range")>]
  member x.TextDocumentSemanticTokensRange(p: SemanticTokensRangeParams, ctok) : Task<SemanticTokens option> =
    logger.info (Log.setMessage "Semantic highlighing range request: {parms}" >> Log.addContextDestructured "parms" p)
    let fn = p.TextDocument.GetFilePath() |> Utils.normalizePath
    let fcsRange = protocolRangeToRange (UMX.untag fn) p.Range
    let getTokens = commands.GetHighlighting(fn, Some fcsRange) |> AsyncResult.ofCoreResponse
    x.handleSemanticTokens (getTokens) |> Async.startTaskWithCancel ctok

  [<RpcMethod("fsharp/fsharpLiterate")>]
  member x.FSharpFSharpLiterate(p: FSharpLiterateRequest, ctok) =
    task {
      logger.info (Log.setMessage "FSharpLiterate Request: {parms}" >> Log.addContextDestructured "parms" p)
      let fn = p.FileName |> Utils.normalizePath
      let! res = commands.FSharpLiterate fn |> AsyncResult.ofCoreResponse |> Async.startTaskWithCancel ctok
      return { Content = CommandResponse.fsharpLiterate FsAutoComplete.JsonSerializer.writeJson res }
    }

  [<RpcMethod("fsharp/pipelineHints")>]
  member x.FSharpPipelineHints(p: FSharpPipelineHintRequest, ctok) =
    logger.info (Log.setMessage "FSharpPipelineHints Request: {parms}" >> Log.addContextDestructured "parms" p)
    let fn = p.FileName |> Utils.normalizePath

    fn
    |> x.fileHandler
         (fun fn tyRes lines ->
           let res = commands.PipelineHints tyRes |> CoreResponse.toRpcError
           { Content = CommandResponse.pipelineHint FsAutoComplete.JsonSerializer.writeJson res } |> async.Return)
    |> Async.startTaskWithCancel ctok
