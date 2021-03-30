module FsAutoComplete.Lsp

open FsAutoComplete
open FsAutoComplete.CodeFix
open FsAutoComplete.CodeFix.Types
open FsAutoComplete.Logging
open FSharp.Compiler.SourceCodeServices
open LanguageServerProtocol
open LanguageServerProtocol.LspResult
open LanguageServerProtocol.Server
open LanguageServerProtocol.Types
open LspHelpers
open Newtonsoft.Json.Linq
open Ionide.ProjInfo.ProjectSystem
open System
open System.IO
open FsToolkit.ErrorHandling
open FSharp.UMX

module FcsRange = FSharp.Compiler.Text.Range
type FcsRange = FSharp.Compiler.Text.Range
module FcsPos = FSharp.Compiler.Text.Pos
type FcsPos = FSharp.Compiler.Text.Pos

module AsyncResult =
  let ofCoreResponse (ar: Async<CoreResponse<'a>>) =
    ar |> Async.map (function | CoreResponse.Res a -> Ok a | CoreResponse.ErrorRes msg | CoreResponse.InfoRes msg -> Error (JsonRpc.Error.InternalErrorMessage msg))


open FSharp.Analyzers

type FSharpLspClient(sendServerNotification: ClientNotificationSender, sendServerRequest: ClientRequestSender) =
    inherit LspClient ()

    override __.WindowShowMessage(p) =
        sendServerNotification "window/showMessage" (box p) |> Async.Ignore

    override __.WindowShowMessageRequest(p) =
        sendServerRequest.Send "window/showMessageRequest" (box p)

    override __.WindowLogMessage(p) =
        sendServerNotification "window/logMessage" (box p) |> Async.Ignore

    override __.TelemetryEvent(p) =
        sendServerNotification "telemetry/event" (box p) |> Async.Ignore

    override __.ClientRegisterCapability(p) =
        sendServerRequest.Send "client/registerCapability" (box p)

    override __.ClientUnregisterCapability(p) =
        sendServerRequest.Send "client/unregisterCapability" (box p)

    override __.WorkspaceWorkspaceFolders () =
        sendServerRequest.Send "workspace/workspaceFolders" ()

    override __.WorkspaceConfiguration (p) =
        sendServerRequest.Send "workspace/configuration" (box p)

    override __.WorkspaceApplyEdit (p) =
        sendServerRequest.Send "workspace/applyEdit" (box p)

    override __.WorkspaceSemanticTokensRefresh () =
      sendServerNotification "workspace/semanticTokens/refresh" () |> Async.Ignore

    override __.TextDocumentPublishDiagnostics(p) =
        sendServerNotification "textDocument/publishDiagnostics" (box p) |> Async.Ignore

    ///Custom notification for workspace/solution/project loading events
    member __.NotifyWorkspace (p: PlainNotification) =
        sendServerNotification "fsharp/notifyWorkspace" (box p) |> Async.Ignore

    ///Custom notification for initial workspace peek
    member __.NotifyWorkspacePeek (p: PlainNotification) =
        sendServerNotification "fsharp/notifyWorkspacePeek" (box p) |> Async.Ignore

    member __.NotifyCancelledRequest (p: PlainNotification) =
        sendServerNotification "fsharp/notifyCancel" (box p) |> Async.Ignore

    member __.NotifyFileParsed (p: PlainNotification) =
        sendServerNotification "fsharp/fileParsed" (box p) |> Async.Ignore

type FSharpLspServer(backgroundServiceEnabled: bool, state: State, lspClient: FSharpLspClient) =
    inherit LspServer()

    let logger = LogProvider.getLoggerByName "LSP"
    let fantomasLogger = LogProvider.getLoggerByName "Fantomas"
    let backgroundService: BackgroundServices.BackgroundService = if backgroundServiceEnabled then BackgroundServices.ActualBackgroundService() :> _ else BackgroundServices.MockBackgroundService() :> _
    let mutable commands = new Commands(FSharpCompilerServiceChecker(backgroundServiceEnabled, false), state, backgroundService, false)
    let mutable commandDisposables = ResizeArray()

    let mutable clientCapabilities: ClientCapabilities option = None
    let mutable glyphToCompletionKind = glyphToCompletionKindGenerator None
    let mutable glyphToSymbolKind = glyphToSymbolKindGenerator None

    let mutable config = FSharpConfig.Default
    let mutable rootPath : string option = None
    let mutable codeFixes = fun p -> [||]

    //TODO: Thread safe version
    let lintFixes = System.Collections.Generic.Dictionary<DocumentUri, (LanguageServerProtocol.Types.Range * TextEdit) list>()
    let analyzerFixes = System.Collections.Generic.Dictionary<DocumentUri, System.Collections.Generic.Dictionary<string, (LanguageServerProtocol.Types.Range * TextEdit) list>>()


    let parseFile (p: DidChangeTextDocumentParams) =
      async {
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
                if config.UnusedOpensAnalyzer then  Async.Start (commands.CheckUnusedOpens filePath)
                if config.UnusedDeclarationsAnalyzer then Async.Start (commands.CheckUnusedDeclarations filePath) //fire and forget this analyzer now that it's syncronous
                if config.SimplifyNameAnalyzer then Async.Start (commands.CheckSimplifiedNames filePath)
            else
                logger.warn (Log.setMessage "ParseFile - Parse not started, received partial change")
        | _ ->
            logger.info (Log.setMessage "ParseFile - Found no change for {file}" >> Log.addContextDestructured "file" filePath)
      } |> Async.Start

    let parseFileDebuncer = Debounce(500, parseFile)

    let diagnosticCollections = System.Collections.Concurrent.ConcurrentDictionary<DocumentUri * string,Diagnostic[]>()

    let sendDiagnostics (uri: DocumentUri) =
        let diags =
            diagnosticCollections
            |> Seq.collect (fun kv ->
                let (u, _) = kv.Key
                if u = uri then kv.Value else [||])
            |> Seq.sortBy (fun n ->
                n.Range.Start.Line
            )
            |> Seq.toArray
        logger.info (Log.setMessage "SendDiag for {file}: {diags} entries" >> Log.addContextDestructured "file" uri >> Log.addContextDestructured "diags" diags.Length )
        {Uri = uri; Diagnostics = diags}
        |> lspClient.TextDocumentPublishDiagnostics
        |> Async.Start

    let handleCommandEvents (n: NotificationEvent) =
      try
          match n with
          | NotificationEvent.FileParsed fn ->
              {Content = UMX.untag fn }
              |> lspClient.NotifyFileParsed
              |> Async.Start
          | NotificationEvent.Workspace ws ->
              logger.info (Log.setMessage "Workspace Notify {ws}" >> Log.addContextDestructured "ws" ws)
              let ws =
                  match ws with
                  | ProjectResponse.Project (x, _) -> CommandResponse.project JsonSerializer.writeJson x
                  | ProjectResponse.ProjectError(_,errorDetails) -> CommandResponse.projectError JsonSerializer.writeJson errorDetails
                  | ProjectResponse.ProjectLoading(projectFileName) -> CommandResponse.projectLoading JsonSerializer.writeJson projectFileName
                  | ProjectResponse.WorkspaceLoad(finished) -> CommandResponse.workspaceLoad JsonSerializer.writeJson finished
                  | ProjectResponse.ProjectChanged(projectFileName) -> failwith "Not Implemented"

              {Content = ws}
              |> lspClient.NotifyWorkspace
              |> Async.Start

          | NotificationEvent.ParseError (errors, file) ->
              let uri = Path.LocalPathToUri file
              diagnosticCollections.AddOrUpdate((uri, "F# Compiler"), [||], fun _ _ -> [||]) |> ignore

              let diags = errors |> Array.map (fcsErrorToDiagnostic)
              diagnosticCollections.AddOrUpdate((uri, "F# Compiler"), diags, fun _ _ -> diags) |> ignore
              sendDiagnostics uri

          | NotificationEvent.UnusedOpens (file, opens) ->
              let uri = Path.LocalPathToUri file
              diagnosticCollections.AddOrUpdate((uri, "F# Unused opens"), [||], fun _ _ -> [||]) |> ignore

              let diags = opens |> Array.map(fun n ->
                  {Diagnostic.Range = fcsRangeToLsp n; Code = None; Severity = Some DiagnosticSeverity.Hint; Source = "FSAC"; Message = "Unused open statement"; RelatedInformation = Some [||]; Tags = Some [| DiagnosticTag.Unnecessary |] }
              )
              diagnosticCollections.AddOrUpdate((uri, "F# Unused opens"), diags, fun _ _ -> diags) |> ignore
              sendDiagnostics uri

          | NotificationEvent.UnusedDeclarations (file, decls) ->
              let uri = Path.LocalPathToUri file
              diagnosticCollections.AddOrUpdate((uri, "F# Unused declarations"), [||], fun _ _ -> [||]) |> ignore

              let diags = decls |> Array.map(fun (n, t) ->
                  {Diagnostic.Range = fcsRangeToLsp n; Code = (if t then Some "1" else None); Severity = Some DiagnosticSeverity.Hint; Source = "FSAC"; Message = "This value is unused"; RelatedInformation = Some [||]; Tags = Some [| DiagnosticTag.Unnecessary |] }
              )
              diagnosticCollections.AddOrUpdate((uri, "F# Unused declarations"), diags, fun _ _ -> diags) |> ignore
              sendDiagnostics uri

          | NotificationEvent.SimplifyNames (file, decls) ->
              let uri = Path.LocalPathToUri file
              diagnosticCollections.AddOrUpdate((uri, "F# simplify names"), [||], fun _ _ -> [||]) |> ignore

              let diags = decls |> Array.map(fun ({ Range = range; RelativeName = _relName }) ->
                  { Diagnostic.Range = fcsRangeToLsp range
                    Code = None
                    Severity = Some DiagnosticSeverity.Hint
                    Source = "FSAC"
                    Message = "This qualifier is redundant"
                    RelatedInformation = Some [| |]
                    Tags = Some [| DiagnosticTag.Unnecessary |] }
              )
              diagnosticCollections.AddOrUpdate((uri, "F# simplify names"), diags, fun _ _ -> diags) |> ignore
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

          | NotificationEvent.Canceled (msg) ->
              let ntf = {Content = msg}
              lspClient.NotifyCancelledRequest ntf
              |> Async.Start
          | NotificationEvent.Diagnostics(p) ->
              p
              |> lspClient.TextDocumentPublishDiagnostics
              |> Async.Start
          | NotificationEvent.AnalyzerMessage(messages, file) ->

              let uri = Path.LocalPathToUri file
              diagnosticCollections.AddOrUpdate((uri, "F# Analyzers"), [||], fun _ _ -> [||]) |> ignore
              match messages with
              | [||] ->
                diagnosticCollections.AddOrUpdate((uri, "F# Analyzers"), [||], fun _ _ -> [||]) |> ignore
              | messages ->
                let fs =
                    messages
                    |> Seq.collect (fun w ->
                        w.Fixes
                        |> List.map (fun f ->
                            let range = fcsRangeToLsp f.FromRange
                            range, {Range = range; NewText = f.ToText})
                    )
                    |> Seq.toList
                let aName = messages.[0].Type

                if analyzerFixes.ContainsKey uri then () else analyzerFixes.[uri] <- new System.Collections.Generic.Dictionary<_,_>()
                analyzerFixes.[uri].[aName] <- fs

                let diag =
                    messages |> Array.map (fun m ->
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
                          Tags = None }
                    )
                diagnosticCollections.AddOrUpdate((uri, "F# Analyzers"), diag, fun _ _ -> diag) |> ignore
                sendDiagnostics uri
      with
      | _ -> ()

    /// centralize any state changes when the config is updated here
    let updateConfig (newConfig: FSharpConfig) =
        let toCompilerToolArgument (path: string) = sprintf "--compilertool:%s" path
        config <- newConfig

        let hadAnalyzersBefore = SDK.Client.registeredAnalyzers.Count <> 0

        match config.AnalyzersPath with
        | [||] ->
          Loggers.analyzers.info(Log.setMessage "Analyzers unregistered")
          SDK.Client.registeredAnalyzers.Clear()
        | paths ->
          for path in paths do
            let (newlyFound, total) = SDK.Client.loadAnalyzers path
            Loggers.analyzers.info(Log.setMessage "Registered {count} analyzers from {path}" >> Log.addContextDestructured "count" newlyFound >> Log.addContextDestructured "path" path)
          let total = SDK.Client.registeredAnalyzers.Count
          Loggers.analyzers.info(Log.setMessage "{count} Analyzers registered overall" >> Log.addContextDestructured "count" total)

        let hasAnalyzersNow = SDK.Client.registeredAnalyzers.Count <> 0
        if hadAnalyzersBefore <> hasAnalyzersNow then
          let oldCommands = commands
          let oldDisposables = commandDisposables
          let newCommands = new Commands(FSharpCompilerServiceChecker(backgroundServiceEnabled, hasAnalyzersNow), state, backgroundService, hasAnalyzersNow)
          commands <- newCommands
          commandDisposables <- ResizeArray()
          commands.SetWorkspaceRoot rootPath
          commandDisposables.Add (commands.Notify.Subscribe handleCommandEvents)
          for (disposable: IDisposable) in oldDisposables do
             disposable.Dispose()
          (oldCommands :> IDisposable).Dispose()


        // only update the dotnet root if it's both a directory and exists
        let di = DirectoryInfo config.DotNetRoot
        if di.Exists
        then
          commands.SetDotnetSDKRoot di
        else
          // if we were mistakenly given the path to a dotnet binary
          // then use the parent directory as the dotnet root instead
          let fi = FileInfo (di.FullName)
          if fi.Exists &&
            ( fi.Name = "dotnet" || fi.Name = "dotnet.exe")
          then
            commands.SetDotnetSDKRoot (fi.Directory)

        commands.SetFSIAdditionalArguments [| yield! config.FSICompilerToolLocations |> Array.map toCompilerToolArgument; yield! config.FSIExtraParameters |]
        commands.SetLinterConfigRelativePath config.LinterConfig

    do
        rootPath |> Option.iter backgroundService.Start
        commandDisposables.Add <| commands.Notify.Subscribe handleCommandEvents

    ///Helper function for handling Position requests using **recent** type check results
    member x.positionHandler<'a, 'b when 'b :> ITextDocumentPositionParams> (f: 'b -> FcsPos -> ParseAndCheckResults -> string -> string [] ->  AsyncLspResult<'a>) (arg: 'b) : AsyncLspResult<'a> =
        async {
            let pos = arg.GetFcsPos()
            let file = arg.GetFilePath() |> Utils.normalizePath
            // logger.info (Log.setMessage "PositionHandler - Position request: {file} at {pos}" >> Log.addContextDestructured "file" file >> Log.addContextDestructured "pos" pos)

            return!
                match commands.TryGetFileCheckerOptionsWithLinesAndLineStr(file, pos) with
                | ResultOrString.Error s ->
                    logger.error (Log.setMessage "PositionHandler - Getting file checker options for {file} failed" >> Log.addContextDestructured "error" s >> Log.addContextDestructured "file" file)
                    AsyncLspResult.internalError s
                | ResultOrString.Ok (options, lines, lineStr) ->
                    try
                        let tyResOpt = commands.TryGetRecentTypeCheckResultsForFile(file, options)
                        match tyResOpt with
                        | None ->
                            logger.info (Log.setMessage "PositionHandler - Cached typecheck results not yet available for {file}" >> Log.addContextDestructured "file" file)
                            AsyncLspResult.internalError "Cached typecheck results not yet available"
                        | Some tyRes ->
                            async {
                                let! r = Async.Catch (f arg pos tyRes lineStr lines)
                                match r with
                                | Choice1Of2 r -> return r
                                | Choice2Of2 e ->
                                    logger.error (Log.setMessage "PositionHandler - Failed during child operation on file {file}" >> Log.addContextDestructured "file" file >> Log.addExn e)
                                    return LspResult.internalError e.Message
                            }
                    with e ->
                        logger.error (Log.setMessage "PositionHandler - Operation failed for file {file}" >> Log.addContextDestructured "file" file >> Log.addExn e)
                        AsyncLspResult.internalError e.Message
        }

    ///Helper function for handling Position requests using **latest** type check results
    member x.positionHandlerWithLatest<'a, 'b when 'b :> ITextDocumentPositionParams> (f: 'b -> FcsPos -> ParseAndCheckResults -> string -> string [] ->  AsyncLspResult<'a>) (arg: 'b) : AsyncLspResult<'a> =
        async {
            let pos = arg.GetFcsPos()
            let file = arg.GetFilePath() |> Utils.normalizePath
            // logger.info (Log.setMessage "PositionHandler - Position request: {file} at {pos}" >> Log.addContextDestructured "file" file >> Log.addContextDestructured "pos" pos)

            return!
                    try
                        async {
                        let! tyResOpt = commands.TryGetLatestTypeCheckResultsForFile(file)
                        return!
                            match tyResOpt with
                            | None ->
                                logger.error (Log.setMessage "PositionHandler - Cached typecheck results for {file} not yet available and are required" >> Log.addContextDestructured "file" file)
                                AsyncLspResult.internalError "Cached typecheck results not yet available"
                            | Some tyRes ->
                                match commands.TryGetFileCheckerOptionsWithLinesAndLineStr(file, pos) with
                                | ResultOrString.Error s ->
                                    logger.error (Log.setMessage "PositionHandler - Getting file checker options for {file} failed" >> Log.addContextDestructured "error" s >> Log.addContextDestructured "file" file)
                                    AsyncLspResult.internalError s
                                | ResultOrString.Ok (options, lines, lineStr) ->
                                    async {
                                        let! r = Async.Catch (f arg pos tyRes lineStr lines)
                                        match r with
                                        | Choice1Of2 r -> return r
                                        | Choice2Of2 e ->
                                            logger.error (Log.setMessage "PositionHandler - Failed during child operation on file {file}" >> Log.addContextDestructured "file" file >> Log.addExn e)
                                            return LspResult.internalError e.Message
                                    }
                        }
                    with e ->
                        logger.error (Log.setMessage "PositionHandler - Operation failed for file {file}" >> Log.addContextDestructured "file" file >> Log.addExn e)
                        AsyncLspResult.internalError e.Message
        }

    member __.ScriptFileProjectOptions = commands.ScriptFileProjectOptions
    override x.Dispose () = ()

let startCore backgroundServiceEnabled toolsPath workspaceLoaderFactory =
    use input = Console.OpenStandardInput()
    use output = Console.OpenStandardOutput()

    let requestsHandlings =
        defaultRequestHandlings<FSharpLspServer> ()

    let state = State.Initial toolsPath workspaceLoaderFactory
    let originalFs = FileSystemAutoOpens.FileSystem
    FileSystemAutoOpens.FileSystem <- FsAutoComplete.FileSystem(originalFs, state.Files.TryFind)
    LanguageServerProtocol.Server.start requestsHandlings input output FSharpLspClient (fun lspClient -> new FSharpLspServer(backgroundServiceEnabled, state, lspClient))

let start backgroundServiceEnabled toolsPath workspaceLoaderFactory =
    let logger = LogProvider.getLoggerByName "Startup"

    try
        let result = startCore backgroundServiceEnabled toolsPath workspaceLoaderFactory
        logger.info (Log.setMessage "Start - Ending LSP mode with {reason}" >> Log.addContextDestructured "reason" result)
        int result
    with
    | ex ->
        logger.error (Log.setMessage "Start - LSP mode crashed" >> Log.addExn ex)
        3
