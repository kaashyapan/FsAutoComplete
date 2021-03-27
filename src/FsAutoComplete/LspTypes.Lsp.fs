module FsAutoComplete.ExternalLsp

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

type FSharpLspClient(rpc: JsonRpc) =
  let logger = LogProvider.getLoggerByType typeof<FSharpLspClient>

  member x.NotifyWorkspacePeek(interestingItems: list<Ionide.ProjInfo.ProjectSystem.WorkspacePeek.Interesting>) =
    let payload: CommandResponse.WorkspacePeekResponse = { Found = interestingItems |> List.map CommandResponse.mapInteresting }
    let responseMessage: CommandResponse.ResponseMsg<_> = { Kind = "workspacePeek"; Data = payload }
    rpc.NotifyWithParameterObjectAsync("fsharp/workspacePeek", responseMessage)

type FSharpLspServer(sender: Stream, reader: Stream, backgroundServiceEnabled: bool, state: State) as this =

  let logger = LogProvider.getLoggerByType typeof<FSharpLspServer>
  // using a custom formatter so that we can add our own
  // * converters
  // * naming strategy
  // * ignoring of missing members on payloads for forward-compatibility
  // this lines up with the serializers used in the prior implementation
  let formatter: IJsonRpcMessageTextFormatter =
    let formatter = new JsonMessageFormatter()
    for converter in LanguageServerProtocol.Server.customConverters do
      formatter.JsonSerializer.Converters.Add converter

    formatter.JsonSerializer.MissingMemberHandling <- MissingMemberHandling.Ignore
    formatter.JsonSerializer.ContractResolver <- Newtonsoft.Json.Serialization.CamelCasePropertyNamesContractResolver()
    formatter :> _
  // LSP uses a header-delimited message stream, so we use the handler that understands that format
  let handler = new StreamJsonRpc.HeaderDelimitedMessageHandler(sender, reader, formatter)
  let rpc = new JsonRpc(handler, this)
  let client = FSharpLspClient(rpc)

  let backgroundService: BackgroundServices.BackgroundService = if backgroundServiceEnabled then BackgroundServices.ActualBackgroundService() :> _ else BackgroundServices.MockBackgroundService() :> _
  let mutable commands = new Commands(FSharpCompilerServiceChecker(backgroundServiceEnabled, false), state, backgroundService, false)
  let mutable commandDisposables = ResizeArray()

  let mutable clientCapabilities: ClientCapabilities option = None
  let mutable glyphToCompletionKind = LspHelpers.GlyphConversions.glyphToCompletionKindGenerator None
  let mutable glyphToSymbolKind = LspHelpers.GlyphConversions.glyphToSymbolKindGenerator None
  let subscriptions = ResizeArray<IDisposable>()

  let mutable config = LspHelpers.FSharpConfig.Default
  let mutable rootPath : string option = None
  let mutable codeFixes = fun p -> [||]
    //TODO: Thread safe version
  let lintFixes = System.Collections.Generic.Dictionary<DocumentUri, (LanguageServerProtocol.Types.Range * TextEdit) list>()
  let analyzerFixes = System.Collections.Generic.Dictionary<DocumentUri, System.Collections.Generic.Dictionary<string, (LanguageServerProtocol.Types.Range * TextEdit) list>>()

  let handleCommandEvents (n: NotificationEvent) = ()

  /// centralize any state changes when the config is updated here
  let updateConfig (newConfig: LspHelpers.FSharpConfig) =
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
      match config.AnalyzersPath with
      | [||] ->
        Loggers.analyzers.info(Log.setMessage "Analyzers unregistered")
        FSharp.Analyzers.SDK.Client.registeredAnalyzers.Clear()
      | paths ->
        for path in paths do
          let (newlyFound, total) = FSharp.Analyzers.SDK.Client.loadAnalyzers path
          Loggers.analyzers.info(Log.setMessage "Registered {count} analyzers from {path}" >> Log.addContextDestructured "count" newlyFound >> Log.addContextDestructured "path" path)
        let total = FSharp.Analyzers.SDK.Client.registeredAnalyzers.Count
        Loggers.analyzers.info(Log.setMessage "{count} Analyzers registered overall" >> Log.addContextDestructured "count" total)


  do
    // hook up request/response logging for debugging
    rpc.TraceSource <- new TraceSource(typeof<FSharpLspServer>.Name, SourceLevels.Verbose)
    rpc.TraceSource.Listeners.Add(new SerilogTraceListener.SerilogTraceListener(typeof<FSharpLspServer>.Name)) |> ignore<int>
    // start the pipes flowing
    rpc.StartListening()

  member x.WaitForClose() = rpc.Completion

  // the names have to match exactly, which can suck if thye have / characters
  // also, you have to match the parameters _exactly_, which means it's safer usually to have
  // a parameter-object and turn on missing member handling ignoring in the json formatter.
  [<JsonRpcMethod("initialize", UseSingleObjectParameterDeserialization = true)>]
  member x.Initialize(p: InitializeParams, ctok: CancellationToken) =
    task {
      logger.info (Log.setMessage "Initialize Request {p}" >> Log.addContextDestructured "p" p )

      let actualRootPath =
        match p.RootUri with
        | Some rootUri -> Some (Path.FileUriToLocalPath rootUri)
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

      let tryGetParseResultsForFile fileName pos = asyncResult {
        let! (projectOptions, fileLines, lineAtPos) = commands.TryGetFileCheckerOptionsWithLinesAndLineStr(fileName, pos)
        match! commands.TryGetLatestTypeCheckResultsForFile(fileName) with
        | None -> return! Error $"No typecheck results available for %A{fileName}"
        | Some tyRes ->
          return tyRes, lineAtPos, fileLines
      }

      let getFileLines = commands.TryGetFileCheckerOptionsWithLines >> Result.map snd
      let getRangeText fileName range = getFileLines fileName |> Result.map (fun lines -> LspHelpers.Conversions.getText lines range)
      let getLineText lines range = LspHelpers.Conversions.getText lines range
      let getProjectOptsAndLines = commands.TryGetFileCheckerOptionsWithLinesAndLineStr
      let tryGetProjectOptions = commands.TryGetFileCheckerOptionsWithLines >> Result.map fst

      let interfaceStubReplacements =
        Map.ofList [
          "$objectIdent", config.InterfaceStubGenerationObjectIdentifier
          "$methodBody", config.InterfaceStubGenerationMethodBody
        ]

      let getInterfaceStubReplacements () = interfaceStubReplacements

      let unionCaseStubReplacements =
        Map.ofList [
          "$1", config.UnionCaseStubGenerationBody
        ]

      let getUnionCaseStubReplacements () = unionCaseStubReplacements

      let recordStubReplacements =
        Map.ofList [
          "$1", config.RecordStubGenerationBody
        ]

      let getRecordStubReplacements () = recordStubReplacements

      let abstractClassStubReplacements =
        Map.ofList [
          "$objectIdent", config.AbstractClassStubGenerationObjectIdentifier
          "$methodBody", config.AbstractClassStubGenerationMethodBody
        ]

      let getAbstractClassStubReplacements () = abstractClassStubReplacements

      codeFixes <- fun p ->
        [|
          Run.ifEnabled (fun _ -> config.UnusedOpensAnalyzer) UnusedOpens.fix
          Run.ifEnabled (fun _ -> config.ResolveNamespaces) (ResolveNamespace.fix tryGetParseResultsForFile commands.GetNamespaceSuggestions)
          SuggestedIdentifier.fix
          RedundantQualifier.fix
          UnusedValue.fix getRangeText
          NewWithDisposables.fix getRangeText
          Run.ifEnabled (fun _ -> config.UnionCaseStubGeneration)
            (GenerateUnionCases.fix getFileLines tryGetParseResultsForFile commands.GetUnionPatternMatchCases getUnionCaseStubReplacements)
          ExternalSystemDiagnostics.linter (fun fileUri -> match lintFixes.TryGetValue(fileUri) with | (true, v) -> Some v | (false, _) -> None )
          ExternalSystemDiagnostics.analyzers (fun fileUri -> match analyzerFixes.TryGetValue(fileUri) with | (true, v) -> Some (v.Values |> Seq.concat |> Seq.toList) | (false, _) -> None )
          Run.ifEnabled (fun _ -> config.InterfaceStubGeneration)
            (GenerateInterfaceStub.fix tryGetParseResultsForFile commands.GetInterfaceStub getInterfaceStubReplacements)
          Run.ifEnabled (fun _ -> config.RecordStubGeneration)
            (GenerateRecordStub.fix tryGetParseResultsForFile commands.GetRecordStub getRecordStubReplacements)
          Run.ifEnabled (fun _ -> config.AbstractClassStubGeneration)
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
          AddTypeToIndeterminateValue.fix tryGetParseResultsForFile tryGetProjectOptions
        |]
        |> Array.map (fun fixer -> async {
            let! fixes = fixer p
            return List.map (CodeAction.OfFix commands.TryGetFileVersion clientCapabilities.Value) fixes
         })

      match p.RootPath, c.AutomaticWorkspaceInit with
      | None, _
      | _, false -> ()
      | Some p, true ->
          async {
              let! peek = commands.WorkspacePeek p config.WorkspaceModePeekDeepLevel (List.ofArray config.ExcludeProjectDirectories)

              match peek with
              | CoreResponse.InfoRes msg | CoreResponse.ErrorRes msg ->
                  ()
              | CoreResponse.Res ints ->

                  let serialized = CommandResponse.workspacePeek JsonSerializer.writeJson ints
                  client.NotifyWorkspacePeek ints |> ignore<Task>

                  let peeks =
                      ints
                      |> List.map LspHelpers.Workspace.mapInteresting
                      |> List.sortByDescending (fun x ->
                          match x with
                          | CommandResponse.WorkspacePeekFound.Solution sln -> LspHelpers.Workspace.countProjectsInSln sln
                          | CommandResponse.WorkspacePeekFound.Directory _ -> -1)

                  match peeks with
                  | [] -> ()
                  | [CommandResponse.WorkspacePeekFound.Directory projs] ->
                      commands.WorkspaceLoad projs.Fsprojs false config.ScriptTFM config.GenerateBinlog
                      |> Async.Ignore
                      |> Async.Start
                  | CommandResponse.WorkspacePeekFound.Solution sln::_ ->
                      let projs =
                          sln.Items
                          |> List.collect LspHelpers.Workspace.foldFsproj
                          |> List.map fst
                      commands.WorkspaceLoad projs false config.ScriptTFM config.GenerateBinlog
                      |> Async.Ignore
                      |> Async.Start
                  | _ ->
                      //TODO: Above case always picks solution with most projects, should be changed
                      ()


              return ()
          } |> Async.Start
      return { InitializeResult.Default with
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
                        SignatureHelpProvider = Some {
                            TriggerCharacters = Some [| '('; ','; ' ' |]
                            RetriggerCharacters = Some [| ','; ')'; ' '|]
                        }
                        CompletionProvider =
                            Some {
                                ResolveProvider = Some true
                                TriggerCharacters = Some ([| '.'; '''; |])
                                AllCommitCharacters = None //TODO: what chars shoudl commit completions?
                            }
                        CodeLensProvider = Some {
                            CodeLensOptions.ResolveProvider = Some true
                        }
                        CodeActionProvider = Some true
                        TextDocumentSync =
                            Some { TextDocumentSyncOptions.Default with
                                     OpenClose = Some true
                                     Change = Some TextDocumentSyncKind.Full
                                     Save = Some { IncludeText = Some true }
                                 }
                        FoldingRangeProvider = Some true
                        SelectionRangeProvider = Some true
                        SemanticTokensProvider = Some {
                          Legend = FsAutoComplete.LspHelpers.createTokenLegend<LspHelpers.ClassificationUtils.SemanticTokenTypes, LspHelpers.ClassificationUtils.SemanticTokenModifier>
                          Range = Some (LanguageServerProtocol.LspJsonConverters.U2.First true)
                          Full = Some (LanguageServerProtocol.LspJsonConverters.U2.First true)
                        }
                    }
            }
    }

