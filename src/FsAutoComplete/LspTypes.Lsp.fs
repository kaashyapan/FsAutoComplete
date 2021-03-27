module FsAutoComplete.ExternalLsp

open FsAutoComplete
open StreamJsonRpc
open System.IO
open FSharp.Control.Tasks.Affine
open LanguageServerProtocol.Types
open System.Diagnostics
open Newtonsoft.Json.Linq
open Newtonsoft.Json

type FSharpLspServer(sender: Stream, reader: Stream, backgroundServiceEnabled: bool, state: State) as this =

  let formatter: IJsonRpcMessageTextFormatter =
    let formatter = new JsonMessageFormatter()
    for converter in LanguageServerProtocol.Server.customConverters do
      formatter.JsonSerializer.Converters.Add converter

    formatter.JsonSerializer.MissingMemberHandling <- MissingMemberHandling.Ignore
    formatter.JsonSerializer.ContractResolver <- Newtonsoft.Json.Serialization.CamelCasePropertyNamesContractResolver()
    formatter :> _
  let handler = new StreamJsonRpc.HeaderDelimitedMessageHandler(sender, reader, formatter)
  let rpc = new JsonRpc(handler, this)

  let backgroundService: BackgroundServices.BackgroundService = if backgroundServiceEnabled then BackgroundServices.ActualBackgroundService() :> _ else BackgroundServices.MockBackgroundService() :> _
  let mutable commands = new Commands(FSharpCompilerServiceChecker(backgroundServiceEnabled, false), state, backgroundService, false)
  let mutable commandDisposables = ResizeArray()

  do
    rpc.TraceSource <- new TraceSource(typeof<FSharpLspServer>.Name, SourceLevels.Verbose)
    rpc.TraceSource.Listeners.Add(new SerilogTraceListener.SerilogTraceListener(typeof<FSharpLspServer>.Name)) |> ignore<int>
    rpc.StartListening()

  member x.WaitForClose() = rpc.Completion

  [<JsonRpcMethod("initialize", UseSingleObjectParameterDeserialization = true)>]
  member x.Initialize(p: InitializeParams) =
    task {
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

