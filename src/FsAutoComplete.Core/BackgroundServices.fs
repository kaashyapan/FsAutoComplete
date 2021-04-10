module BackgroundServices

open FsAutoComplete
open LanguageServerProtocol
open System.IO
open FSharp.Compiler.SourceCodeServices
open Ionide.ProjInfo.ProjectSystem
open FsAutoComplete.Logging
open StreamJsonRpc
open System.Threading.Tasks

let logger =
  LogProvider.getLoggerByName "Background Service"

type Msg = { Value: string }

type BackgroundFileCheckType =
  | SourceFile of filePath: string
  | ScriptFile of filePath: string * tfm: FSIRefs.TFM
  member x.FilePath =
    match x with
    | SourceFile (path)
    | ScriptFile (path, _) -> path


type UpdateFileParms =
  { File: BackgroundFileCheckType
    Content: string
    Version: int }

type ProjectParms =
  { Options: FSharpProjectOptions
    File: string }

type FileParms = { File: BackgroundFileCheckType }

let p =
  let t = typeof<State>
  Path.GetDirectoryName t.Assembly.Location

let pid =
  System
    .Diagnostics
    .Process
    .GetCurrentProcess()
    .Id.ToString()

type MessageType = Diagnostics of Types.PublishDiagnosticsParams

type BackgroundServiceClient(exe, args) =
  inherit Client(exe, args)

  let messageRecieved = Event<MessageType>()

  member x.UpdateFile(msg: UpdateFileParms) =
    x.Rpc.NotifyWithParameterObjectAsync ("background/update", msg)

  member x.UpdateProject (msg: ProjectParms) =
    x.Rpc.NotifyWithParameterObjectAsync ("background/project", msg)

  member x.SaveFile (msg: FileParms) =
    x.Rpc.NotifyWithParameterObjectAsync("background/save", msg)

  [<JsonRpcMethod("background/notify")>]
  member x.Notify(msg: string)  =
    logger.info (Log.setMessage "Background service message {msg}" >> Log.addContextDestructured "msg" msg)

  [<JsonRpcMethod("background/diagnostics", UseSingleObjectParameterDeserialization = true)>]
  member x.Diagnostics(msg: Types.PublishDiagnosticsParams)  =
    messageRecieved.Trigger (Diagnostics msg)

  member x.MessageReceived = messageRecieved.Publish
type BackgroundService =
  abstract UpdateFile : BackgroundFileCheckType * string * int -> unit
  abstract UpdateProject : string * FSharpProjectOptions -> unit
  abstract SaveFile : BackgroundFileCheckType -> unit
  abstract MessageReceived : IEvent<MessageType>
  abstract Start : workspaceDir: string -> unit
  abstract GetSymbols: string -> Async<option<SymbolCache.SymbolUseRange array>>
  abstract GetImplementation: string -> Async<option<SymbolCache.SymbolUseRange array>>

type ActualBackgroundService() =
  let client = BackgroundServiceClient("dotnet", Path.Combine(p, "fsautocomplete.backgroundservices.dll") + " " + pid)

  interface BackgroundService with
    member x.Start (workspaceDir) =
      SymbolCache.initCache workspaceDir
      client.Start()

    member x.UpdateFile(file, content, version) =
      let msg : UpdateFileParms =
        { File = file
          Content = content
          Version = version }

      client.UpdateFile msg |> ignore<Task>

    member x.UpdateProject(file, opts) =
      let msg = { File = file; Options = opts }
      client.UpdateProject msg |> ignore<Task>

    member x.SaveFile(file) =
      let msg : FileParms = { File = file }
      client.SaveFile msg |> ignore<Task>

    member x.MessageReceived = client.MessageReceived

    member x.GetSymbols symbolName =
      SymbolCache.getSymbols symbolName

    member x.GetImplementation symbolName =
      SymbolCache.getImplementation symbolName

type MockBackgroundService() =
  let m = Event<_>()

  interface BackgroundService with
    member x.Start _ = ()

    member x.UpdateFile(file, content, version) = ()

    member x.UpdateProject(file, opts) = ()

    member x.SaveFile(file) = ()

    member x.MessageReceived = m.Publish

    member x.GetSymbols _ = async { return None }

    member x.GetImplementation _ = async { return None }
