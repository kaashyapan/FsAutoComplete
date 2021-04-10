namespace FsAutoComplete.BackgroundServices

open System
open System.IO
open System.Text
open LanguageServerProtocol.Server
open LanguageServerProtocol.Types
open LanguageServerProtocol
open FSharp.Compiler
open FSharp.Compiler.Text
open FSharp.Compiler.SourceCodeServices
open System.Collections.Concurrent
open FsAutoComplete
open Ionide.ProjInfo.ProjectSystem
open FSharp.UMX
open System.Reactive.Linq
open StreamJsonRpc
open Newtonsoft.Json
open System.Threading.Tasks

type BackgroundFileCheckType =
| SourceFile of filePath: string
| ScriptFile of filePath: string * tfm: FSIRefs.TFM
with
    member x.FilePath =
        match x with
        | SourceFile(path)
        | ScriptFile(path, _) -> path


type UpdateFileParms = {
    File: BackgroundFileCheckType
    Content: string
    Version: int
}

type ProjectParms = {
    Options: FSharpProjectOptions
    File: string
}

type FileParms = {
    File: BackgroundFileCheckType
}

type Msg = {Value: string}

type State = {
    Files : ConcurrentDictionary<string<LocalPath>, VolatileFile>
    FileCheckOptions : ConcurrentDictionary<string<LocalPath>, FSharpProjectOptions>
}

with
    static member Initial =
        { Files = ConcurrentDictionary(); FileCheckOptions = ConcurrentDictionary() }

module Helpers =
    let fcsSeverityToDiagnostic = function
        | FSharpDiagnosticSeverity.Error -> Some DiagnosticSeverity.Error
        | FSharpDiagnosticSeverity.Warning -> Some DiagnosticSeverity.Warning
        | FSharpDiagnosticSeverity.Hidden -> None
        | FSharpDiagnosticSeverity.Info -> Some DiagnosticSeverity.Information

    let fcsErrorToDiagnostic (error: FSharpDiagnostic) =
        {
            Range =
                {
                    Start = { Line = error.StartLineAlternate - 1; Character = error.StartColumn }
                    End = { Line = error.EndLineAlternate - 1; Character = error.EndColumn }
                }
            Severity = fcsSeverityToDiagnostic error.Severity
            Source = "F# Compiler"
            Message = error.Message
            Code = Some (string error.ErrorNumber)
            RelatedInformation = Some [||]
            Tags = None
        }

    /// Algorithm from https://stackoverflow.com/a/35734486/433393 for converting file paths to uris,
    /// modified slightly to not rely on the System.Path members because they vary per-platform
    let filePathToUri (filePath: string<LocalPath>): DocumentUri =
        let filePath = UMX.untag filePath
        let uri = StringBuilder(filePath.Length)
        for c in filePath do
            if (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || (c >= '0' && c <= '9') ||
                c = '+' || c = '/' || c = '.' || c = '-' || c = '_' || c = '~' ||
                c > '\xFF' then
                uri.Append(c) |> ignore
            // handle windows path separator chars.
            // we _would_ use Path.DirectorySeparator/AltDirectorySeparator, but those vary per-platform and we want this
            // logic to work cross-platform (for tests)
            else if c = '\\' then
                uri.Append('/') |> ignore
            else
                uri.Append('%') |> ignore
                uri.Append((int c).ToString("X2")) |> ignore

        if uri.Length >= 2 && uri.[0] = '/' && uri.[1] = '/' then // UNC path
            "file:" + uri.ToString()
        else
            "file:///" + (uri.ToString()).TrimStart('/')


module JsonRpc =
  let configureJsonRpcHandler(inStream: Stream, outStream: Stream) =
    // using a custom formatter so that we can add our own
    // * converters
    // * naming strategy
    // * ignoring of missing members on payloads for forward-compatibility
    // this lines up with the serializers used in the prior implementation
    let formatter : IJsonRpcMessageTextFormatter =
      let formatter = new JsonMessageFormatter()
      formatter.JsonSerializer.Converters
      |> Seq.tryFind (fun c -> c.GetType() = typeof<Newtonsoft.Json.Converters.DiscriminatedUnionConverter>)
      |> Option.iter (fun c -> formatter.JsonSerializer.Converters.Remove c |> ignore<bool>)

      for converter in LanguageServerProtocol.Server.customConverters do
        formatter.JsonSerializer.Converters.Add converter

      formatter.JsonSerializer.MissingMemberHandling <- MissingMemberHandling.Ignore
      formatter.JsonSerializer.ContractResolver <- Newtonsoft.Json.Serialization.CamelCasePropertyNamesContractResolver()
      formatter :> _
    // LSP uses a header-delimited message stream, so we use the handler that understands that format
    let handler = new StreamJsonRpc.HeaderDelimitedMessageHandler(outStream, inStream, formatter)
    handler

type FsacClient(rpc: JsonRpc) =

  member __.SendDiagnostics(p: PublishDiagnosticsParams) =
    rpc.NotifyWithParameterObjectAsync("background/diagnostics", p)

  member __.Notify(o: Msg) =
    rpc.NotifyWithParameterObjectAsync("background/notify", 0)

type BackgroundServiceServer(sender: Stream, reader: Stream, state: State) as this =


    let checker = FSharpChecker.Create(projectCacheSize = 1, keepAllBackgroundResolutions = false, suggestNamesForErrors = false)

    let mutable latestSdkVersion = lazy None
    let mutable latestRuntimeVersion = lazy None
    //TODO: does the backgroundservice ever get config updates?
    let rpc =
      let handler = JsonRpc.configureJsonRpcHandler (reader, sender)
      new JsonRpc(handler, this)

    let mutable client = FsacClient(rpc)

    do
      checker.ImplicitlyStartBackgroundWork <- false
      let sdkRoot = Environment.dotnetSDKRoot.Value
      if sdkRoot.Exists
      then
        latestSdkVersion <- Environment.latest3xSdkVersion sdkRoot
        latestRuntimeVersion <- Environment.latest3xRuntimeVersion sdkRoot

    let getFilesFromOpts (opts: FSharpProjectOptions) =
        (if Array.isEmpty opts.SourceFiles then
            opts.OtherOptions
            |> Seq.where (fun n -> not (n.StartsWith "-") && (n.EndsWith ".fs" || n.EndsWith ".fsi") )
            |> Seq.toArray
         else
            opts.SourceFiles)
        |> Array.map Utils.normalizePath

    let getListOfFilesForProjectChecking (file: BackgroundFileCheckType) =
        let replaceRefs (projOptions: FSharpProjectOptions) =
            let okOtherOpts = projOptions.OtherOptions |> Array.filter (fun r -> not <| r.StartsWith("-r"))
            let assemblyPaths =
              match latestSdkVersion.Value, latestRuntimeVersion.Value with
              | None, _
              | _, None ->
                []
              | Some sdkVersion, Some runtimeVersion ->
                FSIRefs.netCoreRefs Environment.dotnetSDKRoot.Value (string sdkVersion) (string runtimeVersion) (FSIRefs.tfmForRuntime sdkVersion) true
            let refs = assemblyPaths |> List.map (fun r -> "-r:" + r)
            let finalOpts = Array.append okOtherOpts (Array.ofList refs)
            { projOptions with OtherOptions = finalOpts }

        let getScriptOptions file lines tfm =
            match tfm with
            | FSIRefs.NetFx ->
                checker.GetProjectOptionsFromScript(file, SourceText.ofString lines, assumeDotNetFramework = true, useSdkRefs = false, useFsiAuxLib = true)
            | FSIRefs.NetCore ->
                async {
                    let! (opts, errors) = checker.GetProjectOptionsFromScript(file, SourceText.ofString lines, assumeDotNetFramework = false, useSdkRefs = true, useFsiAuxLib = true)
                    return replaceRefs opts, errors
                }

        match file with
        | ScriptFile(file, tfm) ->
            state.Files.TryFind (Utils.normalizePath file) |> Option.map (fun st ->
                async {
                    let! (opts, _errors) = getScriptOptions file (st.Lines |> String.concat "\n") tfm
                    let sf = getFilesFromOpts opts

                    return
                        sf
                        |> Array.skipWhile (fun n -> n <> (Utils.normalizePath file))
                        |> Array.toList
                }
            )
        | SourceFile file ->
            match state.FileCheckOptions.TryFind (Utils.normalizePath file) with
            | None ->
                client.Notify {Value = sprintf "Couldn't find file check options for %A" file } |> ignore<Task>
                client.Notify {Value = sprintf "Known files %A" (state.FileCheckOptions.Keys |> Seq.toArray) } |> ignore<Task>
                None
            | Some opts ->
                let sf = getFilesFromOpts opts

                sf
                |> Array.skipWhile (fun n -> n <> (Utils.normalizePath file))
                |> Array.toList
                |> async.Return
                |> Some

    let typecheckFile ignoredFile (file: string<LocalPath>) =
        async {
            do! client.Notify {Value = sprintf "Typechecking %s" (UMX.untag file) } |> Async.AwaitTask
            match state.Files.TryFind file, state.FileCheckOptions.TryFind file with
            | Some vf, Some opts ->
                let txt = vf.Lines |> String.concat "\n"
                let! pr, cr = checker.ParseAndCheckFileInProject(UMX.untag file, defaultArg vf.Version 0, SourceText.ofString txt, opts)
                match cr with
                | FSharpCheckFileAnswer.Aborted ->
                    do! client.Notify {Value = sprintf "Typechecking aborted %s" (UMX.untag file) } |> Async.AwaitTask
                    return ()
                | FSharpCheckFileAnswer.Succeeded res ->
                    let symbols = res.GetAllUsesOfAllSymbolsInFile()
                    SymbolCache.updateSymbols file symbols
                    match ignoredFile with
                    | Some fn when fn = file -> return ()
                    | _ ->
                        let errors = Array.append pr.Errors res.Errors |> Array.map (Helpers.fcsErrorToDiagnostic)
                        let msg = {Diagnostics = errors; Uri = Helpers.filePathToUri file}
                        do! client.SendDiagnostics msg |> Async.AwaitTask
                        return ()
            | Some vf, None when (UMX.untag file).EndsWith ".fsx" ->
                let txt = vf.Lines |> String.concat "\n"
                let! (opts, _errors) = checker.GetProjectOptionsFromScript(UMX.untag file, SourceText.ofString txt, assumeDotNetFramework = true, useSdkRefs = false)
                let! pr, cr = checker.ParseAndCheckFileInProject(UMX.untag file, defaultArg vf.Version 0, SourceText.ofString txt, opts)
                match cr with
                | FSharpCheckFileAnswer.Aborted ->
                    do! client.Notify {Value = sprintf "Typechecking aborted %s" (UMX.untag file) } |> Async.AwaitTask
                    return ()
                | FSharpCheckFileAnswer.Succeeded res ->
                    let symbols = res.GetAllUsesOfAllSymbolsInFile()
                    SymbolCache.updateSymbols file symbols
                    match ignoredFile with
                    | Some fn when fn = file -> return ()
                    | _ ->
                        let errors = Array.append pr.Errors res.Errors |> Array.map (Helpers.fcsErrorToDiagnostic)
                        let msg = {Diagnostics = errors; Uri = Helpers.filePathToUri file}
                        do! client.SendDiagnostics msg |> Async.AwaitTask
                        return ()
            | _ ->
                do! client.Notify {Value = sprintf "Couldn't find state %s" (UMX.untag file) } |> Async.AwaitTask
                return ()
        }

    let getDependingProjects (file: string<LocalPath>) =
        let project = state.FileCheckOptions.TryFind file
        match project with
        | None -> []
        | Some s ->
            state.FileCheckOptions
            |> Seq.map (fun kv -> kv.Value)
            |> Seq.distinctBy (fun o -> o.ProjectFileName)
            |> Seq.filter (fun o ->
                o.ReferencedProjects
                |> Array.map (fun (_,v) -> Path.GetFullPath v.ProjectFileName)
                |> Array.contains s.ProjectFileName )
            |> Seq.toList

    let reactor = MailboxProcessor.Start(fun agent ->
        let rec recieveLast last =
            async {
                let! msg = agent.TryReceive(5)
                match msg with
                | Some s ->
                    return! recieveLast (Some s)
                | None ->
                    return last
            }

        let rec loop (isFromSave,lst) = async {
            let! msg = recieveLast None
            match msg, lst with
            //Empty
            | None, [] ->

                // checker.ClearLanguageServiceRootCachesAndCollectAndFinalizeAllTransients ()
                do! Async.Sleep 300
                return! loop (false, [])
            //Empty
            | Some (_,_,[]), [] ->
                // checker.ClearLanguageServiceRootCachesAndCollectAndFinalizeAllTransients ()
                do! Async.Sleep 300
                return! loop (false, [])
            //No request we just continue
            | None, x::xs ->
                do! typecheckFile None x
                return! loop (isFromSave, xs)

            //We've ended processing request we start new
            | Some(saveRequest,fn, x::xs), [] ->
                do! typecheckFile (Some fn) x
                return! loop (saveRequest, xs)

            //If incoming is normal update and current is from save request we continue current
            | Some (false,_,_), x::xs when isFromSave  ->
                do! typecheckFile None x
                return! loop (isFromSave, xs)

            //If incoming is normal and previous was normal
            | Some (false,fn,(x::xs)), _ ->
                do! typecheckFile (Some fn) x
                return! loop (false, xs)

            //If incoming request is from save we always start it
            | Some(true, fn, (x::xs)), _ ->
                do! typecheckFile (Some fn) x
                return! loop (true, xs)

            //If incoming request doesn't contain any list we just continue previous one
            | Some (_,fn,[]), x::xs ->
                do! typecheckFile (Some fn) x
                return! loop (isFromSave, xs)
        }

        loop (false, [])
    )

    let bouncer = Debounce(500, reactor.Post)

    let clearOldFilesFromCache () =
      async {
        let! files = SymbolCache.getKnownFiles ()
        match files with
        | None -> ()
        | Some files ->
          for f in (Seq.distinct files) do
            if File.Exists f.FileName then ()
            else
              let! _ = SymbolCache.deleteFile f.FileName
              do! client.Notify { Value = sprintf "Cleaned file %s" f.FileName } |> Async.AwaitTask
              ()
      }

    let clearOldCacheSubscription =
        Observable.Interval(TimeSpan.FromMinutes(5.))
        |> Observable.subscribe(fun _ -> clearOldFilesFromCache () |> Async.Start)

    [<JsonRpcMethod("background/update", UseSingleObjectParameterDeserialization=true)>]
    member __.UpdateTextFile(p: UpdateFileParms) =
        async {
            do! client.Notify {Value = sprintf "File update %s" p.File.FilePath } |> Async.AwaitTask
            let file = Utils.normalizePath p.File.FilePath

            let vf = {Lines = p.Content.Split( [|'\n' |] ); Touched = DateTime.Now; Version = Some p.Version  }
            state.Files.AddOrUpdate(file, (fun _ -> vf),( fun _ _ -> vf) ) |> ignore
            let! filesToCheck = defaultArg (getListOfFilesForProjectChecking p.File) (async.Return [])
            do! client.Notify { Value = sprintf "Files to check %A" filesToCheck } |> Async.AwaitTask
            bouncer.Bounce (false, file,filesToCheck)
            return LspResult.success ()
        }

    [<JsonRpcMethod("background/project", UseSingleObjectParameterDeserialization=true)>]
    member __.UpdateProject(p: ProjectParms) =
        async {
            let sf = getFilesFromOpts p.Options

            sf
            |> Seq.iter (fun file ->
                state.FileCheckOptions.AddOrUpdate(file, (fun _ -> p.Options), (fun _ _ -> p.Options))
                |> ignore
            )
            do! client.Notify {Value = sprintf "Project Updated %s" p.Options.ProjectFileName} |> Async.AwaitTask
            return LspResult.success ()
        }

    [<JsonRpcMethod("background/save", UseSingleObjectParameterDeserialization=true)>]
    member __.FileSaved(p: FileParms) =
        async {

            let file = Utils.normalizePath p.File.FilePath

            do! client.Notify {Value = sprintf "File Saved %s " (UMX.untag file) } |> Async.AwaitTask

            let projects = getDependingProjects file
            let! filesToCheck = defaultArg (getListOfFilesForProjectChecking p.File) (async.Return [])
            let filesToCheck =
                [
                    yield! filesToCheck
                    yield! projects |> Seq.collect getFilesFromOpts
                ]
            bouncer.Bounce (true, file,filesToCheck)
            return LspResult.success ()
        }

    member _.WaitForClose = rpc.Completion

    interface IDisposable with
      member _.Dispose () =
        clearOldCacheSubscription.Dispose()

module Program =
    open Serilog
    open Serilog.Configuration
    open Serilog.Events
    open Serilog.Core
    let state = State.Initial

    // default the verbosity to warning
    let verbositySwitch = LoggingLevelSwitch(LogEventLevel.Information)
    let outputTemplate = "[{Timestamp:HH:mm:ss.fff} {Level:u3}] [{SourceContext}] {Message:lj}{NewLine}{Exception}"
    let logConf =
      LoggerConfiguration()
        .MinimumLevel.ControlledBy(verbositySwitch)
        .Enrich.FromLogContext()
        .Destructure.FSharpTypes()
        .Destructure.ByTransforming<FSharp.Compiler.Text.Range>(fun r -> box {| FileName = r.FileName; Start = r.Start; End = r.End |})
        .Destructure.ByTransforming<FSharp.Compiler.Text.Pos>(fun r -> box {| Line = r.Line; Column = r.Column |})
        .Destructure.ByTransforming<Newtonsoft.Json.Linq.JToken>(fun tok -> tok.ToString() |> box)
        .Destructure.ByTransforming<System.IO.DirectoryInfo>(fun di -> box di.FullName)
        .WriteTo.Async(
          fun c -> c.Console(outputTemplate = outputTemplate, standardErrorFromLevel = Nullable<_>(LogEventLevel.Verbose), theme = Serilog.Sinks.SystemConsole.Themes.AnsiConsoleTheme.Code) |> ignore
        ) // make it so that every console log is logged to stderr

    [<EntryPoint>]
    let main argv =

        let pid = Int32.Parse argv.[0]

        let originalFs = FileSystemAutoOpens.FileSystem
        let fs = FileSystem(originalFs, state.Files.TryFind) :> IFileSystem
        FileSystemAutoOpens.FileSystem <- fs
        ProcessWatcher.zombieCheckWithHostPID (fun () -> exit 0) pid
        SymbolCache.initCache (Environment.CurrentDirectory)
        let server = new BackgroundServiceServer(Console.OpenStandardOutput(), Console.OpenStandardInput(), state)
        server.WaitForClose.GetAwaiter().GetResult()
        0
