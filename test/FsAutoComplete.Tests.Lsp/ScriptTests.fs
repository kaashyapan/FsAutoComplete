module FsAutoComplete.Tests.ScriptTest

open Expecto
open System.IO
open LanguageServerProtocol
open LanguageServerProtocol.Types
open FsAutoComplete
open FsAutoComplete.LspHelpers
open Helpers
open System.Threading

let scriptPreviewTests toolsPath workspaceLoaderFactory =
  let serverStart = lazy (
    let path = Path.Combine(__SOURCE_DIRECTORY__, "TestCases", "PreviewScriptFeatures")
    let scriptPath = Path.Combine(path, "Script.fsx")
    let (server, events) = serverInitialize path { defaultConfigDto with FSIExtraParameters = Some [| "--langversion:preview" |] } toolsPath workspaceLoaderFactory
    do waitForWorkspaceFinishedParsing events |> Async.RunSynchronously
    server, events, scriptPath
  )
  let serverTest f () = f serverStart.Value

  testList "script features" [
    testCase "can typecheck scripts when preview features are used" (serverTest (fun (server, events, scriptPath) ->
      do server.TextDocumentDidOpen({ TextDocument = loadDocument scriptPath }, CancellationToken.None).Wait()
      match waitForParseResultsForFile "Script.fsx" events with
      | Ok () ->
        () // all good, no parsing/checking errors
      | Core.Result.Error errors ->
        failwithf "Errors while parsing script %s: %A" scriptPath errors
    ))
  ]

let scriptEvictionTests toolsPath workspaceLoaderFactory =
  let serverStart = lazy (
    let path = Path.Combine(__SOURCE_DIRECTORY__, "TestCases", "PreviewScriptFeatures")
    let scriptPath = Path.Combine(path, "Script.fsx")
    let (server, events) = serverInitialize path defaultConfigDto toolsPath workspaceLoaderFactory
    do waitForWorkspaceFinishedParsing events |> Async.RunSynchronously
    server, events, scriptPath
  )
  let serverTest f () = f serverStart.Value

  testList "script eviction tests" [
    ptestCase "can update script typechecking when arguments change" (serverTest (fun (server, events, scriptPath) ->
      let openScript () = do server.TextDocumentDidOpen({ TextDocument = loadDocument scriptPath }, CancellationToken.None).Wait()

      openScript ()
      match waitForParseResultsForFile "Script.fsx" events with
      | Ok () ->
        failwithf "Expected errors before we trigger script changes"
      | Core.Result.Error errors ->
        ()

      let configChange: DidChangeConfigurationParams =
        let config : FSharpConfigRequest =
          { FSharp = { defaultConfigDto with FSIExtraParameters = Some [| "--langversion:preview" |] } }
        { Settings = Server.serialize config }
      do server.WorkspaceDidChangeConfiguration(configChange, CancellationToken.None).Wait()
      do waitForParseResultsForFile "Script.fsx" events |> ignore // errors returned by background checking, not sure why it worked before...

      openScript ()
      match waitForParseResultsForFile "Script.fsx" events with
      | Ok () ->
        ()
      | Core.Result.Error errors ->
        failwithf "Should be no typecheck errors after we set the preview argument"
    ))
  ]


let dependencyManagerTests toolsPath workspaceLoaderFactory =
  let serverStart = lazy (
    let workingDir = Path.Combine(__SOURCE_DIRECTORY__, "TestCases", "DependencyManagement")
    let dependencyManagerAssemblyDir = Path.Combine(__SOURCE_DIRECTORY__, "..", "FsAutoComplete.DependencyManager.Dummy", "bin", "Debug", "netstandard2.0")
    let dependencyManagerEnabledConfig =
      { defaultConfigDto with
          FSIExtraParameters = Some [| "--langversion:preview" |] }
    let (server, events) = serverInitialize workingDir dependencyManagerEnabledConfig toolsPath workspaceLoaderFactory
    do waitForWorkspaceFinishedParsing events |> Async.RunSynchronously
    server, events, workingDir
  )

  let serverTest f = fun () -> f serverStart.Value

  testList "dependencyManager integrations" [
    testCase "can typecheck script that depends on #r dummy dependency manager" (serverTest (fun (server, events, workingDir) ->
      let scriptName = "DepManagerPresentScript.fsx"
      let scriptPath = Path.Combine(workingDir, scriptName)
      do server.TextDocumentDidOpen({ TextDocument = loadDocument scriptPath }, CancellationToken.None).Wait()
      match waitForParseResultsForFile scriptName events with
      | Ok _ -> ()
      | Core.Result.Error e ->
        failwithf "Error during typechecking: %A" e
    ))

    testCase "fails to typecheck script when dependency manager is missing" (serverTest (fun (server, events, workingDir) ->
      let scriptName = "DepManagerAbsentScript.fsx"
      let scriptPath = Path.Combine(workingDir, scriptName)
      do server.TextDocumentDidOpen({ TextDocument = loadDocument scriptPath }, CancellationToken.None).Wait()

      match waitForParseResultsForFile scriptName events with
      | Ok _ ->
        failwith "Expected to fail typechecking a script with a dependency manager that's missing"
      | Core.Result.Error e ->
        match e with
        | [| { Code = Some "3400" }; _ |] -> () // this is the error code that signals a missing dependency manager, so this is a 'success'
        | e -> failwithf "Unexpected error during typechecking: %A" e
    ))
  ]

let scriptGotoTests toolsPath workspaceLoaderFactory =
  let serverStart = lazy (
    let path = Path.Combine(__SOURCE_DIRECTORY__, "TestCases", "GoToTests")

    let (server, event) = serverInitialize path defaultConfigDto toolsPath workspaceLoaderFactory
    do waitForWorkspaceFinishedParsing event |> Async.RunSynchronously

    let scriptPath = Path.Combine(path, "Script.fsx")
    let tdop : DidOpenTextDocumentParams = { TextDocument = loadDocument scriptPath }
    do server.TextDocumentDidOpen(tdop, CancellationToken.None).Wait()

    (server, scriptPath)
  )
  let serverTest f () =
    let (server, path) = serverStart.Value
    f server path

  testSequenced <| testList "Script GoTo Tests" [
    testCase "Go-to-definition on #load integration test" (serverTest (fun server scriptPath ->
      let p : TextDocumentPositionParams = {
        TextDocument = { Uri = Path.FilePathToUri scriptPath }
        Position = { Line = 0; Character = 10 }
      }
      let res = server.TextDocumentDefinition(p, CancellationToken.None).GetAwaiter().GetResult()
      match res with
      | None -> failtest "Request none"
      | Some (GotoResult.Multiple _) -> failtest "Should only get one location"
      | Some (GotoResult.Single r) ->
        Expect.stringEnds r.Uri "/simple.fsx" "should navigate to the mentioned script file"
        ()
    ))
  ]
