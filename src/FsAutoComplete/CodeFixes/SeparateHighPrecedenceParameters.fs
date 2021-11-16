module FsAutoComplete.CodeFix.SeparateHighPrecedenceParameters

open LanguageServerProtocol.Types
open FsAutoComplete.CodeFix
open FsAutoComplete.CodeFix.Types
open FsToolkit.ErrorHandling
open FsAutoComplete.LspHelpers
open FsAutoComplete
open FSharp.Compiler.Text
open FSharp.Compiler.EditorServices
open FSharp.UMX
open Newtonsoft.Json.Linq

type Contract = 
    { leftRange: LanguageServerProtocol.Types.Range
      rightRange: LanguageServerProtocol.Types.Range }

let (|OfType|_|) (o: obj option) = 
    match o with
    | Some (:? JObject as o) -> o.ToObject() |> Some
    | _ -> None

type JObject  with
    member x.TryFind<'t> key = 
        match x.TryGetValue(key) with
        | true, v -> Some (v.ToObject<'t>())
        | false, _ -> None

let fix (getRangeText: GetRangeText): CodeFix = 
    Run.ifDiagnosticByCode (Set.ofList ["3369"; "3368"]) (fun diagnostic codeActionParams -> asyncResult {
        match diagnostic.Data with
        | OfType(c: Contract) ->
            let insertRange = {Start = c.leftRange.End; End = c.leftRange.End }
            return  [ { File = codeActionParams.TextDocument
                        Edits = [| { NewText = " "; Range = insertRange } |]
                        Title = "Separate these expressions"
                        SourceDiagnostic = Some diagnostic
                        Kind = FixKind.Fix } ]
        | _ -> return []
        // | HasKey "leftRange" (r: Range) & HasKey "rightRange" (r: Range) -> 
        //     return []
        // | _ -> 
        //     return []
        // let! erroringExpressionText = getRangeText fileName diagnostic.Range
        // match erroringExpressionText |> Seq.tryHead with
        // | None -> return []
        // | Some '[' -> 
        //     match erroringExpressionText.IndexOf("][") with
        //     | -1 -> return []
        //     | n -> 
        //         let pos = { Line = diagnostic.Range.Start.Line; Character = diagnostic.Range.Start.Character + n + 1 }
        //         let insertRange = {Start = pos; End = pos }
        //         return  [ { File = codeActionParams.TextDocument
        //                     Edits = [| { NewText = " "; Range = insertRange } |]
        //                     Title = "Separate these expressions"
        //                     SourceDiagnostic = Some diagnostic
        //                     Kind = FixKind.Fix } ]
        // | Some '(' ->
        //     match erroringExpressionText.IndexOf(")[") with
        //     | -1 -> return []
        //     | n -> 
        //         let pos = { Line = diagnostic.Range.Start.Line; Character = diagnostic.Range.Start.Character + n + 1 }
        //         let insertRange = {Start = pos; End = pos }
        //         return  [ { File = codeActionParams.TextDocument
        //                     Edits = [| { NewText = " "; Range = insertRange } |]
        //                     Title = "Separate these expressions"
        //                     SourceDiagnostic = Some diagnostic
        //                     Kind = FixKind.Fix } ]
        // | Some alpha when System.Char.IsLetter alpha ->
        //     match erroringExpressionText.IndexOf("[") with
        //     | -1 -> return []
        //     | n -> 
        //         let pos = { Line = diagnostic.Range.Start.Line; Character = diagnostic.Range.Start.Character + n }
        //         let insertRange = {Start = pos; End = pos }
        //         return  [ { File = codeActionParams.TextDocument
        //                     Edits = [| { NewText = " "; Range = insertRange } |]
        //                     Title = "Separate these expressions"
        //                     SourceDiagnostic = Some diagnostic
        //                     Kind = FixKind.Fix } ]
        // | Some otherChar -> return []
    })
