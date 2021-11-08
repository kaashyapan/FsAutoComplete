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


let fix (getRangeText: GetRangeText): CodeFix = 
    Run.ifDiagnosticByCode (Set.ofList ["3369"]) (fun diagnostic codeActionParams -> asyncResult {
        let fileName =
          codeActionParams.TextDocument.GetFilePath() |> Utils.normalizePath

        let! erroringExpressionText = getRangeText fileName diagnostic.Range
        match erroringExpressionText |> Seq.tryHead with
        | None -> return []
        | Some '[' -> 
            match erroringExpressionText.IndexOf("][") with
            | -1 -> return []
            | n -> 
                let pos = { Line = diagnostic.Range.Start.Line; Character = diagnostic.Range.Start.Character + n + 1 }
                let insertRange = {Start = pos; End = pos }
                return  [ { File = codeActionParams.TextDocument
                            Edits = [| { NewText = " "; Range = insertRange } |]
                            Title = "Separate these expressions"
                            SourceDiagnostic = Some diagnostic
                            Kind = FixKind.Fix } ]
        | Some '(' ->
            match erroringExpressionText.IndexOf(")[") with
            | -1 -> return []
            | n -> 
                let pos = { Line = diagnostic.Range.Start.Line; Character = diagnostic.Range.Start.Character + n + 1 }
                let insertRange = {Start = pos; End = pos }
                return  [ { File = codeActionParams.TextDocument
                            Edits = [| { NewText = " "; Range = insertRange } |]
                            Title = "Separate these expressions"
                            SourceDiagnostic = Some diagnostic
                            Kind = FixKind.Fix } ]
        | Some alpha when System.Char.IsLetter alpha ->
            match erroringExpressionText.IndexOf("[") with
            | -1 -> return []
            | n -> 
                let pos = { Line = diagnostic.Range.Start.Line; Character = diagnostic.Range.Start.Character + n }
                let insertRange = {Start = pos; End = pos }
                return  [ { File = codeActionParams.TextDocument
                            Edits = [| { NewText = " "; Range = insertRange } |]
                            Title = "Separate these expressions"
                            SourceDiagnostic = Some diagnostic
                            Kind = FixKind.Fix } ]
        | Some otherChar -> return []
    })
