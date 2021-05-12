﻿// --------------------------------------------------------------------------------------
// (c) Tomas Petricek, http://tomasp.net/blog
// --------------------------------------------------------------------------------------
module FsAutoComplete.TipFormatter

open System
open System.IO
open System.Text
open System.Xml
open System.Collections.Generic
open System.Text.RegularExpressions
open FSharp.Compiler.EditorServices
open FSharp.Compiler.Symbols
open FSharp.Compiler.Text
open FSharp.Compiler.Text.TaggedText
open FsAutoComplete.Logging
open XmlDoc

let logger = LogProvider.getLoggerByName "TipFormatter"

[<RequireQualifiedAccess>]
type FormatCommentStyle =
    | Legacy
    | FullEnhanced
    | SummaryOnly
    | Documentation

// --------------------------------------------------------------------------------------
// Formatting of tool-tip information displayed in F# IntelliSense
// --------------------------------------------------------------------------------------
let private buildFormatComment (tip: ToolTipElementData) (formatStyle : FormatCommentStyle) (typeDoc: string option) =
    match tip.XmlDoc with
    | FSharpXmlDoc.FromXmlText xmlDoc ->
        try
            let lines = xmlDoc.GetElaboratedXmlLines()
            let doc = lines |> String.concat Environment.NewLine
            // We create a "fake" XML document in order to use the same parser for both libraries and user code
            let xml = $"<fake>%s{doc}</fake>"
            let doc = XmlDocument()
            doc.LoadXml(xml)

            // This try to mimic how we found the indentation size when working a real XML file
            let rec findIndentationSize (lines : string list) =
                match lines with
                | head::tail ->
                    let lesserThanIndex = head.IndexOf("<")
                    if lesserThanIndex <> - 1 then
                        lesserThanIndex
                    else
                        findIndentationSize tail
                | [] -> 0

            let indentationSize =
                lines
                |> Array.toList
                |> findIndentationSize

            let xmlDoc = XmlDocMember(doc, indentationSize, 0)

            match formatStyle with
            | FormatCommentStyle.Legacy ->
                xmlDoc.ToString()
            | FormatCommentStyle.SummaryOnly ->
                xmlDoc.ToSummaryOnlyString()
            | FormatCommentStyle.FullEnhanced ->
                xmlDoc.ToFullEnhancedString()
            | FormatCommentStyle.Documentation ->
                xmlDoc.ToDocumentationString()

        with
            | ex ->
                logger.warn (Log.setMessage "TipFormatter - Error while parsing the doc comment" >> Log.addExn ex)
                sprintf "An error occured when parsing the doc comment, please check that your doc comment is valid.\n\nMore info can be found LSP output"

    | FSharpXmlDoc.FromXmlFile(dllFile, memberName) ->
        match getXmlDoc dllFile with
        | Some doc when doc.ContainsKey memberName ->
            let typeDoc =
                match typeDoc with
                | Some s when doc.ContainsKey s ->
                    match formatStyle with
                    | FormatCommentStyle.Legacy ->
                        doc.[s].ToString()
                    | FormatCommentStyle.SummaryOnly ->
                        doc.[s].ToSummaryOnlyString()
                    | FormatCommentStyle.FullEnhanced ->
                        doc.[s].ToFullEnhancedString()
                    | FormatCommentStyle.Documentation ->
                        doc.[s].ToDocumentationString()
                | _ -> ""

            match formatStyle with
            | FormatCommentStyle.Legacy ->
                doc.[memberName].ToString() + (if typeDoc <> "" then "\n\n" + typeDoc else "")
            | FormatCommentStyle.SummaryOnly ->
                doc.[memberName].ToSummaryOnlyString() + (if typeDoc <> "" then "\n\n" + typeDoc else "")
            | FormatCommentStyle.FullEnhanced ->
                doc.[memberName].ToFullEnhancedString() + (if typeDoc <> "" then "\n\n" + typeDoc else "")
            | FormatCommentStyle.Documentation ->
                doc.[memberName].ToDocumentationString() + (if typeDoc <> "" then "\n\n" + typeDoc else "")
        | _ -> ""
    | FSharpXmlDoc.None -> ""
    //   // TODO: we should probably format the tagged text based on tag?
    //   // in this case, we just use the maindescription
    //   let mainPart =
    //    tip.MainDescription |> Array.map (fun t -> t.Text) |> String.concat ""
    //   match formatStyle with
    //   | FormatCommentStyle.Documentation ->
    //     "**Documentation**" + nl + nl + mainPart
    //   | FormatCommentStyle.FullEnhanced
    //   | FormatCommentStyle.Legacy
    //   | FormatCommentStyle.SummaryOnly -> mainPart


let private formatGenericParamInfo (text: TaggedText []) =
  match text with
  | [||] -> ""
  | elements ->
    // should be of the form X is Y
    let builder = new StringBuilder()
    builder.Append "* " |> ignore<StringBuilder>
    elements
    |> Array.iter (fun t ->
      match t.Tag with
      | TextTag.Alias
      | TextTag.Class
      | TextTag.Delegate
      | TextTag.Enum
      | TextTag.Struct
      | TextTag.TypeParameter
      | TextTag.Union
      | TextTag.Record
      | TextTag.TypeParameter ->
        builder.Append('`').Append(t.Text).Append('`') |> ignore<StringBuilder>
      | _ ->
        builder.Append(t.Text) |> ignore<StringBuilder>
    )
    builder.ToString()

let renderText (text: TaggedText) =
  match text.Tag with
  | TextTag.ActivePatternCase
  | TextTag.ActivePatternResult
  | TextTag.Alias
  | TextTag.Class
  | TextTag.Delegate
  | TextTag.Enum
  | TextTag.Event
  | TextTag.Field
  | TextTag.Function
  | TextTag.Interface
  | TextTag.Keyword
  | TextTag.LineBreak
  | TextTag.Local
  | TextTag.Member
  | TextTag.Method
  | TextTag.Module
  | TextTag.ModuleBinding
  | TextTag.Namespace
  | TextTag.NumericLiteral
  | TextTag.Operator
  | TextTag.Parameter
  | TextTag.Property
  | TextTag.Punctuation
  | TextTag.Record
  | TextTag.RecordField
  | TextTag.Space
  | TextTag.StringLiteral
  | TextTag.Struct
  | TextTag.Text
  | TextTag.TypeParameter
  | TextTag.Union
  | TextTag.UnionCase
  | TextTag.UnknownEntity
  | TextTag.UnknownType -> text.Text


let formatTip (ToolTipText tips) : (string * string) list list =
    tips
    |> List.choose (function
        | ToolTipElement.Group items ->
            let getRemarks (it : ToolTipElementData) =
              it.Remarks
              |> Option.map (fun n -> if Array.isEmpty n then n else [| tagSpace "\n\n"; yield! n |])
              |> Option.defaultValue [||]

            let makeTooltip (tipElement: ToolTipElementData) =
              let headerElements = Array.append tipElement.MainDescription (getRemarks tipElement)
              let header =
                headerElements
                |> Array.map renderText
                |> String.concat ""
              logger.info (Log.setMessage "rendering text of {text} for tooltip" >> Log.addContextDestructured "text" headerElements)
              let body = buildFormatComment tipElement FormatCommentStyle.Legacy None
              header, body

            items
            |> List.map makeTooltip
            |> Some

        | ToolTipElement.CompositionError (error) -> Some [("<Note>", error)]
        | _ -> None)

let formatTipEnhanced (ToolTipText tips) (signature : string) (footer : string) (typeDoc: string option) (formatCommentStyle : FormatCommentStyle) : (string * string * string) list list =
    tips
    |> List.choose (function
        | ToolTipElement.Group items ->
            let formatElement (i: ToolTipElementData) =
                let comment =
                    if i.TypeMapping.IsEmpty then
                      buildFormatComment i formatCommentStyle typeDoc
                    else
                      buildFormatComment i formatCommentStyle typeDoc
                      + nl + nl + "**Generic Parameters**" + nl + nl
                      + (i.TypeMapping |> List.map formatGenericParamInfo |> String.concat "\n")
                signature, comment, footer

            let items = items |> List.map formatElement
            Some items
        | ToolTipElement.CompositionError (error) -> Some [("<Note>", error, "")]
        | ToolTipElement.None -> None)

let formatDocumentation (ToolTipText tips) ((signature, (constructors, fields, functions, interfaces, attrs, ts)) : string * (string [] * string [] * string []* string[]* string[]* string[])) (footer : string) (cn: string) =
    tips
    |> List.choose (function
        | ToolTipElement.Group items ->
            Some (items |> List.map (fun i ->
                let comment =
                    if i.TypeMapping.IsEmpty then
                      buildFormatComment i FormatCommentStyle.Documentation None
                    else
                      buildFormatComment i FormatCommentStyle.Documentation None
                      + nl + nl + "**Generic Parameters**" + nl + nl
                      + (i.TypeMapping |> List.map formatGenericParamInfo |> String.concat "\n")

                (signature, constructors, fields, functions, interfaces, attrs, ts, comment, footer, cn)))
        | ToolTipElement.CompositionError (error) -> Some [("<Note>", [||],[||], [||], [||], [||], [||], error, "", "")]
        | _ -> None)

let formatDocumentationFromXmlSig (xmlSig: string) (assembly: string) ((signature, (constructors, fields, functions, interfaces, attrs, ts)) : string * (string [] * string [] * string [] * string[]* string[]* string[])) (footer : string) (cn: string) =
    let xmlDoc =  FSharpXmlDoc.FromXmlFile(assembly, xmlSig)
    let element: ToolTipElementData =
      { MainDescription = [||]
        ParamName = None
        Remarks = None
        TypeMapping = []
        XmlDoc = xmlDoc }
    let comment = buildFormatComment element FormatCommentStyle.Documentation None
    [[(signature, constructors, fields, functions, interfaces, attrs, ts, comment, footer, cn)]]

let extractSignature (ToolTipText tips) =
    let getSignature (text: TaggedText []) =
        logger.info (Log.setMessage "got tagged text of {text} while looking for signature" >> Log.addContextDestructured "text" text)
        "farts"
        // let nlpos = str.IndexOfAny([|'\r';'\n'|])
        // let firstLine =
        //     if nlpos > 0 then str.[0..nlpos-1]
        //     else str

        // if firstLine.StartsWith("type ", StringComparison.Ordinal) then
        //     let index = firstLine.LastIndexOf("=", StringComparison.Ordinal)
        //     if index > 0 then firstLine.[0..index-1]
        //     else firstLine
        // else firstLine

    let firstResult x =
        match x with
        | ToolTipElement.Group gs -> List.tryPick (fun (t : ToolTipElementData) -> if not (Array.isEmpty t.MainDescription) then Some t.MainDescription else None) gs
        | _ -> None

    tips
    |> Seq.tryPick firstResult
    |> Option.map getSignature
    |> Option.defaultValue ""

let extractGenerics (ToolTipText tips) =
    let firstResult x =
        match x with
        | ToolTipElement.Group gs ->
          gs
          |> List.tryPick (fun (t : ToolTipElementData) -> if not (t.TypeMapping.IsEmpty) then Some t.TypeMapping else None)
        | _ -> None
        |> Option.map (List.map (Array.map (fun tt -> tt.Text) >> String.concat ""))

    tips
    |> Seq.tryPick firstResult
    |> Option.defaultValue []
