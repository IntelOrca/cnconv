module IntelOrca.Chess.convnotation

open System
open System.Text.RegularExpressions
open System.IO
open Board
open Notation
open Types

let parseGame (s: string) =
    let readLineProperty (s: string) =
        let m = Regex.Match(s, @"^\s*(\S+)\s+(.*)\s*$")
        if m.Success then
            let key = m.Groups.[1].Value
            let value = m.Groups.[2].Value
            Ok (key, value)
        else
            Error "Bad property: "

    let rec readGameLines (lines: string list) =
        match lines with
        | head :: tail ->
            let tokens = head.Split([|' '; '\t'|], StringSplitOptions.RemoveEmptyEntries)
            if tokens.Length = 1 then
                match parseClassicNotation White tokens.[0] with
                | Some whiteMove -> Ok ([whiteMove])
                | _ -> Error "Bad move line: "
            elif tokens.Length = 2 then
                match readGameLines tail with
                | Ok nextMoves ->
                    let whiteMove = parseClassicNotation White tokens.[0]
                    let blackMove = parseClassicNotation Black tokens.[1]
                    match (whiteMove, blackMove) with
                    | (Some whiteMove, Some blackMove) ->
                        Ok (whiteMove :: blackMove :: nextMoves)
                    | (None, _) -> Error ("Bad move line: " + tokens.[0])
                    | (_, None) -> Error ("Bad move line: " + tokens.[1])
                | Error e -> Error e
            else
                Error "Bad move line: "
        | [] -> Ok []

    let rec readLines tags (lines: string list) =
        match lines with
        | "" :: tail ->
            match readGameLines tail with
            | Ok moves -> Ok (List.rev tags, moves)
            | Error e -> Error e
        | head :: tail ->
            match readLineProperty head with
            | Ok tag -> readLines (tag :: tags) tail
            | Error e -> Error e
        | [] -> Error "No game"

    let lines = s.Replace("\r\n", "\n").Split('\n') |> Array.toList
    match readLines [] lines with
    | Ok (tags, moves) ->
        Ok { tags = tags
             elements = List.map (fun x -> PgnElement.Notation x) moves }
    | Error e -> Error e

let parseGameFromCommandLine (argv: string[]) =
    let path =
        if argv.Length > 0 then
            argv.[0]
        else
            "C:\Users\Ted\Documents\GitHub\convnotation\games\game23.txt"

    File.ReadAllText(path)
    |> parseGame

let main2 (argv: string[]) =
    match parseGameFromCommandLine argv with
    | Ok pgn ->
        let moves = Pgn.getMoves pgn
        let endStates =
            getInitialBoard
            |> deriveMostLikelyState moves
        match endStates with
        | [] ->
            printfn "%s" "Unable to derive an end state"
            1
        | [single] ->
            // printfn "%s" "One end state found:"
            // printfn "Fen: %s" (getFen single)
            // printfn "Pgn:\n%s" (Pgn.fromTagsAndGameState pgn.tags single)
            printfn "%s" (Pgn.fromTagsAndGameState pgn.tags single)
            0
        | multiple ->
            printfn "%s" "Multiple end states found!"
            1
    | Error e ->
        printfn "%s" e
        1

[<EntryPoint>]
let main argv =
    main2 argv
