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
        | head :: tail when System.String.IsNullOrWhiteSpace(head) -> readGameLines tail
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

type GameStateNode = {
    state: GameState
    children: GameStateNode list
}

let gameStateTree (leaves: GameState list) =
    let getParent (child: GameState) =
        match child.previous with
        | Some (parent, _) -> Some parent
        | None -> None

    let rec getNode (child: GameState) (map: Map<GameState, GameStateNode>) =
        let addNode state map =
            let node = { state = state; children = [] }
            let map = Map.add child node map
            (map, node)

        match Map.tryFind child map with
        | Some node -> (map, node)
        | None ->
            match getParent child with
            | Some parent ->
                let (map, parentNode) = getNode parent map
                let (map, node) = addNode child map
                let map =
                    map
                    |> Map.add parentNode.state { parentNode with children = node :: parentNode.children }
                (map, node)
            | None ->
                addNode child map

    let mutable map = Map.empty
    for leaf in leaves do
        map <- getNode leaf map |> fst

    map
    |> Map.tryPick (fun x y -> if Option.isNone x.previous then Some y else None)

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
            printfn "\nFen:\n%s" (getFen single)
            0
        | multiple ->
            let root = gameStateTree multiple
            printfn "%s" "Multiple end states found!"
            for state in multiple do
                printfn "  Pgn:\n%s" (Pgn.fromGameState state)
                printfn "  Fen:\n%s" (getFen state)
            1
    | Error e ->
        printfn "%s" e
        1

[<EntryPoint>]
let main argv =
    match argv |> Array.toList with
    | "derive" :: tail ->
        match tail with
        | fen :: move :: _ ->
            match fromFen fen with
            | Some state ->
                printfn "FEN: %s" (getFen state)
                match parseClassicNotation state.toMove move with
                | Some md ->
                    let possibleMoves =
                        deriveMoves md state
                        |> List.mapi (fun i x -> (i, x))
                    for (i, move) in possibleMoves do
                        match doMove move state with
                        | Some state ->
                            let (prevState, prevMove) = state.previous |> Option.get
                            let move = Pgn.getMoveNotation prevMove prevState
                            let fen = getFen state
                            printfn "  %s -> %s" move fen
                        | None -> printfn "  invalid"
                    0
                | None ->
                    printfn "Invalid classic move notation"
                    1
            | None ->
                printfn "Invalid FEN"
                1
        | _ ->
            printfn "usage: derive <fen> <move>"
            1
    | _ ->
        main2 argv
