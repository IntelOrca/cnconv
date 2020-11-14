module IntelOrca.Chess.convnotation

open System
open System.Text.RegularExpressions
open System.IO
open Board
open Notation
open Types

let pieceFromChar = function
    | 'P' -> Some Piece.Pawn
    | 'N' -> Some Piece.Knight
    | 'B' -> Some Piece.Bishop
    | 'R' -> Some Piece.Rook
    | 'Q' -> Some Piece.Queen
    | 'K' -> Some Piece.King
    | _ -> None

let filesFromPiece = function
    | 'R' -> Some (File.QueenRook, File.KingRook)
    | 'N' -> Some (File.QueenKnight, File.KingKnight)
    | 'B' -> Some (File.QueenBishop, File.KingBishop)
    | _ -> None

let fileFromString = function
    | "QR" -> Some File.QueenRook
    | "QN" -> Some File.QueenKnight
    | "QB" -> Some File.QueenBishop
    | "Q" -> Some File.Queen
    | "K" -> Some File.King
    | "KB" -> Some File.KingBishop
    | "KN" -> Some File.KingKnight
    | "KR" -> Some File.KingRook
    | _ -> None

let rankFromChar colour c =
    match colour with
    | White ->
        match c with
        | '1' -> Some R1
        | '2' -> Some R2
        | '3' -> Some R3
        | '4' -> Some R4
        | '5' -> Some R5
        | '6' -> Some R6
        | '7' -> Some R7
        | '8' -> Some R8
        | _ -> None
    | Black ->
        match c with
        | '1' -> Some R8
        | '2' -> Some R7
        | '3' -> Some R6
        | '4' -> Some R5
        | '5' -> Some R4
        | '6' -> Some R3
        | '7' -> Some R2
        | '8' -> Some R1
        | _ -> None

let destinationFromNotation colour (s: char list) =
    match s with
    | s :: p :: r :: [] ->
        // Specific
        match rankFromChar colour r with
        | Some rank ->
            let s = string s + string p
            match fileFromString s with
            | Some file -> Some (Specific (file, rank))
            | None -> None
        | None -> None
    | p :: r :: [] ->
        // Ambiguous
        match rankFromChar colour r with
        | Some rank ->
            match fileFromString (string p) with
            | Some file -> Some (Specific (file, rank))
            | None ->
                match filesFromPiece p with
                | Some (fileA, fileB) -> Some (Ambiguous (fileA, fileB, rank))
                | None -> None
        | None -> None
    | _ -> None

let getOldNotation (move: Move) =
    let getCharFromPiece = function
        | Piece.Pawn -> "P"
        | Piece.Knight -> "N"
        | Piece.Bishop -> "B"
        | Piece.Rook -> "R"
        | Piece.Queen -> "Q"
        | Piece.King -> "K"

    let getStringFromFile = function
        | File.QueenRook -> "QR"
        | File.QueenKnight -> "QN"
        | File.QueenBishop -> "QB"
        | File.Queen -> "Q"
        | File.King -> "K"
        | File.KingBishop -> "KB"
        | File.KingKnight -> "KN"
        | File.KingRook -> "KR"

    let getStringFromFileAmbig = function
        | File.QueenRook | File.KingRook -> "R"
        | File.QueenKnight | File.KingKnight -> "N"
        | File.QueenBishop | File.KingBishop -> "B"
        | _ -> null

    let getStringFromRank rank =
        match move.colour with
        | White ->
            match rank with
            | R1 -> "1"
            | R2 -> "2"
            | R3 -> "3"
            | R4 -> "4"
            | R5 -> "5"
            | R6 -> "6"
            | R7 -> "7"
            | R8 -> "8"
        | Black ->
            match rank with
            | R1 -> "8"
            | R2 -> "7"
            | R3 -> "6"
            | R4 -> "5"
            | R5 -> "4"
            | R6 -> "3"
            | R7 -> "2"
            | R8 -> "1"

    match move.destination with
    | Ambiguous (f0, _, rank) ->
        getCharFromPiece move.piece +
        "-" +
        getStringFromFileAmbig f0 +
        getStringFromRank rank
    | Specific (file, rank) ->
        getCharFromPiece move.piece +
        "-" +
        getStringFromFile file +
        getStringFromRank rank
    | CaptureFiles (fileA, _, p) ->
        getCharFromPiece move.piece + "x" +
        getStringFromFileAmbig fileA +
        getCharFromPiece p
    | CaptureFile (file, p) ->
        getCharFromPiece move.piece + "x" +
        getStringFromFile file +
        getCharFromPiece p
    | Destination.Capture p ->
        getCharFromPiece move.piece + "x" + getCharFromPiece p
    | Destination.CastleKingSide ->
        "O-O"
    | Destination.CastleQueenSide ->
        "O-O-O"

let convMove colour (s: string) =
    match s.ToCharArray() |> Array.toList with
    | 'O' :: '-' :: 'O' :: [] ->
        Some { colour = colour
               piece = Piece.King
               destination = CastleKingSide }
    | head :: tail ->
        match pieceFromChar head with
        | Some piece ->
            match tail with
            | '-' :: tail ->
                // Move
                match destinationFromNotation colour tail with
                | Some destination ->
                    Some { colour = colour
                           piece = piece
                           destination = destination }
                | None -> None
            | 'x' :: s :: f :: p :: [] ->
                // Take
                let s = (string s) + (string f)
                match fileFromString s with
                | Some file ->
                    match pieceFromChar p with
                    | Some piece ->
                        Some { colour = colour
                               piece = piece
                               destination = CaptureFile (file, piece) }
                    | None -> None
                | None -> None
            | 'x' :: f :: p :: [] ->
                // Take
                match filesFromPiece f with
                | Some (fileA, fileB) ->
                    match pieceFromChar p with
                    | Some p ->
                        Some { colour = colour
                               piece = piece
                               destination = CaptureFiles (fileA, fileB, p) }
                    | None -> None
                | None -> None
            | 'x' :: p :: [] ->
                // Take
                match pieceFromChar p with
                | Some p ->
                    Some { colour = colour
                           piece = piece
                           destination = Destination.Capture p }
                | None -> None
            | _ -> None
        | None -> None
    | [] -> None

let parseGame (s: string) =
    let mutable pEvent = ""
    let mutable pDate = ""
    let mutable pWhite = ""
    let mutable pBlack = ""
    let mutable pBoard = ""
    let mutable pMoves = []

    let readLineProperty (s: string) =
        let m = Regex.Match(s, @"^\s*(\S+)\s+(.*)\s*$")
        if m.Success then
            let key = m.Groups.[1].Value
            let value = m.Groups.[2].Value
            match key with
            | "event" ->
                pEvent <- value
                Ok ()
            | "date" ->
                pDate <- value
                Ok ()
            | "white" ->
                pWhite <- value
                Ok ()
            | "black" ->
                pBlack <- value
                Ok ()
            | "board" ->
                pBoard <- value
                Ok ()
            | _ ->
                Error "Bad property: "
        else
            Error "Bad property: "

    let rec readGameLines (lines: string list) =
        match lines with
        | head :: tail ->
            let tokens = head.Split([|' '; '\t'|], StringSplitOptions.RemoveEmptyEntries)
            if tokens.Length = 1 then
                match convMove White tokens.[0] with
                | Some whiteMove -> Ok ([whiteMove])
                | _ -> Error "Bad move line: "
            elif tokens.Length = 2 then
                match readGameLines tail with
                | Ok nextMoves ->
                    let whiteMove = convMove White tokens.[0]
                    let blackMove = convMove Black tokens.[1]
                    match (whiteMove, blackMove) with
                    | (Some whiteMove, Some blackMove) ->
                        Ok (whiteMove :: blackMove :: nextMoves)
                    | (None, _) -> Error ("Bad move line: " + tokens.[0])
                    | (_, None) -> Error ("Bad move line: " + tokens.[1])
                | Error e -> Error e
            else
                Error "Bad move line: "
        | [] -> Ok []

    let rec readLines (lines: string list) =
        match lines with
        | "" :: tail ->
            match readGameLines tail with
            | Ok moves ->
                pMoves <- moves
                Ok ()
            | Error e -> Error e
        | head :: tail ->
            match readLineProperty head with
            | Ok () -> readLines tail
            | Error e -> Error e
        | [] -> Error "No game"

    let lines = s.Replace("\r\n", "\n").Split('\n') |> Array.toList
    match readLines lines with
    | Ok () ->
        Ok { event = pEvent
             date = DateTime.Parse(pDate)
             white = pWhite
             black = pBlack
             board = pBoard
             moves = pMoves }
    | Error e ->
        Error e

let parseGameFromCommandLine (argv: string[]) =
    let path =
        if argv.Length > 0 then
            argv.[0]
        else
            "C:\Users\Ted\Documents\GitHub\convnotation\games\game23.txt"

    File.ReadAllText(path)
    |> parseGame

let main2 (argv: string[]) =
    let game = parseGameFromCommandLine argv
    match game with
    | Ok game ->
        // let moves =
        //     let iToColour i =
        //         if i &&& 1 = 0 then White
        //         else Black
        // 
        //     input.Split([|' '; '\n'|], StringSplitOptions.RemoveEmptyEntries)
        //     |> Array.indexed
        //     |> Array.map (fun (i, s) -> (iToColour i, s))
        //     |> Array.map (fun (colour, s) -> convMove colour s)

        let rec displayNextMove moves =
            match moves with
            | w :: b :: tail ->
                printfn "%-8s %s" (getOldNotation w) (getOldNotation b)
                displayNextMove tail
            | w :: [] ->
                printfn "%s" (getOldNotation w)
            | [] ->
                ()

        displayNextMove game.moves
    | Error e ->
        printfn "%s" e
    0 // return an integer exit code

[<EntryPoint>]
let main argv =
    let getColour i =
        if i &&& 1 = 0 then White
        else Black
    let moves =
        ["P-Q4";  "N-KB3"
         "P-QB4"; "P-KN3"
         "N-KB3"; "B-N2"
         "P-K3";  "O-O"
         "N-B3";  "P-Q3"
         "B-Q3";  "QN-Q2"
         "P-QN3"; "P-B4"
         "B-N2";  "P-QR3"
         "O-O";   "R-K1"
         "P-Q5";  "P-K3"
         "R-N1";  "PxP"
         "NxP";   "P-QN4"
         "NxN";   "NxN"
         "PxP";   "Q-R4"
         "P-QR4"; "PxP"
         "BxQNP"; "R-K2"
         "QxP"]
        |> List.mapi (fun i n -> parseClassicNotation (getColour i) n |> Option.get)
    let mutable currentState = getInitialBoard
    for md in moves do
        match deriveMove md currentState with
        | Some pm ->
            match doMove pm currentState with
            | Some state ->
                printfn "%s" (getMoveNotation pm currentState)
                printfn "%s" (getFen state)
                currentState <- state
            | None -> printfn "%s" "Unable to do move"
        | None -> printfn "%s" "Unable to derive move"

    printfn "PGN:\n%s" (getPgn currentState)
    0
