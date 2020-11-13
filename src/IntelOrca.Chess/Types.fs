module IntelOrca.Chess.Types

open System

type Colour = White | Black
type Piece = Pawn | Knight | Bishop | Rook | Queen | King
type File = QueenRook | QueenKnight | QueenBishop | Queen | King | KingBishop | KingKnight | KingRook
type Rank = R1 | R2 | R3 | R4 | R5 | R6 | R7 | R8
type Location = File * Rank
type Destination =
    | Capture of Piece
    | CaptureFile of File * Piece
    | CaptureFiles of File * File * Piece
    | Ambiguous of File * File * Rank
    | Specific of Location
    | CastleKingSide
    | CastleQueenSide

type Move = { colour: Colour; piece: Piece; destination: Destination }

type Game = {
    event: string
    date: DateTime
    white: string
    black: string
    board: string
    moves: Move list
}

type CastleKind =
    | KingSide
    | QueenSide

type MoveDescriptorFile =
    | Specifc of File
    | AnyKnight
    | AnyBishop
    | AnyRook
    | Any

type MoveDescriptorEntity = {
    piece: Piece option
    file: MoveDescriptorFile
    rank: Rank option
}

type MoveDescriptor =
    | Move of MoveDescriptorEntity * MoveDescriptorEntity
    | Capture of MoveDescriptorEntity * MoveDescriptorEntity
    | Castle of CastleKind

//
// --------- PGN --------- 
//

type PgnTag = string * string

type PgnElement =
    | MoveNumber of int
    | Notation of MoveDescriptor
    | Comment of string
    | Unknown of string

type Pgn = {
    tags: PgnTag list
    elements: PgnElement list
}

//
// --------- Helpers --------- 
//

let rankToIndex = function
    | R1 -> 0
    | R2 -> 1
    | R3 -> 2
    | R4 -> 3
    | R5 -> 4
    | R6 -> 5
    | R7 -> 6
    | R8 -> 7

let fileToIndex = function
    | QueenRook -> 0
    | QueenKnight -> 1
    | QueenBishop -> 2
    | Queen -> 3
    | King -> 4
    | KingBishop -> 5
    | KingKnight -> 6
    | KingRook -> 7

let indexToRank = function
    | 0 -> Some R1
    | 1 -> Some R2
    | 2 -> Some R3
    | 3 -> Some R4
    | 4 -> Some R5
    | 5 -> Some R6
    | 6 -> Some R7
    | 7 -> Some R8
    | _ -> None
    
let indexToFile = function
    | 0 -> Some QueenRook
    | 1 -> Some QueenKnight
    | 2 -> Some QueenBishop
    | 3 -> Some Queen
    | 4 -> Some King
    | 5 -> Some KingBishop
    | 6 -> Some KingKnight
    | 7 -> Some KingRook
    | _ -> None

let locationToIndex (file, rank) =
    (fileToIndex file, rankToIndex rank)

let indexToLocation (file, rank) =
    match (indexToFile file, indexToRank rank) with
    | (Some file, Some rank) -> Some (file, rank)
    | _ -> None

let getNotationForFile file =
    let fi = fileToIndex file
    int 'a' + fi |> char |> string

let getNotationForRank rank =
    let ri = rankToIndex rank
    int '1' + ri |> char |> string

let getNotationForLocation (file, rank) =
    let fi = getNotationForFile file
    let ri = getNotationForRank rank
    fi + ri

let getLocationFromNotation (notation: string) =
    if notation <> null && notation.Length = 2 then
        let f = notation.[0]
        let r = notation.[1]
        if f >= 'a' && f <= 'h' && r >= '1' && r <= '8' then
            let file = indexToFile (int f - int 'a') |> Option.get
            let rank = indexToRank (int r - int '1') |> Option.get
            Some (file, rank)
        else
            None
    else
        None

