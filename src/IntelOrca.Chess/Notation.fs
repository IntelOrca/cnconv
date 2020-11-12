module IntelOrca.Chess.Notation

open Types

type NotationKind =
    | Classic
    | Modern

module MoveDescriptorEntity =
    let fromPiece piece =
        { piece = Some piece
          file = MoveDescriptorFile.Any
          rank = None }

    let fromLocation (file, rank) =
        { piece = None
          file = MoveDescriptorFile.Specifc file
          rank = Some rank }

    let fromFile file =
        { piece = None
          file = file
          rank = None }

    let fromFileRank (file, rank) =
        { piece = None
          file = file
          rank = Some rank }

module MoveDescriptor =
    let fromCastleKind kind =
        MoveDescriptor.Castle kind

    let pieceToLocation piece location =
        MoveDescriptor.Move (MoveDescriptorEntity.fromPiece piece, MoveDescriptorEntity.fromLocation location)

    let locationToLocation src dst =
        MoveDescriptor.Move (MoveDescriptorEntity.fromLocation src, MoveDescriptorEntity.fromLocation dst)

    let pieceTakesPiece a b =
        MoveDescriptor.Capture (MoveDescriptorEntity.fromPiece a, MoveDescriptorEntity.fromPiece b)

let parseNotation (kind: NotationKind) (s: string): MoveDescriptor option =
    // Parse helpers:
    let (|Castle|_|) = function
        | ['O'; '-'; 'O'] -> Some CastleKind.KingSide
        | ['O'; '-'; 'O'; '-'; 'O'] -> Some CastleKind.QueenSide
        | _ -> None

    let (|File|_|) = function
        | 'a' -> Some File.QueenRook
        | 'b' -> Some File.QueenKnight
        | 'c' -> Some File.QueenBishop
        | 'd' -> Some File.Queen
        | 'e' -> Some File.King
        | 'f' -> Some File.KingBishop
        | 'g' -> Some File.KingKnight
        | 'h' -> Some File.KingRook
        | _ -> None

    let (|Rank|_|) = function
        | '1' -> Some R1
        | '2' -> Some R2
        | '3' -> Some R3
        | '4' -> Some R4
        | '5' -> Some R5
        | '6' -> Some R6
        | '7' -> Some R7
        | '8' -> Some R8
        | _ -> None

    let (|Piece|_|) = function
        | 'P' -> Some Piece.Pawn
        | 'N' -> Some Piece.Knight
        | 'B' -> Some Piece.Bishop
        | 'R' -> Some Piece.Rook
        | 'Q' -> Some Piece.Queen
        | 'K' -> Some Piece.King
        | _ -> None

    let (|Cross|_|) = function
        | 'x' :: tail -> Some tail
        | _ -> None

    let (|Hyphen|_|) = function
        | '-' :: tail -> Some tail
        | _ -> None

    // Parser:
    let parseClassic chars =
        let (|LeftComponent|_|) = function
            | 'Q' :: 'R' :: tail ->
                Some ({ piece = Some Piece.Rook
                        file = MoveDescriptorFile.Specifc QueenRook
                        rank = None }, tail)
            | Piece piece :: tail -> Some (MoveDescriptorEntity.fromPiece piece, tail)
            | _ -> None

        let (|RightComponent|_|) = function
            | 'K' :: (Rank r) :: tail -> Some (MoveDescriptorEntity.fromLocation (File.King, r), tail)
            | 'K' :: 'R' :: (Rank r) :: tail -> Some (MoveDescriptorEntity.fromLocation (File.KingRook, r), tail)
            | 'K' :: 'B' :: (Rank r) :: tail -> Some (MoveDescriptorEntity.fromLocation (File.KingBishop, r), tail)
            | 'K' :: 'N' :: (Rank r) :: tail -> Some (MoveDescriptorEntity.fromLocation (File.KingKnight, r), tail)
            | 'Q' :: (Rank r) :: tail -> Some (MoveDescriptorEntity.fromLocation (File.Queen, r), tail)
            | 'Q' :: 'R' :: (Rank r) :: tail -> Some (MoveDescriptorEntity.fromLocation (File.QueenRook, r), tail)
            | 'Q' :: 'B' :: (Rank r) :: tail -> Some (MoveDescriptorEntity.fromLocation (File.QueenBishop, r), tail)
            | 'Q' :: 'N' :: (Rank r) :: tail -> Some (MoveDescriptorEntity.fromLocation (File.QueenKnight, r), tail)
            | 'R' :: (Rank r) :: tail -> Some (MoveDescriptorEntity.fromFileRank (MoveDescriptorFile.AnyRook, r), tail)
            | 'B' :: (Rank r) :: tail -> Some (MoveDescriptorEntity.fromFileRank (MoveDescriptorFile.AnyBishop, r), tail)
            | 'N' :: (Rank r) :: tail -> Some (MoveDescriptorEntity.fromFileRank (MoveDescriptorFile.AnyKnight, r), tail)
            | Piece piece :: tail -> Some (MoveDescriptorEntity.fromPiece piece, tail)
            | _ -> None

        match chars with
        | Castle kind -> Some (MoveDescriptor.fromCastleKind kind)
        | LeftComponent (c1, chars) ->
            match chars with
            | Cross chars ->
                // Capture
                match chars with
                | RightComponent (c2, _) -> Some (MoveDescriptor.Capture (c1, c2))
                | _ -> None
            | Hyphen chars ->
                // Move
                match chars with
                | RightComponent (c2, _) -> Some (MoveDescriptor.Move (c1, c2))
                | _ -> None
            | _ -> None
        | _ -> None

    let parseModern chars =
        let (|Location|_|) = function
            | (File f) :: (Rank r) :: tail -> Some ((f, r), tail)
            | _ -> None

        let rec (|Component|_|) = function
            | Location (location, tail) ->
                Some (MoveDescriptorEntity.fromLocation location, tail)
            | File f :: tail ->
                Some (MoveDescriptorEntity.fromFile (MoveDescriptorFile.Specifc f), tail)
            | Piece piece :: tail ->
                match tail with
                | File _ :: Component (_, _) ->
                    match tail with
                    | File f :: tail ->
                        Some ({ piece = Some piece
                                file = MoveDescriptorFile.Specifc f
                                rank = None }, tail)
                    | _ -> None
                | _ -> Some (MoveDescriptorEntity.fromPiece piece, tail)
            | _ -> None

        match chars with
        | Castle kind -> Some (MoveDescriptor.fromCastleKind kind)
        | Component (c1, chars) ->
            match chars with
            | Component (c2, _) ->
                // Move
                Some (MoveDescriptor.Move (c1, c2))
            | Cross chars ->
                // Capture
                match chars with
                | Component (c2, _) -> Some (MoveDescriptor.Capture (c1, c2))
                | _ -> None
            | [] ->
                // Pawn move (e.g. e4)
                Some (MoveDescriptor.Move (MoveDescriptorEntity.fromPiece Piece.Pawn, c1))
            | _ -> None
        | _ -> None

    let chars =
        s.ToCharArray()
        |> Array.toList
    match kind with
    | Classic -> parseClassic chars
    | Modern -> parseModern chars
