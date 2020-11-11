module IntelOrca.Chess.Notation

open Types

// Kinds of notation to parse:
//   e2         PxP
//   e2e4       QxP
//   Nc3        QR-B1
//   Bxf6       N-R3
//   gxf6       Q-KN2
//   Nbd2       NxP(B3)
//   dxc4
//   O-O
//   O-O-O

module MoveDescriptorEntity =
    let fromPiece piece =
        { piece = Some piece
          file = MoveDescriptorFile.Any
          rank = None }

    let fromLocation (file, rank) =
        { piece = None
          file = MoveDescriptorFile.Specifc file
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

let parseMove (s: string): MoveDescriptor option =
    // Parse helpers:
    let (|Castle|_|) c =
        match c with
        | ['O'; '-'; 'O'] -> Some CastleKind.KingSide
        | ['O'; '-'; 'O'; '-'; 'O'] -> Some CastleKind.QueenSide
        | _ -> None

    let (|File|_|) c =
        match c with
        | 'a' -> Some File.QueenRook
        | 'b' -> Some File.QueenKnight
        | 'c' -> Some File.QueenBishop
        | 'd' -> Some File.Queen
        | 'e' -> Some File.King
        | 'f' -> Some File.KingBishop
        | 'g' -> Some File.KingKnight
        | 'h' -> Some File.KingRook
        | _ -> None

    let (|Rank|_|) r =
        match r with
        | '1' -> Some R1
        | '2' -> Some R2
        | '3' -> Some R3
        | '4' -> Some R4
        | '5' -> Some R5
        | '6' -> Some R6
        | '7' -> Some R7
        | '8' -> Some R8
        | _ -> None

    let (|Location|_|) chars =
        match chars with
        | (File f) :: (Rank r) :: tail -> Some ((f, r), tail)
        | _ -> None

    let (|Piece|_|) c =
        match c with
        | 'P' :: tail -> Some (Piece.Pawn, tail)
        | 'N' :: tail -> Some (Piece.Knight, tail)
        | 'B' :: tail -> Some (Piece.Bishop, tail)
        | 'R' :: tail -> Some (Piece.Rook, tail)
        | 'Q' :: tail -> Some (Piece.Queen, tail)
        | 'K' :: tail -> Some (Piece.King, tail)
        | _ -> None

    // Parser:
    let rec parseMove chars =
        match chars with
        | Castle kind -> Some (MoveDescriptor.fromCastleKind kind)
        | Location (location, tail) ->
            match tail with
            | [] -> Some (MoveDescriptor.pieceToLocation Piece.Pawn location)
            | Location (location2, []) -> Some (MoveDescriptor.locationToLocation location location2)
            | _ -> None
        | Piece (p1, tail) ->
            match tail with
            | 'x' :: tail ->
                // Capture
                match tail with
                | Piece (p2, []) ->
                    Some (MoveDescriptor.pieceTakesPiece p1 p2)
                | _ -> None
            | '-' :: tail ->
                // Move
                None
            | _ -> None
        | _ -> None

    s.ToCharArray()
    |> Array.toList
    |> parseMove
