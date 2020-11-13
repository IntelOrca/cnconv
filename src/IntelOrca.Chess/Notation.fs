module IntelOrca.Chess.Notation

open System.Text
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

type ParseResult<'TResult> =
    | Match of 'TResult * char list
    | BadMatch of string
    | NoMatch

type TokenKind =
    | OpenTag
    | CloseTag
    | Text of string
    | NumberDot of int
    | QuotedLiteral of string
    | BracketLiteral of string
    | BracedLiteral of string
    | Whitespace
    | NewLine

module Pgn =
    let parse (pgn: string) =
        let isDigit ch = System.Char.IsDigit(ch)

        let isWhitespace ch =
            match ch with
            | '\r'| '\n' -> false
            | ch when System.Char.IsWhiteSpace(ch) -> true
            | _ -> false

        let isWhitespaceOrNewLine ch =
            match ch with
            | '\r'| '\n' -> true
            | ch when System.Char.IsWhiteSpace(ch) -> true
            | _ -> false

        let parseWhitespace chars =
            let rec parseWhitespace c chars =
                match chars with
                | head :: tail when isWhitespace head -> parseWhitespace (c + 1) tail
                | tail ->
                    if c <> 0 then Match (Whitespace, tail)
                    else NoMatch
            parseWhitespace 0 chars

        let parseNewLine chars =
            match chars with
            | '\r' :: '\n' :: tail
            | '\r' :: tail
            | '\n' :: tail -> Match (NewLine, tail)
            | _ -> NoMatch

        let parseText chars =
            let sb = new StringBuilder()
            let rec build chars =
                match chars with
                | head :: tail when not (isWhitespaceOrNewLine head) ->
                    sb.Append(head) |> ignore
                    build tail
                | tail ->
                    if sb.Length = 0 then NoMatch
                    else Match (Text (sb.ToString()), tail)
            build chars

        let parseMoveNumber =
            let digitsToInt digits =
                let charToDigit ch = (int ch) - (int '0')
                let rec digitsToInt num digits =
                    match digits with
                    | head :: tail -> digitsToInt ((num * 10) + charToDigit head) tail
                    | [] -> num
                digitsToInt 0 (List.rev digits)

            let rec parseMoveNumber digits chars =
                match chars with
                | head :: tail when isDigit head ->
                    parseMoveNumber (head :: digits) tail
                | '.' :: w :: tail when isWhitespaceOrNewLine w ->
                    if List.isEmpty digits then NoMatch
                    else Match (NumberDot (digitsToInt digits), tail)
                | _ -> NoMatch

            parseMoveNumber []

        let parseLiteral startCh endCh chars =
            match chars with
            | head :: tail when head = startCh ->
                let sb = new StringBuilder()
                let rec build chars =
                    match chars with
                    | head :: tail ->
                        if head = endCh then
                            Match (sb.ToString(), tail)
                        else
                            sb.Append(head) |> ignore
                            build tail
                    | [] -> BadMatch "end of literal not found"
                build tail
            | _ -> NoMatch

        let parseQuotedLiteral chars =
            match parseLiteral '"' '"' chars with
            | Match (result, tail) -> Match (QuotedLiteral result, tail)
            | BadMatch err -> BadMatch err
            | NoMatch -> NoMatch

        let parseBracketLiteral chars =
            match parseLiteral '(' ')' chars with
            | Match (result, tail) -> Match (BracketLiteral result, tail)
            | BadMatch err -> BadMatch err
            | NoMatch -> NoMatch

        let parseBracedLiteral chars =
            match parseLiteral '{' '}' chars with
            | Match (result, tail) -> Match (BracedLiteral result, tail)
            | BadMatch err -> BadMatch err
            | NoMatch -> NoMatch

        let (<|>) a b =
            let innerParser chars =
                match a chars with
                | Match (result, tail) -> Match (result, tail)
                | BadMatch err -> BadMatch err
                | NoMatch -> b chars
            innerParser

        let choice parsers =
            List.reduce (<|>) parsers

        let parseChar ch token chars =
            match chars with
            | head :: tail when head = ch -> Match (token, tail)
            | _ -> NoMatch

        let rec parseToken chars =
            match chars |> (parseNewLine <|> parseWhitespace) with
            | Match (_, tail) -> parseToken tail
            | NoMatch ->
                choice [
                    parseChar '[' OpenTag
                    parseChar ']' CloseTag
                    parseQuotedLiteral
                    parseBracketLiteral
                    parseBracedLiteral
                    parseMoveNumber
                    parseText] chars
            | BadMatch err -> BadMatch err

        let parseTags chars =
            let rec parseTags tags chars =
                let parseTag chars =
                    match parseToken chars with
                    | Match (OpenTag, chars) ->
                        match parseToken chars with
                        | Match (Text key, chars) ->
                            match parseToken chars with
                            | Match (QuotedLiteral value, chars) ->
                                match parseToken chars with
                                | Match (CloseTag, chars) ->
                                    Match ((key, value), chars)
                                | BadMatch err -> BadMatch err
                                | _ -> BadMatch "expected tag close"
                            | BadMatch err -> BadMatch err
                            | _ -> BadMatch "expected tag value"
                        | BadMatch err -> BadMatch err
                        | _ -> BadMatch "expected tag name"
                    | BadMatch err -> BadMatch err
                    | _ -> NoMatch
                match parseTag chars with
                | Match (tag, tail) -> parseTags (tag :: tags) tail
                | BadMatch err -> BadMatch err
                | NoMatch -> Match(tags, chars)
            parseTags [] chars

        let parseTextElement (text: string) =
            if text.Length > 0 && isDigit text.[0] then
                // Probably win/lose marker
                NoMatch
            else
                let trimmed = text.TrimEnd('#', '+', '!', '?')
                match parseNotation Modern trimmed with
                | Some md -> Match (Notation md, [])
                | None -> BadMatch ("Unable to parse move notation: " + text)

        let parseElements chars =
            let rec parseMoves elements chars =
                match parseToken chars with
                | Match (BracketLiteral text, tail)
                | Match (BracedLiteral text, tail) ->
                    parseMoves (Comment text :: elements) tail
                | Match (NumberDot n, tail) ->
                    parseMoves (MoveNumber n :: elements) tail
                | Match (Text text, tail) ->
                    match parseTextElement text with
                    | Match (x, _) -> parseMoves (x :: elements) tail
                    | BadMatch err -> BadMatch err
                    | NoMatch -> parseMoves (Unknown text :: elements) tail
                | BadMatch err -> BadMatch err
                | NoMatch -> Match (List.rev elements, chars)
                | _ -> BadMatch "Unexpected token"
            parseMoves [] chars

        let parsePgn chars =
            match parseTags chars with
            | Match (tags, chars) ->
                match parseElements chars with
                | Match (elements, chars) ->
                    let pgn = { tags = tags; elements = elements }
                    Match (pgn, chars)
                | BadMatch err -> BadMatch err
                | NoMatch -> BadMatch "No moves"
            | BadMatch err -> BadMatch err
            | NoMatch -> NoMatch

        let chars = pgn.ToCharArray() |> Array.toList
        match parsePgn chars with
        | Match (pgn, _) -> Ok pgn
        | BadMatch err -> Error err
        | NoMatch -> Error "Empty PGN"

    let getTag (key: string) (pgn: Pgn): string option =
        let kvp =
            pgn.tags
            |> List.tryFind (fun (k, _) -> k = key)
        match kvp with
        | Some (_, value) -> Some value
        | None -> None

    let getMoves (pgn: Pgn): MoveDescriptor list =
        let getNotation el =
            match el with
            | Notation n -> Some n
            | _ -> None

        pgn.elements
        |> List.choose getNotation
