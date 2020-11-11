module IntelOrca.Chess.Board

open System
open Types
open System.Text

type PieceState = {
    colour: Colour
    piece: Piece
    location: Location
}

type PossibleMove =
    | AtoB of Location * Location
    | CastleQueenSide
    | CastleKingSide

type GameState = {
    previous: (GameState * PossibleMove) option
    moveNumber: int
    halfMoveClock: int
    toMove: Colour
    pieces: PieceState list
}

let getNotation (move: PossibleMove) =
    match move with
    | AtoB (a, b) -> (getNotationForLocation a) + (getNotationForLocation b)
    | CastleQueenSide -> "O-O"
    | CastleKingSide -> "O-O-O"

let createPieceState colour piece file rank =
    { colour = colour
      piece = piece
      location = (file, rank) }

let getInitialBoard =
    { previous = None
      moveNumber = 1
      halfMoveClock = 0
      toMove = White
      pieces = [
          (createPieceState White Piece.Rook File.QueenRook Rank.R1)
          (createPieceState White Piece.Knight File.QueenKnight Rank.R1)
          (createPieceState White Piece.Bishop File.QueenBishop Rank.R1)
          (createPieceState White Piece.Queen File.Queen Rank.R1)
          (createPieceState White Piece.King File.King Rank.R1)
          (createPieceState White Piece.Bishop File.KingBishop Rank.R1)
          (createPieceState White Piece.Knight File.KingKnight Rank.R1)
          (createPieceState White Piece.Rook File.KingRook Rank.R1)

          (createPieceState White Piece.Pawn File.QueenRook Rank.R2)
          (createPieceState White Piece.Pawn File.QueenKnight Rank.R2)
          (createPieceState White Piece.Pawn File.QueenBishop Rank.R2)
          (createPieceState White Piece.Pawn File.Queen Rank.R2)
          (createPieceState White Piece.Pawn File.King Rank.R2)
          (createPieceState White Piece.Pawn File.KingBishop Rank.R2)
          (createPieceState White Piece.Pawn File.KingKnight Rank.R2)
          (createPieceState White Piece.Pawn File.KingRook Rank.R2)

          (createPieceState Black Piece.Pawn File.QueenRook Rank.R7)
          (createPieceState Black Piece.Pawn File.QueenKnight Rank.R7)
          (createPieceState Black Piece.Pawn File.QueenBishop Rank.R7)
          (createPieceState Black Piece.Pawn File.Queen Rank.R7)
          (createPieceState Black Piece.Pawn File.King Rank.R7)
          (createPieceState Black Piece.Pawn File.KingBishop Rank.R7)
          (createPieceState Black Piece.Pawn File.KingKnight Rank.R7)
          (createPieceState Black Piece.Pawn File.KingRook Rank.R7)

          (createPieceState Black Piece.Rook File.QueenRook Rank.R8)
          (createPieceState Black Piece.Knight File.QueenKnight Rank.R8)
          (createPieceState Black Piece.Bishop File.QueenBishop Rank.R8)
          (createPieceState Black Piece.Queen File.Queen Rank.R8)
          (createPieceState Black Piece.King File.King Rank.R8)
          (createPieceState Black Piece.Bishop File.KingBishop Rank.R8)
          (createPieceState Black Piece.Knight File.KingKnight Rank.R8)
          (createPieceState Black Piece.Rook File.KingRook Rank.R8)
      ]}

let getNextColour = function
    | White -> Black
    | Black -> White

let getPossibleMoves (state: GameState): PossibleMove list =
    let isOccupied location =
        state.pieces
        |> List.exists (fun x -> x.location = location)
    let isTheirs location =
        state.pieces
        |> List.where (fun x -> x.colour <> state.toMove)
        |> List.exists (fun x -> x.location = location)
    let isOurs location =
        state.pieces
        |> List.where (fun x -> x.colour = state.toMove)
        |> List.exists (fun x -> x.location = location)
    let getMoveLocation (ps: PieceState) x y =
        let y =
            if ps.colour = White then y
            else -y
        let (oldX, oldY) = locationToIndex ps.location
        indexToLocation (oldX + x, oldY + y)

    let getPossibleMoves (ps: PieceState): PossibleMove list =
        let followPath offsetX offsetY =
            let rec followPath locations x y =
                match getMoveLocation ps x y with
                | Some location ->
                    if isOurs location then
                        locations
                    elif isTheirs location then
                        location :: locations
                    else
                        followPath (location :: locations) (x + offsetX) (y + offsetY)
                | None -> locations
            followPath [] offsetX offsetY

        let getMove (x: int, y: int) =
            match getMoveLocation ps x y with
            | Some location ->
                if isOurs location then None
                else Some (AtoB (ps.location, location))
            | None -> None

        match ps.piece with
        | Piece.Pawn ->
            let forward =
                match getMoveLocation ps 0 1 with
                | Some location ->
                    if isOccupied location then None
                    else Some (AtoB (ps.location, location))
                | None -> None
            let forwardTwo =
                let isInitalPosition =
                    let rank =
                        if ps.colour = White then Rank.R2
                        else Rank.R7
                    snd ps.location = rank
                match isInitalPosition, getMoveLocation ps 0 2 with
                | true, Some location ->
                    if isOccupied location then None
                    else Some (AtoB (ps.location, location))
                | _ -> None
            let diagonal i =
                match getMoveLocation ps i 1 with
                | Some location ->
                    if isTheirs location then Some (AtoB (ps.location, location))
                    else None
                | None -> None
            [forward; forwardTwo; diagonal -1; diagonal 1]
            |> List.choose id
        | Piece.Knight ->
            [(-2, -1); (-2, 1); (-1, -2); (-1, 2); (1, -2); (1, 2); (2, -1); (2, 1)]
            |> List.map getMove
            |> List.choose id
        | Piece.Bishop ->
            [followPath -1 -1; followPath 1 1; followPath 1 -1; followPath -1 1]
            |> List.collect id
            |> List.map (fun x -> AtoB (ps.location, x))
        | Piece.Rook ->
            [followPath -1 0; followPath 1 0; followPath 0 -1; followPath 0 1]
            |> List.collect id
            |> List.map (fun x -> AtoB (ps.location, x))
        | Piece.Queen ->
            [followPath -1 0; followPath 1 0; followPath 0 -1; followPath 0 1;
             followPath -1 -1; followPath 1 1; followPath 1 -1; followPath -1 1]
            |> List.collect id
            |> List.map (fun x -> AtoB (ps.location, x))
        | Piece.King ->
            [(-1, -1); (0, -1); (1, -1);
             (-1,  0);          (1,  0);
             (-1,  1); (0,  1); (1,  1)]
            |> List.map getMove
            |> List.choose id

    state.pieces
    |> List.where (fun x -> x.colour = state.toMove)
    |> List.collect getPossibleMoves

let getPiece (state: GameState) (move: PossibleMove): Piece option =
    match move with
    | AtoB (a, _) ->
        let ps =
            state.pieces
            |> List.tryFind (fun x -> x.location = a)
        match ps with
        | Some ps -> Some ps.piece
        | None -> None
    | _ -> Some Piece.King

let getDestination (move: PossibleMove) =
    match move with
    | AtoB (_, b) -> b
    | _ -> (File.QueenRook, R1)

let getPieceAt location pieces =
    pieces
    |> List.tryFind (fun x -> x.location = location)

let removePiece location pieces =
    pieces
    |> List.where (fun x -> x.location <> location)

let addPiece ps pieces = ps :: pieces

let doMove (move: PossibleMove) (state: GameState): GameState option =
    let toMove =
        if state.toMove = Black then White
        else Black
    let moveNumber =
        if state.toMove = Black then
            state.moveNumber + 1
        else
            state.moveNumber

    match move with
    | AtoB (a, b) ->
        let movedPiece = state.pieces |> getPieceAt a
        let capturedPiece = state.pieces |> getPieceAt b
        let newPieces =
            match movedPiece with
            | Some ps ->
                state.pieces
                |> removePiece a
                |> removePiece b
                |> addPiece { ps with location = b }
                |> Some
            | None -> None

        let halfMoveClock =
            match (movedPiece, capturedPiece) with
            | (Some movedPiece, _) when movedPiece.piece = Piece.Pawn -> 0
            | (Some movedPiece, Some capturedPiece) -> 0
            | _ -> state.halfMoveClock + 1

        match newPieces with
        | Some pieces ->
            Some { previous = Some (state, move)
                   moveNumber = moveNumber
                   halfMoveClock = halfMoveClock
                   toMove = toMove
                   pieces = pieces }
        | None -> None
    | CastleQueenSide -> None
    | CastleKingSide -> None

let doMoveWithOld (move: Move) (state: GameState) =
    let possibleMoves =
        getPossibleMoves state
        |> List.where (fun x -> (getPiece state x) = Some move.piece)

    match move.destination with
    | Specific destination ->
        let move =
            possibleMoves
            |> List.tryFind (fun x -> getDestination x = destination)
        match move with
        | Some move -> doMove move state
        | None -> None
    | _ -> None

let getFen (state: GameState): string =
    let foldSpace (chars: char list) =
        let sb = new StringBuilder()
        let append (c: char) = sb.Append(c) |> ignore
        let appendAcc acc = if acc <> 0 then sb.Append(acc) |> ignore
        let rec foldSpace (acc: int) (chars: char list) =
            match chars with
            | ' ' :: tail ->
                foldSpace (1 + acc) tail
            | head :: tail ->
                appendAcc acc
                append head
                foldSpace 0 tail
            | [] ->
                appendAcc acc
        foldSpace 0 chars
        sb.ToString()

    let getRow rank =
        [ for x = 0 to 7 do
              let file = indexToFile x |> Option.get
              let ps =
                  state.pieces
                  |> getPieceAt (file, rank)
              match ps with
              | Some ps ->
                  match ps.colour with
                  | White ->
                      match ps.piece with
                      | Piece.Pawn -> 'P'
                      | Piece.Knight -> 'N'
                      | Piece.Bishop -> 'B'
                      | Piece.Rook -> 'R'
                      | Piece.Queen -> 'Q'
                      | Piece.King -> 'K'
                  | Black ->
                      match ps.piece with
                      | Piece.Pawn -> 'p'
                      | Piece.Knight -> 'n'
                      | Piece.Bishop -> 'b'
                      | Piece.Rook -> 'r'
                      | Piece.Queen -> 'q'
                      | Piece.King -> 'k'
              | None -> ' ' ]
        |> foldSpace

    let placement =
        [| for y = 7 downto 0 do
               let rank = indexToRank y |> Option.get
               getRow rank |]
        |> String.concat "/"
    let toMove = if state.toMove = White then "w" else "b"
    let castlingAbility = "KQkq"
    let enpassant = "-"
    let halfmove = string state.halfMoveClock
    let fullmove = string state.moveNumber

    // rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1
    seq { placement; toMove; castlingAbility; enpassant; halfmove; fullmove }
    |> String.concat " "

let getMoveSourcePieceKind (move: PossibleMove) (state: GameState) =
    match move with
    | AtoB (src, dst) ->
        match getPieceAt src state.pieces with
        | Some ps -> Some ps.piece
        | None -> None
    | _ -> Some Piece.King

let getMoveSourceLocation (move: PossibleMove) (state: GameState) =
    match move with
    | AtoB (src, _) -> src
    | _ -> (File.KingRook, R1)

let getMoveTargetLocation (move: PossibleMove) (state: GameState) =
    match move with
    | AtoB (_, dst) -> dst
    | _ -> (File.KingRook, R1)

let getMoveNotation (move: PossibleMove) (state: GameState) =
    let srcPiece = getMoveSourcePieceKind move state
    let srcLocation = getMoveSourceLocation move state
    let dstLocation = getMoveTargetLocation move state
    let allPossibleMoves =
        state
        |> getPossibleMoves
        |> List.where (fun x -> getMoveSourcePieceKind x state = srcPiece)
        |> List.where (fun x -> getMoveTargetLocation x state = dstLocation)

    let getPieceNotation = function
        | Piece.Pawn -> ""
        | Piece.Knight -> "N"
        | Piece.Bishop -> "B"
        | Piece.Rook -> "R"
        | Piece.Queen -> "Q"
        | Piece.King -> "K"

    match (srcPiece, allPossibleMoves) with
    | Some srcPiece, _ :: [] ->
        match getPieceAt dstLocation state.pieces with
        | Some ps when ps.piece = Piece.Pawn -> getNotationForFile (fst srcLocation) + "x" + (getNotationForLocation dstLocation)
        | Some _ -> (getPieceNotation srcPiece) + "x" + (getNotationForLocation dstLocation)
        | _ -> (getPieceNotation srcPiece) + (getNotationForLocation dstLocation)
    | _ ->
        match move with
        | AtoB (src, dst) ->
            let src = getNotationForLocation src
            let dst = getNotationForLocation dst
            src + dst
        | CastleKingSide -> "O-O"
        | CastleQueenSide -> "O-O-O"

let getPgn (state: GameState): string =
    let moves = 
        let rec getMoves moves state =
            match state.previous with
            | Some (prev, move) -> getMoves (getMoveNotation move prev :: moves) prev
            | None -> moves
        getMoves [] state

    moves
    |> List.mapi (
        fun i n ->
            if i &&& 1 = 0 then
                let moveNumber = 1 + (i / 2)
                sprintf "%d. %s" moveNumber n
            else
                n)
    |> String.concat " "
