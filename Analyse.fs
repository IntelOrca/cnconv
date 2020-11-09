module IntelOrca.ChessNotationConv.Analyse

open Types

type PieceState = {
    colour: Colour
    piece: Piece
    location: Location
}

type GameState = {
    lastState: GameState option
    moveNumber: int
    toMove: Colour
    pieces: PieceState list
}

type PossibleMove =
    | AtoB of Location * Location
    | CastleQueenSide
    | CastleKingSide

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
    { lastState = None
      moveNumber = 0
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
        match ps.piece with
        | Pawn ->
            let forward =
                match getMoveLocation ps 0 1 with
                | Some location ->
                    if isOccupied location then None
                    else Some (AtoB (ps.location, location))
                | None -> None
            let diagonal i =
                match getMoveLocation ps i 1 with
                | Some location ->
                    if isTheirs location then None
                    else Some (AtoB (ps.location, location))
                | None -> None
            [forward; diagonal -1; diagonal 1]
            |> List.choose id
        | _ -> []

    state.pieces
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
        let newPieces =
            match state.pieces |> getPieceAt a with
            | Some ps ->
                state.pieces
                |> removePiece a
                |> removePiece b
                |> addPiece ps
                |> Some
            | None -> None
        match newPieces with
        | Some pieces ->
            Some { lastState = Some state
                   moveNumber = moveNumber
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

let getFen (state: GameState) =
    // rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1

    let isDigit c = c >= '0' && c <= '9'

    let rec foldSpace (chars: char list) =
        match chars with
        | a :: b :: tail ->
            if isDigit a && isDigit b then
                foldSpace (char (int a + int b - int '0') :: tail)
            else
                a :: b :: tail
        | a -> a

    let getRow rank =
        [ for x in 0..8 do
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

    let chars =
        [ for y in 0..8 do
              let rank = indexToRank y |> Option.get
              getRow rank ]

    state.pieces
