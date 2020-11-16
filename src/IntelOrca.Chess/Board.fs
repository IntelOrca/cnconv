module IntelOrca.Chess.Board

open Types
open System.Text

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
                  |> List.tryFind (fun x -> x.location = (file, rank))
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
    let castlingAbility =
        let ca = state.castleAvailability
        let s =
            [if ca.whiteKing then "K" else ""
             if ca.whiteQueen then "Q" else ""
             if ca.blackKing then "k" else ""
             if ca.blackQueen then "q" else ""]
            |> String.concat ""
        if s = "" then "-" else s
    let enpassant = "-"
    let halfmove = string state.halfMoveClock
    let fullmove = string state.moveNumber

    // rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1
    seq { placement; toMove; castlingAbility; enpassant; halfmove; fullmove }
    |> String.concat " "

let fromFen (fen: string): GameState option =
    let parsePieces (s: string): PieceState list option =
        let parseRow (s: string) rank =
            let (|Digit|_|) c =
                if System.Char.IsDigit(c) then Some (int c - int '0')
                else None

            let parsePieceChar = function
                | 'P' -> Some (White, Piece.Pawn)
                | 'N' -> Some (White, Piece.Knight)
                | 'B' -> Some (White, Piece.Bishop)
                | 'R' -> Some (White, Piece.Rook)
                | 'Q' -> Some (White, Piece.Queen)
                | 'K' -> Some (White, Piece.King)
                | 'p' -> Some (Black, Piece.Pawn)
                | 'n' -> Some (Black, Piece.Knight)
                | 'b' -> Some (Black, Piece.Bishop)
                | 'r' -> Some (Black, Piece.Rook)
                | 'q' -> Some (Black, Piece.Queen)
                | 'k' -> Some (Black, Piece.King)
                | _ -> None

            let rec parseRow pieces fileIndex chars =
                match indexToFile fileIndex with
                | Some file ->
                    match chars with
                    | Digit d :: tail ->
                        parseRow pieces (fileIndex + d) tail
                    | head :: tail ->
                        match parsePieceChar head with
                        | Some (colour, piece) ->
                            let ps = 
                                { colour = colour
                                  piece = piece
                                  location = (file, rank) }
                            parseRow (ps :: pieces) (fileIndex + 1) tail
                        | None -> None
                    | [] -> Some pieces
                | None -> Some pieces

            parseRow [] 0 (s.ToCharArray() |> Array.toList)

        match s.Split('/') |> Array.toList with
        | rows when List.length rows = 8 ->
            let pieces =
                rows
                |> List.mapi (fun i r -> parseRow r (indexToRank (7 - i) |> Option.get))
                |> List.choose id
            match pieces with
            | pieces when List.length pieces = 8 -> Some (pieces |> List.collect id)
            | _ -> None
        | _ -> None

    let parseToMove = function
        | "w" -> Some White
        | "b" -> Some Black
        | _ -> None

    let parseCastleAvailability (s: string) =
        Some {  whiteKing = s.Contains("K")
                whiteQueen = s.Contains("Q")
                blackKing = s.Contains("k")
                blackQueen = s.Contains("q") }

    let parseInt s =
        let mutable r = byte 0
        if System.Byte.TryParse(s, &r) then Some (int r)
        else None

    let parts =
        fen.Split([|' '|], System.StringSplitOptions.RemoveEmptyEntries)
        |> Array.toList
    match parts with
    | board :: toMove :: castleAvailability :: enPassant :: halfMove :: fullMove :: _ ->
        match (parsePieces board, parseToMove toMove, parseCastleAvailability castleAvailability, parseInt halfMove, parseInt fullMove) with
        | (Some pieces, Some toMove, Some castleAvailability, Some halfMove, Some fullMove) ->
            Some  { previous = None
                    moveNumber = fullMove
                    halfMoveClock = halfMove
                    toMove = toMove
                    pieces = pieces
                    castleAvailability = castleAvailability }
        | _ -> None
    | _ -> None

let getNotation (move: PossibleMove) =
    match move with
    | AtoB (a, b) -> (getNotationForLocation a) + (getNotationForLocation b)
    | Castle (KingSide) -> "O-O"
    | Castle (QueenSide) -> "O-O-O"

let createPieceState colour piece file rank =
    { colour = colour
      piece = piece
      location = (file, rank) }

let getInitialBoard =
    { previous = None
      moveNumber = 1
      halfMoveClock = 0
      toMove = White
      castleAvailability = {
          whiteKing = true
          whiteQueen = true
          blackKing = true
          blackQueen = true
      }
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
                if Option.isSome forward then
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
                else
                    None
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

let getSource (move: PossibleMove) =
    match move with
    | AtoB (a, _) -> a
    | _ -> (File.QueenRook, R1)

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

let castlePieces kind colour pieces =
    let (cornerFile, kingFile, rookFile) =
        match kind with
        | CastleKind.KingSide -> (File.KingRook, File.KingKnight, File.KingBishop)
        | CastleKind.QueenSide -> (File.QueenRook, File.QueenBishop, File.Queen)
    let rank =
        match colour with
        | White -> R1
        | Black -> R8
    pieces
    |> removePiece (cornerFile, rank)
    |> removePiece (File.King, rank)
    |> addPiece { colour = colour; piece = Piece.King; location = (kingFile, rank) }
    |> addPiece { colour = colour; piece = Piece.Rook; location = (rookFile, rank) }

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
                   pieces = pieces
                   castleAvailability = state.castleAvailability }
        | None -> None
    | Castle kind ->
        let newPieces = castlePieces kind state.toMove state.pieces
        let castleAvailability =
            match state.toMove with
            | White ->
                { state.castleAvailability with
                    whiteKing = false 
                    whiteQueen = false }
            | Black ->
                { state.castleAvailability with
                    blackKing = false 
                    blackQueen = false }
        Some { previous = Some (state, move)
               moveNumber = moveNumber
               halfMoveClock = state.halfMoveClock + 1
               toMove = toMove
               pieces = newPieces
               castleAvailability = castleAvailability }

/// Convert a move descriptor to a fully qualified move based on the given game state.
let deriveMoves (move: MoveDescriptor) (state: GameState): PossibleMove list =
    let possibleMoves = getPossibleMoves state

    let filterMoves filters =
        let folder moves filter =
            moves
            |> List.where (fun move -> filter move)
        List.fold folder possibleMoves filters

    match move with
    | Move (a, b)
    | Capture (a, b) ->
        let sourcePieceFilter m =
            match a.piece with
            | Some piece -> Some piece = getPiece state m
            | None -> true
        let fileFilter pmf mdf =
            match mdf with
            | MoveDescriptorFile.Specifc file -> file = pmf
            | MoveDescriptorFile.AnyKnight -> pmf = File.KingKnight || pmf = File.QueenKnight
            | MoveDescriptorFile.AnyBishop -> pmf = File.KingBishop || pmf = File.QueenBishop
            | MoveDescriptorFile.AnyRook -> pmf = File.KingRook || pmf = File.QueenRook
            | _ -> true
        let sourceFileFilter m = fileFilter (getSource m |> fst) a.file

        let destinationPieceFilter m =
            match b.piece with
            | Some piece ->
                match getPieceAt (getDestination m) state.pieces with
                | Some ps -> ps.piece = piece
                | None -> false
            | None -> true
        let destinationFileFilter m = fileFilter (getDestination m |> fst) b.file
        let destinationRankFilter m =
            match b.rank with
            | Some rank -> rank = (getDestination m |> snd)
            | None -> true

        let filters = [sourcePieceFilter; sourceFileFilter;
                        destinationPieceFilter; destinationFileFilter; destinationRankFilter]
        filterMoves filters
    | MoveDescriptor.Castle kind ->
        [Castle kind]

let deriveMove move state =
    deriveMoves move state
    |> List.tryExactlyOne

let rec deriveMostLikelyState (moves: MoveDescriptor list) (state: GameState) =
    let getGameStateLength state =
        let rec getGameStateLength len state =
            match state with
            | Some (state, _) -> getGameStateLength (len + 1) state.previous
            | None -> len
        getGameStateLength 0 state.previous
    match moves with
    | move :: tail ->
        let result =
            deriveMoves move state
            |> List.choose (fun m -> doMove m state)
            |> List.collect (deriveMostLikelyState tail)
        // match result with
        // | [] -> [state]
        // | other -> other
        match result with
        | [] -> [state]
        | other -> [List.maxBy getGameStateLength other]
    | [] -> [state]

let doMoves (moves: MoveDescriptor list) (state: GameState) =
    let fn state move =
        match state with
        | Some state ->
            match deriveMove move state with
            | Some derivedMove -> doMove derivedMove state
            | None -> None
        | None -> None
    List.fold fn (Some state) moves

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
