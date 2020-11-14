namespace IntelOrca.Chess.Tests

open System
open IntelOrca.Chess.Types
open IntelOrca.Chess.Board
open IntelOrca.Chess.Notation
open Xunit

type GameStateTests() =
    let parseFQM (notation: string) =
        if notation = "O-O" then Castle (KingSide)
        elif notation = "O-O-O" then Castle (QueenSide)
        else
            let src = notation.Substring(0, 2)
            let dst = notation.Substring(2, 2)
            let src = getLocationFromNotation src |> Option.get
            let dst = getLocationFromNotation dst |> Option.get
            AtoB (src, dst)

    let move (notation: string) (state: GameState) =
        let fqm = parseFQM notation
        state
        |> doMove fqm
        |> Option.get

    let assertFen (expected: string) (state: GameState) =
        Assert.Equal(expected, state |> getFen)
        state

    let assertPgn (expected: string) (state: GameState) =
        Assert.Equal(expected, state |> getPgn)
        state

    [<Fact>]
    let ``initial board`` () =
        getInitialBoard
        |> assertFen "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"

    [<Fact>]
    let ``e4`` () =
        getInitialBoard
        |> move "e2e4"
        |> assertFen "rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR b KQkq - 0 1"
        |> assertPgn "1. e4"

    [<Fact>]
    let ``e4 e5`` () =
        getInitialBoard
        |> move "e2e4"
        |> move "e7e5"
        |> assertFen "rnbqkbnr/pppp1ppp/8/4p3/4P3/8/PPPP1PPP/RNBQKBNR w KQkq - 0 2"
        |> assertPgn "1. e4 e5"

    [<Fact>]
    let ``e4 e5 Nc3`` () =
        getInitialBoard
        |> move "e2e4"
        |> move "e7e5"
        |> move "b1c3"
        |> assertFen "rnbqkbnr/pppp1ppp/8/4p3/4P3/2N5/PPPP1PPP/R1BQKBNR b KQkq - 1 2"
        |> assertPgn "1. e4 e5 2. Nc3"

    [<Fact>]
    let ``d4 d5 c4 dxc4`` () =
        getInitialBoard
        |> move "d2d4"
        |> move "d7d5"
        |> move "c2c4"
        |> move "d5c4"
        |> assertFen "rnbqkbnr/ppp1pppp/8/8/2pP4/8/PP2PPPP/RNBQKBNR w KQkq - 0 3"
        |> assertPgn "1. d4 d5 2. c4 dxc4"

    let assertDeriveMove s expected state =
        let md =
            parseModernNotation s
            |> Option.get
        let expectedMove = parseFQM expected
        let move = deriveMove md state
        Assert.Equal(Some expectedMove, move)

    [<Fact>]
    let ``deriveMove e4`` () =
        assertDeriveMove "e4" "e2e4" getInitialBoard

    [<Fact>]
    let ``classic moves`` () =
        let getColour i =
            if i &&& 1 = 0 then White
            else Black
        let classicMoves =
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
        getInitialBoard
        |> doMoves classicMoves
        |> Option.get
        |> assertPgn "1. d4 Nf6 2. c4 g6 3. Nf3 Bg7 4. e3 O-O 5. Nc3 d6 6. Bd3 Nbd7 7. b3 c5 8. Bb2 a6 9. O-O Re8 10. d5 e6 11. Rb1 exd5 12. Nxd5 b5 13. Nxf6 Nxf6 14. cxb5 Qa5 15. a4 axb5 16. Bxb5 Re7 17. Qxd6"
        |> assertFen "r1b3k1/4rpbp/3Q1np1/qBp5/P7/1P2PN2/1B3PPP/1R3RK1 b - - 0 17"
