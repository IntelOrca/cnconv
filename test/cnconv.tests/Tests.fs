namespace IntelOrca.Chess.Tests

open System
open IntelOrca.Chess.Types
open IntelOrca.Chess.Board
open Xunit

type GameStateTests() =
    let move (notation: string) (state: GameState) =
        let src = notation.Substring(0, 2)
        let dst = notation.Substring(2, 2)
        let src = getLocationFromNotation src |> Option.get
        let dst = getLocationFromNotation dst |> Option.get
        state
        |> doMove (AtoB (src, dst))
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
