namespace IntelOrca.Chess.Tests

open IntelOrca.Chess.Types
open IntelOrca.Chess.Board
open IntelOrca.Chess.Notation
open Xunit

type PgnTests() =
    let parsePgn pgn =
        match Pgn.parse pgn with
        | Ok pgn -> pgn
        | Error err -> failwith err

    let assertTag key expectedValue pgn =
        Assert.Equal(Pgn.getTag key pgn, Some expectedValue)
        pgn

    let assertFen fen pgn =
        getInitialBoard
        |> doMoves (Pgn.getMoves pgn)
        |> Option.get
        |> getFen

    [<Fact>]
    let ``Fischer vs Spassky`` () =
        let pgn = @"
            [Event ""F/S Return Match""]
            [Site ""Belgrade, Serbia JUG""]
            [Date ""1992.11.04""]
            [Round ""29""]
            [White ""Fischer, Robert J.""]
            [Black ""Spassky, Boris V.""]
            [Result ""1/2-1/2""]

            1. e4 e5 2. Nf3 Nc6 3. Bb5 a6 {This opening is called the Ruy Lopez.}
            4. Ba4 Nf6 5. O-O Be7 6. Re1 b5 7. Bb3 d6 8. c3 O-O 9. h3 Nb8 10. d4 Nbd7
            11. c4 c6 12. cxb5 axb5 13. Nc3 Bb7 14. Bg5 b4 15. Nb1 h6 16. Bh4 c5 17. dxe5
            Nxe4 18. Bxe7 Qxe7 19. exd6 Qf6 20. Nbd2 Nxd6 21. Nc4 Nxc4 22. Bxc4 Nb6
            23. Ne5 Rae8 24. Bxf7+ Rxf7 25. Nxf7 Rxe1+ 26. Qxe1 Kxf7 27. Qe3 Qg5 28. Qxg5
            hxg5 29. b3 Ke6 30. a3 Kd6 31. axb4 cxb4 32. Ra5 Nd5 33. f3 Bc8 34. Kf2 Bf5
            35. Ra7 g6 36. Ra6+ Kc5 37. Ke1 Nf4 38. g3 Nxh3 39. Kd2 Kb5 40. Rd6 Kc5 41. Ra6
            Nf2 42. g4 Bd3 43. Re6 1/2-1/2"

        parsePgn pgn
        |> assertTag "Event" "F/S Return Match"
        |> assertTag "Site" "Belgrade, Serbia JUG"
        |> assertTag "Date" "1992.11.04"
        |> assertTag "Round" "29"
        |> assertTag "White" "Fischer, Robert J."
        |> assertTag "Black" "Spassky, Boris V."
        |> assertTag "Result" "1/2-1/2"
        |> assertFen "8/8/4R1p1/2k3p1/1p4P1/1P1b1P2/3K1n2/8 b - - 2 43"

    [<Fact>]
    let ``Ted vs Alex`` () =
        let pgn =
            @"
            [Event ""Casual Correspondence game""]
            [Site ""https://lichess.org/44WRflx0""]
            [Date ""2020.08.31""]
            [White ""coffeecoder""]
            [Black ""ElvinDrude""]
            [Result ""1-0""]
            [UTCDate ""2020.08.31""]
            [UTCTime ""15:26:26""]
            [WhiteElo ""1500""]
            [BlackElo ""1500""]
            [Variant ""Standard""]
            [TimeControl ""-""]
            [ECO ""A04""]
            [Opening ""Zukertort Opening: Slav Invitation""]
            [Termination ""Normal""]
            [Annotator ""lichess.org""]

            1. Nf3 c6 { A04 Zukertort Opening: Slav Invitation }
            2. d4 d5 3. Bg5 h6 4. Bf4 Nf6 5. e3 Bf5 6. Nbd2 e6
            7. Ne5 c5?! { (-0.08 → 0.73) Inaccuracy. Nbd7 was best. } (7... Nbd7 8. Be2 Be7 9. O-O Nxe5 10. Bxe5 O-O 11. c4 Nd7 12. Bg3 Qb6 13. Qb3 Qxb3 14. Nxb3)
            8. Bb5+ Nbd7 9. c3?! { (0.13 → -0.38) Inaccuracy. O-O was best. } (9. O-O a6) 9... a6 10. Bxd7+ Nxd7 11. Ndf3 cxd4
            12. Nxd4 Be4 13. Nxe6?? { (-0.34 → -3.13) Blunder. Qh5 was best. } (13. Qh5) 13... fxe6 14. Qh5+ g6
            15. Nxg6? { (-3.42 → -6.25) Mistake. Qe2 was best. } (15. Qe2 Bg7) 15... Rg8?? { (-6.25 → 0.00) Blunder. Nf6 was best. } (15... Nf6 16. Qh3 Bxg6 17. Qxe6+ Qe7
            18. Qb6 Rd8 19. O-O Bg7 20. Rad1 O-O 21. Bg3 Rc8 22. Rfe1) 16. Nh8+? { (0.00 → -1.07) Mistake. Nxf8+ was best. } (16. Nxf8+ Kxf8 17. Bxh6+ Ke7 18. Bg5+ Nf6
            19. f3 Qb6 20. Bxf6+ Kxf6 21. fxe4 Qxe3+ 22. Qe2 Qxe2+) 16... Ke7?? { (-1.07 → Mate in 1) Checkmate is now unavoidable. Bg6 was best. } (16... Bg6 17. Nxg6 Nf6 18. Qe5 Rxg6
            19. Qxe6+ Qe7 20. Qxe7+ Bxe7 21. O-O Kf7 22. Rad1 Ke6 23. g3) 17. Qf7# { White wins by checkmate. } 1-0"

        parsePgn pgn
        |> assertTag "White" "coffeecoder"
        |> assertTag "Black" "ElvinDrude"
        |> assertFen "r2q1brN/1p1nkQ2/p3p2p/3p4/4bB2/2P1P3/PP3PPP/R3K2R b KQ - 4 17"
