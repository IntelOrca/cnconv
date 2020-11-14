namespace IntelOrca.Chess.Tests

open IntelOrca.Chess.Types
open IntelOrca.Chess.Notation
open Xunit

type NotationTests() =
    let assertFail msg = failwith msg

    let assertPiece piece (entity: MoveDescriptorEntity) =
        Assert.Equal(Some piece, entity.piece)
        Assert.Equal(MoveDescriptorFile.Any, entity.file)
        Assert.Equal(None, entity.rank)

    let assertPieceFile piece expectedFile (entity: MoveDescriptorEntity) =
        Assert.Equal(Some piece, entity.piece)
        Assert.Equal(expectedFile, entity.file)
        Assert.Equal(None, entity.rank)

    let assertLocation (expectedFile, expectedRank) (entity: MoveDescriptorEntity) =
        Assert.Equal(None, entity.piece)
        Assert.Equal(MoveDescriptorFile.Specifc expectedFile, entity.file)
        Assert.Equal(Some expectedRank, entity.rank)

    let assertFileRank (expectedFile, expectedRank) (entity: MoveDescriptorEntity) =
        Assert.Equal(None, entity.piece)
        Assert.Equal(expectedFile, entity.file)
        Assert.Equal(Some expectedRank, entity.rank)

    let assertFile expectedFile (entity: MoveDescriptorEntity) =
        Assert.Equal(None, entity.piece)
        Assert.Equal(expectedFile, entity.file)
        Assert.Equal(None, entity.rank)

    let assertMove desc =
        match desc with
        | Some (MoveDescriptor.Move (a, b)) -> (a, b)
        | _ -> assertFail "Move expected"

    let assertCapture desc =
        match desc with
        | Some (MoveDescriptor.Capture (a, b)) -> (a, b)
        | _ -> assertFail "Capture expected"

    let assertCastle expected desc =
        match desc with
        | Some (MoveDescriptor.Castle kind) ->
            Assert.Equal(expected, kind)
        | _ -> assertFail "Castle expected"

    let assertEntities assertA assertB (entityA, entityB) =
        assertA entityA
        assertB entityB

    let parseClassic = parseClassicNotation White
    let parseModern = parseModernNotation

    [<Fact>]
    let ``O-O`` () =
        for kind in [Classic; Modern] do
            parseNotation kind White "O-O"
            |> assertCastle CastleKind.KingSide

    [<Fact>]
    let ``O-O-O`` () =
        for kind in [Classic; Modern] do
            parseNotation kind White "O-O-O"
            |> assertCastle CastleKind.QueenSide

    // Modern
    [<Fact>]
    let ``Modern.e4`` () =
        parseModern "e4"
        |> assertMove
        |> assertEntities (assertPiece Piece.Pawn) (assertLocation (File.King, R4))

    [<Fact>]
    let ``Modern.e2e4`` () =
        parseModern "e2e4"
        |> assertMove
        |> assertEntities (assertLocation (File.King, R2)) (assertLocation (File.King, R4))

    [<Fact>]
    let ``Modern.Nc3`` () =
        parseModern "Nc3"
        |> assertMove
        |> assertEntities (assertPiece Piece.Knight) (assertLocation (File.QueenBishop, R3))

    [<Fact>]
    let ``Modern.Bxf6`` () =
        parseModern "Bxf6"
        |> assertCapture
        |> assertEntities (assertPiece Piece.Bishop) (assertLocation (File.KingBishop, R6))

    [<Fact>]
    let ``Modern.gxf6`` () =
        parseModern "gxf6"
        |> assertCapture
        |> assertEntities (assertFile (MoveDescriptorFile.Specifc File.KingKnight)) (assertLocation (File.KingBishop, R6))

    [<Fact>]
    let ``Modern.Nbd2`` () =
        parseModern "Nbd2"
        |> assertMove
        |> assertEntities (assertPieceFile Piece.Knight (MoveDescriptorFile.Specifc File.QueenKnight)) (assertLocation (File.Queen, R2))

    // Classic
    [<Fact>]
    let ``Classic.PxP`` () =
        parseClassic "PxP"
        |> assertCapture
        |> assertEntities (assertPiece Piece.Pawn) (assertPiece Piece.Pawn)

    [<Fact>]
    let ``Classic.P-Q4`` () =
        parseClassic "P-Q4"
        |> assertMove
        |> assertEntities (assertPiece Piece.Pawn) (assertLocation (File.Queen, R4))

    [<Fact>]
    let ``Classic.P-QR4`` () =
        parseClassic "P-QR4"
        |> assertMove
        |> assertEntities (assertPiece Piece.Pawn) (assertLocation (File.QueenRook, R4))

    [<Fact>]
    let ``Classic.P-QN4`` () =
        parseClassic "P-QN4"
        |> assertMove
        |> assertEntities (assertPiece Piece.Pawn) (assertLocation (File.QueenKnight, R4))

    [<Fact>]
    let ``Classic.P-QB4`` () =
        parseClassic "P-QB4"
        |> assertMove
        |> assertEntities (assertPiece Piece.Pawn) (assertLocation (File.QueenBishop, R4))

    [<Fact>]
    let ``Classic.P-K4`` () =
        parseClassic "P-K4"
        |> assertMove
        |> assertEntities (assertPiece Piece.Pawn) (assertLocation (File.King, R4))

    [<Fact>]
    let ``Classic.P-KR4`` () =
        parseClassic "P-KR4"
        |> assertMove
        |> assertEntities (assertPiece Piece.Pawn) (assertLocation (File.KingRook, R4))

    [<Fact>]
    let ``Classic.P-KB4`` () =
        parseClassic "P-KB4"
        |> assertMove
        |> assertEntities (assertPiece Piece.Pawn) (assertLocation (File.KingBishop, R4))

    [<Fact>]
    let ``Classic.P-KN4`` () =
        parseClassic "P-KN4"
        |> assertMove
        |> assertEntities (assertPiece Piece.Pawn) (assertLocation (File.KingKnight, R4))

    [<Fact>]
    let ``Classic.N-B3`` () =
        parseClassic "N-B3"
        |> assertMove
        |> assertEntities (assertPiece Piece.Knight) (assertFileRank (MoveDescriptorFile.AnyBishop, R3))

    [<Fact>]
    let ``Classic.R-R6`` () =
        parseClassic "R-R6"
        |> assertMove
        |> assertEntities (assertPiece Piece.Rook) (assertFileRank (MoveDescriptorFile.AnyRook, R6))

    [<Fact>]
    let ``Classic.QR-B1`` () =
        parseClassic "QR-B1"
        |> assertMove
        |> assertEntities (assertPieceFile Piece.Rook (MoveDescriptorFile.Specifc File.QueenRook)) (assertFileRank (MoveDescriptorFile.AnyBishop, R1))
