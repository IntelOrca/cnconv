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

    let assertLocation (expectedFile, expectedRank) (entity: MoveDescriptorEntity) =
        Assert.Equal(None, entity.piece)
        Assert.Equal(MoveDescriptorFile.Specifc expectedFile, entity.file)
        Assert.Equal(Some expectedRank, entity.rank)

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

    [<Fact>]
    let ``O-O`` () =
        parseMove "O-O"
        |> assertCastle CastleKind.KingSide

    [<Fact>]
    let ``O-O-O`` () =
        parseMove "O-O-O"
        |> assertCastle CastleKind.QueenSide

    [<Fact>]
    let ``e4`` () =
        parseMove "e4"
        |> assertMove
        |> assertEntities (assertPiece Piece.Pawn) (assertLocation (File.King, R4))

    [<Fact>]
    let ``PxP`` () =
        parseMove "PxP"
        |> assertCapture
        |> assertEntities (assertPiece Piece.Pawn) (assertPiece Piece.Pawn)
