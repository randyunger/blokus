package service

import org.scalatest.FunSuite

/**
 * Created by runger on 3/10/16.
 */
class BoardTest extends FunSuite {

  test("Squares are empty") {
    val board = Board("-X\n-X")
    val piece = Piece("R\nR", Color('R'))
    assert(board.allOverlapSquaresAreEmpty(Move(Position(0,0), piece)))
  }

  test("Squares are not empty") {
    val board = Board("-X\n-X")
    val piece = Piece("R\nR", Color('R'))
    assert(!board.allOverlapSquaresAreEmpty(Move(Position(0,1), piece)))
  }

  test("corner matches color - false") {
    val board = Board("R--\n---\n---")
    val piece = Piece("X", Color('R'))

    val overlap = Move(Position(0,0), piece)
    assert(false == board.cornerMatchesColor(overlap))

    val right = Move(Position(0,1), piece)
    assert(false == board.cornerMatchesColor(right))

    val below = Move(Position(1,0), piece)
    assert(false == board.cornerMatchesColor(below))
  }

  test("corner matches color - true") {
    val board = Board("R--\n---\n---")
    val piece = Piece("X", Color('R'))

    val bottomRight = Move(Position(1,1), piece)
    assert(true == board.cornerMatchesColor(bottomRight))
  }

  test("edge adjacency"){
    val board = Board("RR---\n-R---\n-----")
    val piece = Piece("XX", Color('R'))

    val invalid = Move(Position(0,2), piece)
    assert(false == board.noEdgeAdjacency(invalid))
  }

//  test("edge adjacency"){
//    val board = Board("-GG--\n--G--\n--G--\n-G-GG\n-G-GG")
//    val piece = Piece("XX\nX-", Color('R'))
//
//    val invalid = Move(Position(0,2), piece)
//    assert(false == board.noEdgeAdjacency(invalid))
//  }

  test("edge adjacency 5x5 left"){
      val board = Board("-----\n-----\nG----\nG----\nG----")
      val piece = Piece("X\nX", Color('G'))

      val invalid = Move(Position(1,1), piece)
      assert(false == board.noEdgeAdjacency(invalid))

//      val b2 = board.place(invalid).toOption.get
//      println(b2.matrix.stringRep)
    }

  test("show options") {
    val board = Board("-----\n-----\n--G--\n-----\n-----")
//    val piece = Piece("X\nX", Color('G'))
    val shape = BaseShape("XX-\n-XX")

    val validMoves = for {
      piece <- shape.allOrientations(Color('G'))
      pos <- board.matrix.allPositions
      move = Move(pos, piece)
      if board.canPlace(move)
    } yield move

    validMoves.foreach(m => {
      val nb = board.place(m).toOption.get
      println(nb.matrix.stringRep)
      println()
    })
  }

  ignore("Empty corners"){
    var board = Board("--G--\n--G--\nGGG--\n--G--\n-----")
//    val corners = board.emptyCorners(Color('G'))
//    corners.foreach(cornerPos => {
//      val move = Move(cornerPos, Piece("G", Color('G')))
//      val newBoard = board.place(move)
//      board = newBoard.toOption.get
//    })
    println(board.matrix.stringRep)
  }
}
