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

}
