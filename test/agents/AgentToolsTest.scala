package agents

import org.scalatest.FunSuite
import service.{BaseShape, Board, Color}

import scalaz.{-\/, \/-}

/**
  * Created by runger on 4/21/16.
  */
class AgentToolsTest extends FunSuite {

  test("all moves empty board") {
    val board = Board("-----\n-----\n-----\n-----\n-----")
    val shapes = Set(BaseShape("--X\nXXX"))

    val allMoves = AgentTools.allMoves(Color('X'), shapes, board)

    allMoves.foreach(move => {
      board.place(move) match {
        case -\/(prob) => {
//          println(prob)
//          assert(false)
        }
        case \/-(newBoard) => {
          println(newBoard.matrix.stringRep)
          println("\n")
        }
      }
    })

  }

}
