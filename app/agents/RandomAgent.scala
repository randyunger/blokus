package agents

import service._

import scalaz.{-\/, \/, \/-}

/**
 * Created by runger on 3/9/16.
 */

class RandomAgent {

  def play(myPlayer: Player, liveGame: GameSnapshot): Pass.type \/ Move = {
    val myShapes = liveGame.gameState.tray.shapesFor(myPlayer)._2
    val myPieces = myShapes.map(shape => shape.allOrientations(myPlayer.color))
    val validMoves = for {
      piece <- myPieces.flatten
      x <- 0 to liveGame.config.boardDimensions.x
      y <- 0 to liveGame.config.boardDimensions.y
      position = Position(x, y)
      move = Move(position, piece)
      if liveGame.gameState.board.canPlace(move)
    } yield move
    validMoves.headOption match {
      case None => -\/(Pass)
      case Some(move) => \/-(move)
    }
  }

}

class RandyAgent extends RandomAgent {
  override def play(myPlayer: Player, liveGame: GameSnapshot): Pass.type \/ Move = {
    val myShapes = liveGame.gameState.tray.shapesFor(myPlayer)._2
    val sortedShapes = myShapes.toList.sortBy(sh => -1 * sh.countFullSquares)
    val myPieces = sortedShapes.map(shape => shape.allOrientations(myPlayer.color))

    val validMoves = for {
      piece <- myPieces.flatten
      x <- 0 to liveGame.config.boardDimensions.x
      y <- 0 to liveGame.config.boardDimensions.y
      position = Position(x, y)
      move = Move(position, piece)
      if liveGame.gameState.board.canPlace(move)
    } yield move
    validMoves.headOption match {
      case None => -\/(Pass)
      case Some(move) => \/-(move)
    }
  }
}
