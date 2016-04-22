package agents

import service.{BaseShape, Board, Color, Move}

/**
  * Created by runger on 4/21/16.
  */
object AgentTools {

  def allMoves(color: Color, baseShapes: Set[BaseShape], board: Board) = {
    val myPieces = baseShapes.flatMap(sh => sh.allOrientations(color))

    val myMoves = myPieces.flatMap(piece => {
      piece.matrix.allPositions.flatMap(offset => {
        board.emptyCorners(color).map(corner => {
          val pos = corner.unOffset(offset)
          val move = Move(pos, piece)
          move
        })
      })
    })

    val validMoves = myMoves.filter(move => board.canPlace(move))
    validMoves
  }

}
