package service

import java.util.concurrent.TimeoutException

import com.fasterxml.jackson.core.JsonParseException
import play.Logger
import play.api.libs.json.{JsError, JsSuccess, Json}
import play.api.libs.ws.WS
import scala.annotation.tailrec
import scala.collection.mutable
import scala.concurrent.Await
import scala.concurrent.duration._
import play.api.Play.current

import scalaz.{\/, \/-, -\/}

/**
 * Created by runger on 3/8/16.
 */

object GameRunner {
  val prodInstance = new GameRunner
  def apply() = prodInstance
}

class GameRunner {

  @tailrec
  final def proceed(liveGame: LiveGame): Unit = {
    println("Board state:")
    println(liveGame.gameState.board.matrix.stringRep)
    println()
    println(liveGame.gameState.tray)

    //Main loop
    if(!isOver(liveGame)) {
      val nextState = getNextState(liveGame)
      proceed(liveGame.copy(gameState = nextState))
      
    } else {
      val scores = tallyScores(liveGame)
      sendGameOver(scores, liveGame.config.players)
    }
  }

  def getNextState(liveGame: LiveGame) = {
    val currentPlayer = getNextPlayer(liveGame)
    Logger.info(s"Next player is ${currentPlayer.name}")
    val nextMoveOrPassO = requestMove(currentPlayer, liveGame)

    val (updatedBoard, playedShapes, newDqs, newBonuses) =
      nextMoveOrPassO match {
        case -\/(-\/(problemGettingMove)) => {
          Logger.warn(s"Problem getting move: $problemGettingMove")
          dq(problemGettingMove, currentPlayer, liveGame.gameState.board)
        }
        case -\/(\/-(pass)) => {
          Logger.warn(s"Player passed: $currentPlayer")
          (liveGame.gameState.board, Set.empty[BaseShape], Set(currentPlayer), Set.empty[Bonus])
        }
        case \/-(playedMove) => {
          Logger.info(s"${currentPlayer.name} is playing $playedMove")
          shapeFromTray(liveGame.gameState.tray, currentPlayer, playedMove.piece) match {
            case -\/(badPiece) => {
              Logger.warn(s"Shape not in tray!: $playedMove")
              dq(IllegalShape(), currentPlayer, liveGame.gameState.board)
            }
            case \/-(playedShape) => {
              liveGame.gameState.board.place(playedMove) match {
                case -\/(illegalMove) => {
                  Logger.warn(s"Problem with move content: $illegalMove")
                  dq(illegalMove, currentPlayer, liveGame.gameState.board)
                }
                case \/-(newBoard) => {
                  Logger.info(s"Successfully applied move to board: $playedMove")
                  val bonus = checkBonuses(currentPlayer, playedShape, liveGame)
                  (newBoard, Set(playedShape), Set.empty[Player], bonus)
                }
              }
            }
          }
      }
    }
    val newTray = liveGame.gameState.tray.discard(currentPlayer, playedShapes)
    val newBench = liveGame.gameState.bench.add(newDqs)
    val newBonusBox = liveGame.gameState.bonusBox.add(currentPlayer, newBonuses)
    GameState(updatedBoard, newTray, newBench, newBonusBox)
  }

  def dq(problem: Problem, player: Player, board: Board): (Board, Set[BaseShape], Set[Player], Set[Bonus]) = {
    sendDq()
    //Just use the old board, player is disqualified!
    (board, Set.empty, Set(player), Set.empty)
  }

  def isOver(liveGame: LiveGame) = liveGame.gameState.bench.isFull(liveGame.config)

  def getNextPlayer(liveGame: LiveGame): Player = {
    val active = liveGame.activePlayers

    //Sort by most pieces
    val bpc = liveGame.gameState.tray.playersByPieceCount.reverse.filter {
      case(pl, sh) => active.contains(pl)
    }

    //Get most piece count
    val mostPieces = bpc.head._2.size

    //Take while equal to that count
    val tiedByPieceCount = bpc.takeWhile(_._2.size == mostPieces)

    //Sort list according to config
    val order = tiedByPieceCount.sortBy(pl => liveGame.config.players.zipWithIndex.find {
      case (configPl, _) => pl._1 == configPl
    }.map(_._2))

    order.head._1
  }

  def shapeFromTray(tray: Tray, player: Player, piece: Piece): Problem \/ BaseShape = {
    val remainingShapes = tray.shapesFor(player)._2
    piece.baseShapeFrom(remainingShapes) match {
      case None => -\/(IllegalShape())
      case Some(sh) => \/-(sh)
    }
  }

  def sendGameOver(score: Score, players: List[Player]): Unit = {
    Logger.info("Game Over!")
    Logger.info(score.toString)
  }

  def checkBonuses(player: Player, shape: BaseShape, liveGame: LiveGame): Set[Bonus] = {
    val bonuses = mutable.Set.empty[Bonus]
    val pieceCount = liveGame.gameState.tray.pieceCountForPlayer(player)
    if(pieceCount == 1) { //One because we're looking at the tray PRIOR to this move being applied.
      bonuses += Bonus.usedAllPieces
      if(shape == BaseShape.monomino)
        bonuses += Bonus.monominoLast
    }
    bonuses.toSet
  }

  def tallyScores(liveGame: LiveGame): Score = {
    def countSquares(board: Board, player: Player): Int = board.countSquaresFor(player.color)
    def bonuses(bonusBox: BonusBox, player: Player): Int = bonusBox.tally(player)
    def score(player: Player) = {
      countSquares(liveGame.gameState.board, player) +
        bonuses(liveGame.gameState.bonusBox, player)  //todo: Bonus not being applied properly
    }

    val players = liveGame.config.players
    val scores = players.map(player => player -> score(player))

    Score(scores)
  }

  def requestMove(player: Player, liveGame: LiveGame): Problem \/ Pass.type \/ Move = {
    val url = player.callback
    Logger.info(s"requesting move from: ${url.toString}")
    val w = WS.url(url.toString)

    val moveRequest = MoveRequest(player, liveGame)
    val gameJson = Json.toJson(moveRequest)

    val r = w.post(gameJson)
    val problemOrMove = try {
      val res = Await.result(r, liveGame.config.timeout millis)
      val jsonStr = res.body
      val moveJson = Json.parse(jsonStr)
      Json.fromJson[Move](moveJson) match {
        case JsError(_) => {
          Json.fromJson[Pass.type](moveJson) match {
            case JsError(_) => -\/(-\/(ParseProblem(jsonStr)))
            case JsSuccess(pass, _) => -\/(\/-(pass))
          }
        }
        case JsSuccess(move, _) => \/-(move)
      }
    } catch {
      case j: JsonParseException => -\/(-\/(ParseProblem(j.getLocalizedMessage)))
      case t: TimeoutException => -\/(-\/(MoveTimedOut()))
    }

    problemOrMove
  }

  def sendDq(): Unit = {}

}
