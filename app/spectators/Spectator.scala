package spectators

import java.util.UUID
import java.util.concurrent.ConcurrentHashMap

import play.twirl.api.Html
import service.{Tray, Board, GameConfig, LiveGame}

import scala.collection.mutable

/**
 * Created by runger on 3/12/16.
 */

object Bleachers {
  val prodInstance = new Bleachers()
  def apply() = prodInstance
}

class Bleachers() {
  val spectators = new ConcurrentHashMap[UUID, Spectator]

  def getOrCreateSpectator(id: UUID, gameConfig: GameConfig) = {
    Option(spectators.get(id)).getOrElse{
      val newSpec = Spectator(gameConfig)
      spectators.put(id, newSpec)
    }
  }

  //if no games yet, just returns a default
  def getOneOrDefault = {
    val keys = spectators.keySet()
    try {
      keys.toArray.apply(0).asInstanceOf[Spectator]
    } catch {
      case ex: Exception => Spectator(GameConfig.example)
    }
  }
}

case class Spectator(gameConfig: GameConfig) {
  val history = mutable.Buffer.empty[LiveGame]

  def observe(liveGame: LiveGame) = {
    history.append(liveGame)
  }

  def renderBoard(): Html = {
    val board = history.lastOption.map(_.gameState.board).getOrElse(Board(gameConfig))
    views.html.board(board)
  }

  def renderTray(): Html = {
    val tray = history.lastOption.map(_.gameState.tray).getOrElse(Tray.build(gameConfig))
    views.html.tray(tray)
  }

  def renderConfig(): Html = {
    views.html.config(gameConfig)
  }
}