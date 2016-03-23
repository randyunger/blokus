package spectators

import java.util.UUID
import java.util.concurrent.ConcurrentHashMap

import org.joda.time.DateTime
import play.twirl.api.Html
import service._

import scala.collection.JavaConversions._
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

  def getSpectator(id: String) = {
    val uuid = UUID.fromString(id)
    val spO = Option(spectators.get(uuid))
    spO
  }

  def getOrCreateSpectator(id: UUID, gameConfig: GameConfig): Spectator = {
    Option(spectators.get(id)).getOrElse{
      val newSpec = Spectator(gameConfig)
      spectators.put(id, newSpec)
      newSpec
    }
  }

  //if no games yet, just returns a default
  def getOneOrDefault = {
    val keys = spectators.keySet()
    try {
      keys.toArray.apply(0).asInstanceOf[Spectator]
    } catch {
      case ex: Exception =>
        //Todo: better default
        Spectator(GameConfig.example)
    }
  }

  val future = new DateTime(3000, 1,1,1,1)

  def renderGameList(): Html = {
    val list = spectators.entrySet().toList.map(e => e.getKey -> e.getValue)
    val byStartTime = list.sortBy(_._2.firstSnapshot.map(_.time.getMillis).getOrElse(-1l))
    val idsByStart = byStartTime.map{case(id, sp) => id -> sp.lastSnapshot.map(_.time).getOrElse(future)}
    views.html.gameList(idsByStart)
  }
}

case class SpectatedSnapshot(snapshot: GameSnapshot, time: DateTime)

case class Spectator(gameConfig: GameConfig) {
  val history = mutable.Buffer.empty[SpectatedSnapshot]

  def firstSnapshot = history.sortBy(_.time.getMillis).headOption
  def lastSnapshot = history.sortBy(_.time.getMillis).lastOption

  def observe(liveGame: GameSnapshot) = {
    val ss = SpectatedSnapshot(liveGame, new DateTime())
    history.append(ss)
  }

  def renderBoard(ix: Int): Html = {
//    val board = history.lastOption.map(_.snapshot.gameState.board).getOrElse(Board(gameConfig))
    val board = history.get(ix).snapshot.gameState.board
    views.html.board(board)
  }


  def renderStartBoard(): Html = {
    val board = Board(gameConfig)
    views.html.board(board)
  }

  def renderTray(ix: Int): Html = {
//    val tray = history.lastOption.map(_.snapshot.gameState.tray).getOrElse(Tray.build(gameConfig))
    val tray = history.get(ix).snapshot.gameState.tray
    views.html.tray(tray)
  }

  def renderStartTray(): Html = {
    //    val tray = history.lastOption.map(_.snapshot.gameState.tray).getOrElse(Tray.build(gameConfig))
//    val tray = history.get(ix).snapshot.gameState.tray
    val tray = Tray.build(gameConfig)
    views.html.tray(tray)
  }

  def renderConfig(): Html = {
    views.html.config(gameConfig)
  }

  def renderControls(ix: Int): Html = {
    val dc = DisplayControls(ix, history.size-1)
    views.html.controls(dc)
  }

  def renderBlankControls(): Html = {
    views.html.controls(DisplayControls(0,0))
  }


}

case class DisplayControls(currIx: Int, endIx: Int)