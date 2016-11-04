package spectators

import java.util.UUID
import java.util.concurrent.ConcurrentHashMap

import org.joda.time.DateTime
import play.api.Logger
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
    val idsByStart = byStartTime.map{case(id, sp) => id -> sp.latestSnapshot.map(_.time).getOrElse(future)}
    views.html.gameList(idsByStart)
  }

  def renderWinsChart(): Html = {
    views.html.winsChart()
  }

  def renderScoreChart(): Html = {
    views.html.scoreChart()
  }

  def latestSnapshots = spectators.flatMap {case (id, sp) => sp.latestSnapshot}

  def finalSnapshots = spectators.flatMap {case (id, sp) => sp.finalSnapshot()}

  def winners = finalSnapshots.map(snapshot => {
    snapshot.snapshot.leaders.map(_._1)
  })

  def allPlayers = spectators.flatMap(sp => sp._2.gameConfig.players).toSet

  def winCounts = {
    val score = allPlayers.map(pl => {
      var winCt = 0
      val ptsList = finalSnapshots.map(sn => {
        val leaders = sn.snapshot.leaders.map(_._1)
        val points = if(leaders.contains(pl)) 1 else 0
        winCt = winCt + points
        winCt
      }).toList
      val finalPtsList = if(ptsList.isEmpty) List(0) else ptsList
      pl -> finalPtsList
    }).toList
    RunningScore(score)
  }

  def runningScore = {
    Logger.info("api request for score")

    val score = allPlayers.map(pl => {
      var rPts = 0
      //This won't work right if there are games involving different groups of players?
      //Will stay plateaued for the games it misses out on
      val ptsList = latestSnapshots.map(sn => {
        val currPts = sn.snapshot.currentScore.scores.find(_._1 == pl).map(_._2).getOrElse(0)
        rPts = rPts + currPts
        rPts
      }).toList
      val finalPtsList = if(ptsList.isEmpty) List(0) else ptsList
      pl -> finalPtsList
    }).toList
    RunningScore(score)
  }
}

case class SpectatedSnapshot(snapshot: GameSnapshot, time: DateTime, currentScore: Score, isComplete: Boolean)

case class Spectator(gameConfig: GameConfig) {
  val history = mutable.Buffer.empty[SpectatedSnapshot]

  def firstSnapshot = history.sortBy(_.time.getMillis).headOption
  def latestSnapshot = history.sortBy(_.time.getMillis).lastOption

  def observe(liveGame: GameSnapshot) = {
    val isComplete = liveGame.gameState.isComplete
    val ss = SpectatedSnapshot(liveGame, new DateTime(), liveGame.currentScore, isComplete)
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

  def finalSnapshot(): Option[SpectatedSnapshot] = {
    history.find(_.isComplete)
  }

  def renderScore(): Html = {
    val spectators = Bleachers().spectators.toList

    val allPlayers = Bleachers().latestSnapshots.flatMap(sn => sn.snapshot.config.players)

    val wins = allPlayers.map(pl => (pl -> Bleachers().winners.flatten.count(winner => winner == pl))).toMap

    val totalPoints = allPlayers.map(pl => {
      val points = Bleachers().latestSnapshots.flatMap(sn => sn.currentScore.forPlayer(pl))
      val sumForPlayer = points.sum
      pl -> sumForPlayer
    })

    val ds = DisplayScores(wins, totalPoints.toMap)

    views.html.score(ds)
  }

  def renderBlankControls(): Html = {
    views.html.controls(DisplayControls(0,0))
  }


}

case class DisplayControls(currIx: Int, endIx: Int)
case class DisplayScores(wins: Map[Player, Int], totalScores: Map[Player, Int])