package service

import java.util.UUID

import play.api.libs.json._

import scala.concurrent.duration._

/**
 * Created by runger on 3/8/16.
 */

case class BoardDimensions(x: Int, y: Int)

object BoardDimensions {
  val example = BoardDimensions(20, 20)
  val twentyByTwenty = BoardDimensions(20, 20)

  implicit val fmt = Json.format[BoardDimensions]
}

trait TupleFmt {
  implicit def tuple2Wr[A : Format, B : Format] = Format[(A, B)](
    Reads(jv => {
      val one = (jv \ "1").as[A]
      val two = (jv \ "2").as[B]
      JsSuccess((one, two))
    }),
    Writes( (t: Tuple2[A,B]) =>  Json.obj("1" -> t._1, "2" -> t._2) ))
}

case class Url(str: String)

object Url {
  implicit val fmt = Json.format[Url]
}

case class Player(name: String, color: Color, callback: Url)

object Player {

  implicit val colorFmt = Format(Reads(jv => {
    val jStr = jv.as[JsString]
    val color = jStr.value.charAt(0)
    JsSuccess(Color(color))
  }), Writes((color: Color) => {JsString(color.char.toString)}))

//  implicit val urlFmt = Format(Reads(jv => {
//    val jStr = jv.as[JsString]
//    val u = Url(jStr.value)
//    JsSuccess(u)
//  }), Writes((url: Url) => JsString(url.toString)))

  implicit val fmt = Json.format[Player]

  val example = List(
    Player("Randy", Color('G'), Url("http://localhost:9001"))
    ,Player("Brian", Color('B'), Url("http://localhost:9001"))
    ,Player("Akash", Color('R'), Url("http://localhost:9001"))
    ,Player("George", Color('Y'), Url("http://localhost:9001"))
  )
}

case class SpectatorConfig(url: Url)

object SpectatorConfig {
  implicit val fmt = Json.format[SpectatorConfig]
}

case class GameConfig(players: List[Player], timeout: Long, rounds: Int, boardDimensions: BoardDimensions, shapes: Set[BaseShape], spectators: Set[SpectatorConfig])

object GameConfig {

  implicit val fmt = Json.format[GameConfig]
  val example = GameConfig(Player.example, (10 seconds).toMillis, 20, BoardDimensions.example, BaseShape.standardShapes, Set.empty[SpectatorConfig])
}

case class GameSnapshot(gameId: UUID, config: GameConfig, gameState: GameState, currentScore: Score) {

  def activePlayers = config.players.filter(player => !gameState.bench.benchedPlayers.contains(player))

  //Can have multiple leaders
  def leaders = {
    val byScore = currentScore.scores.sortBy{case(player, score) => score}.reverse
    val highScore = byScore.head._2
    byScore.takeWhile{case(player, score) => score == highScore}
  }
}

case class Bench(benchedPlayers: Set[Player]) {
  def add(players: Set[Player]): Bench = Bench(benchedPlayers ++ players)
  def isFull(gameConfig: GameConfig): Boolean = {
    gameConfig.players.forall(p => this.benchedPlayers.contains(p))
  }
}

object Bench {
  val empty = Bench(Set.empty)
  implicit val fmt = Json.format[Bench]
}

case class Score(scores: List[(Player, Int)]) {
  def forPlayer(player: Player) = scores.find{case(pl, _) => pl == player}.map(_._2)
}

object Score extends TupleFmt {

  def build(players: List[Player]) = {
    Score(players.map(pl => (pl, 0)))
  }

  implicit val fmt = Json.format[Score]

  val example = Score(GameConfig.example.players.map(pl => (pl, 0)))
}

case class Move(position: Position, piece: Piece)

object Move {
  val example = Move(Position.example, Piece.example)

  implicit val fmt = Json.format[Move]
}

case class Tray(shapes: Set[(Player, Set[BaseShape])]) {
  def discard(player: Player, toDiscard: Set[BaseShape]): Tray = {
    val playersShapes = shapesFor(player)._2
    val withoutDiscards = playersShapes diff toDiscard
    val newSet = shapes.toMap.updated(player, withoutDiscards)
    Tray(newSet.toSet)
  }

  def shapesFor(player: Player) = {
    val sh = shapes.find{case (pl, _) => pl == player}
    sh.getOrElse((player, Set.empty[BaseShape]))
  }

  def pieceCountForPlayer(player: Player): Int = {
    shapes.toMap.get(player).map(_.size).getOrElse(-1)
  }

  def playersByPieceCount = {
    shapes.toList.sortBy{case(pl, sh) => pieceCountForPlayer(pl)}
  }

}

object Tray extends TupleFmt {

  def build(gameConfig: GameConfig): Tray = {
    //Initial fill of tray with shapes
    val shapesForPlayers = gameConfig.players.map(pl => (pl, gameConfig.shapes)).toSet

    Tray(shapesForPlayers)
  }

  val example = Tray(Set.empty)

  implicit val fmt = Json.format[Tray]
}

case class BonusBox(box: Set[(Player, Set[Bonus])]) {
  def add(player: Player, newBonuses: Set[Bonus]) = {
    val existing = bonusesForPlayer(player)
    val updated = existing ++ newBonuses
    BonusBox(box + (player -> updated))
  }
  def tally(player: Player) = bonusesForPlayer(player).map(_.points).sum
  def bonusesForPlayer(player: Player) = {
    val bonuses = box.find{ case(pl, b) => pl == player}.toSet
    bonuses.flatMap{ case (pl, b) => b}
  }
}

case class Bonus(name: String, points: Int)

object Bonus {
  val monominoLast = Bonus("Used all & played monomino last", 5)
  val usedAllPieces = Bonus("Used all pieces", 15)

  implicit val fmt = Json.format[Bonus]
}

object BonusBox extends TupleFmt {
  val empty = BonusBox(Set.empty)
  val example = empty
  implicit val fmt = Json.format[BonusBox]
}

case class GameState(board: Board, tray: Tray, bench: Bench, bonusBox: BonusBox, isComplete: Boolean)

object GameState {
  val example = GameState(Board.empty, Tray.example, Bench.empty, BonusBox.empty, false)
  implicit val fmt = Json.format[GameState]
}

object GameSnapshot {
  val example = GameSnapshot(UUID.fromString("12345678-9012-3456-7890-123456789012"), GameConfig.example, GameState.example, Score.example)

  def build(gameConfig: GameConfig): GameSnapshot = {
    val id = UUID.randomUUID()
    val board = Board(gameConfig.boardDimensions.x, gameConfig.boardDimensions.y)
    val tray = Tray.build(gameConfig)
    val initialState = GameState(board, tray, Bench.empty, BonusBox.empty, false)
    GameSnapshot(id, gameConfig, initialState, Score.build(gameConfig.players))
  }

  implicit val fmt = Json.format[GameSnapshot]
}

object Pass {
  implicit val fmt = Format(Reads(jv => JsSuccess(Pass)), Writes((p: Pass.type) => JsObject(Seq.empty)))
}

case class MoveRequest(player: Player, liveGame: GameSnapshot)

object MoveRequest {
  implicit val fmt = Json.format[MoveRequest]
}

trait Problem {
  def msg: String
}

case class ParseProblem(thing: String) extends Problem {
  val msg = s"Couldn't parse: $thing"
}

case class IllegalMove(move: Move) extends Problem {
  val msg = "Bad move"
}

case class NotFound(msg: String) extends Problem

case class MoveTimedOut() extends Problem {
  def msg = "Move timed out"
}

case class IllegalShape() extends Problem {
  val msg = "Illegal shape!"
}