package controllers

import agents.{RandomAgent, RandyAgent}
import com.ning.http.client.AsyncHttpClientConfig
import play.api._
import play.api.libs.json.Json
import play.api.libs.ws.ning.NingWSClient
import play.api.mvc._
import service._
import spectators.Bleachers

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.concurrent.{Await, Future}
import scalaz.{-\/, \/-}

object Application extends Controller {

  def index = Action {
    Ok(views.html.setup())
  }

  def newGameUI = Action { request =>
//    val confStr = request.getQueryString("config")

    val confStr = for {
      form <- request.body.asFormUrlEncoded
      confS <- form.get("config")
      confJ <- confS.headOption
    } yield confJ

    confStr match {
      case Some(c) => {
        startGame(c) match {
          case None => BadRequest("Couldn't build game")
          case Some(g) => {
            Thread.sleep(1000)
            Redirect(s"display/${g.gameId.toString}/0")
          }
        }
      }
      case None => BadRequest("send config json")
    }
  }

  def startGame(json: String): Option[GameSnapshot] = {
    val conf = Json.parse(json).asOpt[GameConfig]

    val resp = conf match {
      case None => None//BadRequest("Couldn't get game config")
      case Some(gameConfig) => {
        var liveGame = GameSnapshot.build(gameConfig)
        Future {
          try {
            (1 to gameConfig.rounds).foreach(i => {
              GameRunner().proceed(liveGame)
              liveGame = GameSnapshot.build(gameConfig)
            })
          } catch {
            case t: Throwable => t.printStackTrace()
          }
        }
        Some(liveGame)
      }
    }
    resp
  }

  def newGameAPI = Action{ request =>
    request.body.asJson match {
      case None => BadRequest("couldn't get request body")
      case Some(jStr) => {
        val str = Json.asciiStringify(jStr) //stupid hack
        startGame(str) match {
          case None => BadRequest("couldn't start game")
          case Some(sn) =>  {
            val js = Json.toJson(sn)
            Ok(js)
          }
        }
      }
    }
  }

  val randomAgent = new RandomAgent
  def randomMove = Action(parse.json) { request =>
    Logger.info(s"Got a request to play a move")

    val res = request.body.asOpt[MoveRequest] match {
      case None => BadRequest("Couldn't get move request")
      case Some(moveRequest) => {
        val moveOrPass = randomAgent.play(moveRequest.player, moveRequest.liveGame)
        val moveJs = moveOrPass match {
          case -\/(pass) => Json.toJson(pass)
          case \/-(move) => Json.toJson(move)
        }
        Ok(moveJs)
      }
    }
    res
  }

  val rAgent = new RandyAgent
  def randyMove = Action(parse.json) { request =>
    Logger.info(s"Got a request to play a move")

    val res = request.body.asOpt[MoveRequest] match {
      case None => BadRequest("Couldn't get move request")
      case Some(moveRequest) => {
        val moveOrPass = rAgent.play(moveRequest.player, moveRequest.liveGame)
        val moveJs = moveOrPass match {
          case -\/(pass) => Json.toJson(pass)
          case \/-(move) => Json.toJson(move)
        }
        Ok(moveJs)
      }
    }
    res
  }

  def watch = Action(parse.json) { request =>
    Logger.info(s"A new move was posted")

    val res = request.body.asOpt[GameSnapshot] match {
      case None => BadRequest("Couldn't get move to watch")
      case Some(snapshot) => {
        val spectator = Bleachers().getOrCreateSpectator(snapshot.gameId, snapshot.config)
        spectator.observe(snapshot)
        Ok
      }
    }
    res
  }

  def displayStart = Action {
    import play.twirl.api._
    val spectator = Bleachers().getOneOrDefault

    val board = Html("")//spectator.renderStartBoard()
    val tray = spectator.renderStartTray()
    val config = spectator.renderConfig()
    val gameList = Bleachers().renderGameList()
    val gameControls = spectator.renderBlankControls()
    val score = Html("")

    Ok(views.html.display(board, tray, config, gameList, gameControls, score))
  }

  def displaySnapshot(id: String, ix: Int) = Action { request =>

    try {
      Bleachers().getSpectator(id) match {
        case None => BadRequest("Unknown id")
        case Some(spectator) => {
          val board = spectator.renderBoard(ix)
          val tray = spectator.renderTray(ix)
          val config = spectator.renderConfig()
          val gameList = Bleachers().renderGameList()
          val gameControls = spectator.renderControls(ix)
          val score = spectator.renderScore()

          Ok(views.html.display(board, tray, config, gameList, gameControls, score))
        }
      }
    } catch {
      case ex: IndexOutOfBoundsException => {
        Thread.sleep(1000)
        Redirect(s"${ix}")
      }
    }
  }

}

object DefaultGame {
  val defaultConfig = GameConfig(List(
    Player("Randy", Color('R'), Url("http://localhost:9000/randyMove"))
    ,Player("Brian", Color('B'), Url("http://localhost:9000/randomMove"))
    ,Player("Trey", Color('P'), Url("http://localhost:9000/randomMove"))
    ,Player("Akash", Color('G'), Url("http://localhost:9000/randomMove"))
  ), (5 minutes).toMillis, 10, BoardDimensions(20, 20), BaseShape.standardShapes,
    Set(SpectatorConfig(Url("http://localhost:9000/watch"))))

  val defaultConfigJson = Json.stringify(Json.toJson(defaultConfig))
}

object StartGame extends App {

  val server = "http://localhost:9000"

  val gameConfig = DefaultGame.defaultConfig


  val gameJv = Json.toJson(gameConfig)
  val gameJson = Json.asciiStringify(gameJv)

  val builder = new AsyncHttpClientConfig.Builder()
  val client = new NingWSClient(builder.build())
  val w= client.url(server + "/newGame")

  println(s"Posting:\n" + gameJson)

  val r = w.post(gameJv)
  val res = Await.result(r, 10 * 1000 millis)
  val body = res.body

  println(body)
  System.exit(0)
}
