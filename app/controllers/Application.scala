package controllers

import java.net.URL

import agents.RandomAgent
import com.ning.http.client.AsyncHttpClientConfig
import play.api._
import play.api.libs.json.Json
import play.api.libs.ws.WS
import play.api.libs.ws.ning.NingWSClient
import play.api.mvc._
import service._
import spectators.{Bleachers, Spectator}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._

import scala.concurrent.{Await, Future}
import scalaz.{\/-, -\/}

object Application extends Controller {

  def index = Action.async {
    Future(Ok(views.html.index("Your new application is ready.")))
  }

  def newGame = Action(parse.json) { request =>
    val resp = request.body.asOpt[GameConfig] match {
      case None => BadRequest("Couldn't get game config")
      case Some(gameConfig) => {
        val liveGame = LiveGame.build(gameConfig)
        Future {
          try {
            GameRunner().proceed(liveGame)
          } catch {
            case t: Throwable => t.printStackTrace()
          }
        }
        val js = Json.toJson(liveGame)
        Ok(js)
      }
    }
    resp
  }

  val agent = new RandomAgent

  def move = Action(parse.json) { request =>
    Logger.info(s"Got a request to play a move")

    val res = request.body.asOpt[MoveRequest] match {
      case None => BadRequest("Couldn't get move request")
      case Some(moveRequest) => {
        val moveOrPass = agent.play(moveRequest.player, moveRequest.liveGame)
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

    val res = request.body.asOpt[MoveRequest] match {
      case None => BadRequest("Couldn't get move to watch")
      case Some(moveReq) => {
        val spectator = Bleachers().getOrCreateSpectator(moveReq.liveGame.gameId, moveReq.liveGame.config)
        spectator.observe(moveReq.liveGame)
        Ok
      }
    }
    res
  }

  def displayLatest = Action {
    val spectator = Bleachers().getOneOrDefault

    val board = spectator.renderBoard()
    val tray = spectator.renderTray()
    val config = spectator.renderConfig()

    Ok(views.html.display(board, tray, config))
  }
}

object StartGame extends App {

  val server = "http://localhost:9000"

  val gameConfig = GameConfig(List(
    Player("Randy", Color('G'), Url("http://localhost:9000/move"))
  ,Player("Bob", Color('R'), Url("http://localhost:9000/move"))
  ,Player("Al", Color('Y'), Url("http://localhost:9000/move"))
  ,Player("Joe", Color('B'), Url("http://localhost:9000/move"))
  ), (5 minutes).toMillis, BoardDimensions(10, 10), BaseShape.standardShapes, Set(SpectatorConfig(Url("http://localhost:9000/watch"))))


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
