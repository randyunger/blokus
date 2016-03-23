package service

/**
 * Created by runger on 3/9/16.
 */

class GameRunnerTest extends org.scalatest.FunSuite {

  test("Shape from tray - base orientation") {
    val player = Player.example.head
    val tray = Tray(Set(player -> Set(BaseShape.trio, BaseShape.tetris)))
    val piece = Piece(BaseShape.trio.matrix, player.color)

    val baseShape = GameRunner().shapeFromTray(tray, player, piece)
    assert(baseShape.toOption.get == BaseShape.trio)
  }

  test("Shape from tray - rotated") {
    val player = Player.example.head
    val tray = Tray(Set(player -> Set(BaseShape.trio, BaseShape.tetris)))
    val piece = Piece(BaseShape.trio.matrix.rotate90, player.color)

    val baseShape = GameRunner().shapeFromTray(tray, player, piece)
    assert(baseShape.toOption.get == BaseShape.trio)
  }

  test("Get next player - different each time") {
    val game = GameSnapshot.build(GameConfig.example)
    assert(game.config.players.size > 1)
    val player1 = GameRunner().getNextPlayer(game).get

    //Simulate player 1 playing a piece
    val p1Shapes = game.gameState.tray.shapesFor(player1)
    val playedPiece = p1Shapes._2.take(1)
    println(s"Playing piece: $playedPiece")
    val newTray = game.gameState.tray.discard(player1, playedPiece)
    val newGameState = game.gameState.copy(tray = newTray)
    val newLiveGame = game.copy(gameState = newGameState)

    //Get player 2
    val p2 = GameRunner().getNextPlayer(newLiveGame).get

    //Should be a different player
    assert(player1 != p2)
  }

  test("Get next player - multiple players") {
    val game = GameSnapshot.build(GameConfig.example)
    assert(game.config.players.size > 1)
    val player1 = GameRunner().getNextPlayer(game).get
    assert(player1 == GameConfig.example.players.head)

    //Simulate player 1 playing a piece
    val p1Shapes = game.gameState.tray.shapesFor(player1)
    val playedPiece = p1Shapes._2.take(1)
    println(s"Playing piece: $playedPiece")
    val newTray = game.gameState.tray.discard(player1, playedPiece)
    val newGameState = game.gameState.copy(tray = newTray)
    val newLiveGame = game.copy(gameState = newGameState)

    //Get player 2
    val p2 = GameRunner().getNextPlayer(newLiveGame).get

    //Asert player is second per config order
    assert(GameConfig.example.players.drop(1).head == p2)
  }

}
