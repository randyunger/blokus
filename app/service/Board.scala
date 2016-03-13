package service

import play.api.libs.json._
import scalaz.{\/-, -\/, \/}

/**
 * Created by runger on 3/8/16.
 */

case class Matrix[T](arr:List[List[T]]) {
  def rotate90 = Matrix(arr.transpose.map(_.reverse))
  def rotate180 = rotate90.rotate90
  def rotate270 = rotate180.rotate90

  def rotations = List(this, rotate90, rotate180, rotate270)
  
  def allPositions: List[Position] = {
    val positions = for {
      x <- arr.indices
      y <- arr(x).indices
    } yield Position(x,y)
    positions.toList
  } 

  def atPosition(position: Position): Option[T] = {
    for {
      row <- arr.lift(position.x)
      cell <- row.lift(position.y)
    } yield cell
  }

  def find(predicate: T => Boolean): List[T] = {
    for {
      row <- arr
      cell <- row
      if predicate(cell)
    } yield cell
  }

  def setPositions(positions: List[Position], t: T) = {
    //mutable buffer
    val cols = arr.map(lT => lT.toBuffer)
    val buff = cols.toBuffer

    //update positions
    positions.map {
      case Position(x, y) => buff(x)(y) = t
    }

    //back to immutable
    val imCols = buff.map(bT => bT.toList)
    val newArr = imCols.toList
    Matrix(newArr)
  }

  def stringRep: String = {
    def strRow(row: List[T]) = {
      val chars = row.map(t => {
        t match {
          case _: Empty.type => '-'
          case _: Full.type => 'X'
          case Block(c) => c.char
        }
      })
      chars.mkString
    }
    val matStr = arr.map(row => strRow(row)).mkString("\n")
    matStr
  }

  override def toString = stringRep
}

object Matrix {
  //Todo: does this belong in board?
  def applySq(rows: String): Matrix[Square] = {
    def strRowToArray(row: String) = {
      val seq = for(ch <- row) yield {
        ch match {
          case '-' => Empty
          case 'X' => Full
          case ch: Char => Block(Color(ch))
        }
      }
      seq.toList
    }

    val sq = rows.split('\n').toList.map(row => strRowToArray(row))
    Matrix(sq)
  }
}

case class Color(char: Char)

//A base shape is the canonical orientation of shapes allowed in the game
//See Piece for an application of a color and a rotation to a shape
case class BaseShape(matrix: Matrix[Square]) {
  //All four piece rotations of this base shape, with the color applied
  def pieces(color: Color): Set[Piece] = {
    matrix.rotations.map(mat => Piece(mat, color)).toSet
  }
  def squareAt(position: Position) = matrix.atPosition(position).getOrElse(OutOfBounds)
  override def toString = matrix.stringRep
}

trait Square
object Empty extends Square
trait Full extends Square
object Full extends Full
object OutOfBounds extends Square
case class Block(color: Color) extends Full

case class Position(x: Int, y: Int) {
  def offset(pos: Position) = Position(x + pos.x, y + pos.y)
}

object Position {
  val example = Position(0,0)

  implicit val fmt = Json.format[Position]
}

//A piece applies orientation and color to a shape
//Todo: color and matrix both store the color. Choose a single source of truth.
case class Piece(matrix: Matrix[Square], color: Color) {

  def squareAt(position: Position) = matrix.atPosition(position).getOrElse(OutOfBounds)

  //Return the matching base shape, if any
  def baseShapeFrom(baseShapes: Set[BaseShape]): Option[BaseShape] = {
    val pieceRotations = matrix.rotations
    baseShapes.find(sh => {
      pieceRotations.contains(sh.matrix)
    })
  }

  def fullPositions = {
    matrix.allPositions.filter(pos => {
      val sq = squareAt(pos)
      sq.isInstanceOf[Full]
    })
  }

  def stringRep: String = matrix.stringRep
  override def toString = "\n" + matrix.stringRep
}

object Piece {

  val example = Piece(BaseShape.standardShapes.head.matrix, Color('G'))

  def apply(str: String, color: Color): Piece = {
    val matrix = Matrix.applySq(str)
    Piece(matrix, color)
  }

  //todo: color is redundant, its already in the serialized matrix!
  implicit val fmt = Format(Reads(jv => {
    val s = (jv \ "s").as[JsString].value
    val c = (jv \ "c").as[JsString].value
    val p = Piece(s, Color(c.head))
    JsSuccess(p)
  }), Writes((piece: Piece) => {
    Json.obj("s" -> JsString(piece.stringRep), "c" -> JsString(piece.color.char.toString))
  }))

}

object BaseShape {
  val monomino = BaseShape("X")

  val duo = BaseShape("XX")

  val threeBar = BaseShape("XXX")

  val trio =BaseShape(
    """X-
      |XX
    """.stripMargin.trim)

  val block = BaseShape(
    """XX
      |XX
    """.stripMargin.trim)

  val tetris = BaseShape(
    """X-
      |XX
      |X-
    """.stripMargin.trim)

  val fourBar = BaseShape("XXXX")

  val ell = BaseShape(
    """X-
      |X-
      |XX
    """.stripMargin.trim)

  val zig = BaseShape(
    """-XX
      |XX-
    """.stripMargin.trim)

  val bigEll = BaseShape(
    """X----
      |XXXXX
    """.stripMargin.trim)

  val capitalT = BaseShape(
    """XXX
      |-X-
      |-X-
    """.stripMargin.trim)

  val flyingWedge = BaseShape(
    """XXX
      |X--
      |X--
    """.stripMargin.trim)

  val ziiig = BaseShape(
    """-XXX
      |XX--
    """.stripMargin.trim)

  val zag = BaseShape(
    """--X
      |XXX
      |X--
    """.stripMargin.trim)

  val fiveBar = BaseShape("XXXXX")

  val squareNub = BaseShape(
    """X-
      |XX
      |XX
    """.stripMargin.trim)

  val wiggle = BaseShape(
    """-XX
      |XX-
      |X--
    """.stripMargin.trim)

  val you = BaseShape(
    """X-X
      |XXX
    """.stripMargin.trim)

  val eff = BaseShape(
    """-XX
      |XX-
      |-X-
    """.stripMargin.trim)

  val plus = BaseShape(
    """-X-
      |XXX
      |-X-
    """.stripMargin.trim)

  val lump = BaseShape(
    """-X--
      |XXXX
    """.stripMargin.trim)

  def apply(rows: String): BaseShape = {
    def strRowToArray(row: String) = {
      val seq = for(ch <- row) yield {
        ch match {
          case '-' => Empty
          case 'X' => Full
        }
      }
      seq.toList
    }

    val shape = rows.split('\n').toList.map(row => strRowToArray(row))
    BaseShape(Matrix(shape))
  }

  val standardShapes = Set(monomino, duo, threeBar, trio, block, ell, tetris)

  implicit val shapeFmt = Format(Reads(jv => {
    val str = jv.as[JsString].value
    JsSuccess(BaseShape.apply(str))
  }), Writes((shape: BaseShape) => JsString(shape.matrix.stringRep)))

}

case class Board(matrix: Matrix[Square]) {

  def squareAt(position: Position) = {
    matrix.atPosition(position).getOrElse(OutOfBounds)
  }

  def place(move: Move): Problem \/ Board = {
    if(canPlace(move)){
      //For all positions in the piece
      //Where the position is full
      val piecePositions = move.piece.fullPositions
      //At the offset from the placed position
      val offsetPos = piecePositions.map(pos => move.position.offset(pos))
      //Update the board to the piece's color
      \/-(Board(matrix.setPositions(offsetPos, Block(move.piece.color))))
    }
    else -\/(IllegalMove(move))
  }

  def canPlace(move: Move): Boolean = {
    allOverlapSquaresAreEmpty(move) &&
//      cornerMatchesColor(move) &&
      noEdgeAdjacency(move) &&
      firstPlayInCorner(move)
  }

  def boardPositionsForMove(move: Move): Set[Position] = {
    //For all positions in the piece
    val piecePositions = move.piece.fullPositions
    //Offset by the placement
    val offsetPos = piecePositions.map(pos => move.position.offset(pos))
    offsetPos.toSet
  }

  def allOverlapSquaresAreEmpty(move: Move): Boolean = {
    //Get the squares
    val boardSquares = boardPositionsForMove(move).map(pos => squareAt(pos))
    //Assert they are all empty
    boardSquares.forall(sq => sq == Empty)
  }

  def corners(move: Move): Set[Position] = {
    def overlap(pos: Position) = boardPositionsForMove(move).contains(pos)
    //Get surrounding positions
    val candidates = surroundingPositions(move)
    //Filter by
    candidates.filter(pos => {
      //1)a corner of surround position overlaps
      //2)no adjacents overlap
      corners(pos).exists(overlap) &&
      !adjacents(pos).exists(overlap)
    })
  }

  def corners(position: Position) = for {
    xOff <- List(-1, 1)
    yOff <- List(-1, 1)
  } yield position.offset(Position(xOff, yOff))

  def adjacents(position: Position) = for {
    offsetPosition <- List(Position(-1,0), Position(0,-1), Position(0,1), Position(1,0))
  } yield position.offset(offsetPosition)
  
  def cornerMatchesColor(move: Move): Boolean = {
    //The corners of the move contain a square
    // such that the color of the square matches the color of the move
    corners(move).exists(pos => squareAt(pos) match {
      case Block(color) if color == move.piece.color => true
      case _ => false
    })
  }

  def surroundingPositions(move: Move): Set[Position] = {
    val surr = boardPositionsForMove(move).map(pos => surroundingPositions(pos))
    surr.flatten
  }

  def surroundingPositions(position: Position): Set[Position] = {
    val offsets = for {
      x <- -1 to 1
      y <- -1 to 1
      if !(x==0 && y==0)
    } yield Position(x,y)
    val pos = offsets.map(o => o.offset(position))
    pos.toSet
  }

  def countSquaresFor(color: Color): Int = {
    val matchingSquares = matrix.find(sq => sq match {
      case Block(c) if c == color => true
      case _ => false
    })
    matchingSquares.size
  }

  def noEdgeAdjacency(move: Move): Boolean = {
    true
  }

  def firstPlayInCorner(move: Move): Boolean = {
    true
  }

}

object Board {
  val twenty = List.iterate[Square](Empty, 20)(_ => Empty)
  val twentyByTwenty = List.iterate(twenty, 20)(_ => twenty)

  def apply(x: Int, y: Int): Board = {
    val rows = List.iterate[Square](Empty, x)(_ => Empty)
    val mat = List.iterate(rows, y)(_ => rows)
    Board(Matrix(mat))
  }

  def apply(gameConfig: GameConfig): Board = {
    apply(gameConfig.boardDimensions.x, gameConfig.boardDimensions.y)
  }

  val empty = Board(Matrix(twentyByTwenty))

  def apply(str: String): Board = {
    val mat = Matrix.applySq(str)
    Board(mat)
  }

  implicit val boardFmt = Format(Reads(jv => {
    val str = jv.as[JsString].value
    JsSuccess(Board.apply(str))
  }), Writes((board: Board) => JsString(board.matrix.stringRep)))
}

object Game extends App {
  val b = Board.empty
  val o = b.matrix.stringRep
  println(o)
}