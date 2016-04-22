package service

import org.scalatest.FunSuite

/**
  * Created by runger on 4/1/16.
  */
class BaseShapeTest extends FunSuite {
  test("All orientations") {
    val sh = BaseShape("XXX\nX--")
//    val shapes = Set(BaseShape("--X\nXXX"))
    val c = Color('C')
    val ors = sh.allOrientations(c)

    assert(ors == Set(
      Piece("XXX\nX--", c)
      ,Piece("XX\n-X\n-X", c)
      ,Piece("--X\nXXX", c)
      ,Piece("X-\nX-\nXX", c)
      //Mirror
      ,Piece("XXX\n--X", c)
      ,Piece("XX\nX-\nX-", c)
      ,Piece("X--\nXXX", c)
      ,Piece("-X\n-X\nXX", c)
    ))

  }
}
