package service

import org.scalatest.FunSuite

/**
 * Created by runger on 3/10/16.
 */
class MatrixTest extends FunSuite {

  test("update") {
    val matrix = Matrix.applySq("-X\nXX")
    val positions = List(Position(1,0), Position(1,1))
    val updated = matrix.setPositions(positions, Empty)
    val str = updated.stringRep
    println(str)
    assert(str == "-X\n--")
  }

  test("rotate90") {
    val matrix = Matrix.applySq("XX\nX-")
    val rot90 = matrix.rotate90
    assert(rot90 == Matrix.applySq("XX\n-X"))
  }

  test("rotate180") {
    val matrix = Matrix.applySq("XX\nX-")
    val rot180 = matrix.rotate180
    assert(rot180 == Matrix.applySq("-X\nXX"))
  }

  test("rotations") {
    val matrix = Matrix.applySq("XX\nX-")
    val rotations = matrix.rotations
    assert(rotations.contains(Matrix.applySq("-X\nXX")))
    assert(rotations.contains(Matrix.applySq("X-\nXX")))
    assert(rotations.contains(Matrix.applySq("XX\n-X")))
    assert(rotations.contains(Matrix.applySq("XX\nX-")))
  }

  test("mirror") {
    val matrix = Matrix.applySq("XXX\nX--")
    val mirror = matrix.mirrorH

    assert(mirror == Matrix.applySq("X--\nXXX"))

  }

}
