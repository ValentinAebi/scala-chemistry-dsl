package math

import org.junit.Assert.{assertEquals, assertNotEquals}
import org.junit.Test

class MatrixTests {

  @Test
  def gaussianEliminationSimpleMatrixTest(): Unit = {
    val m = Matrix(List(
      List(1, 2, -1, -3),
      List(3, 1, 2, 6),
      List(1, -1, 0, 2)
    ))
    val d = Matrix(List(
      List(1, 0, 0, 1),
      List(0, 1, 0, -1),
      List(0, 0, 1, 2)
    ))
    assertNotEquals(d, m)
    m.gaussianElimination()
    assertEquals(d, m)
  }

}
