package math

import org.junit.jupiter.api.Assertions.{assertEquals, assertNotEquals, fail}
import org.junit.jupiter.api.Test

class MatrixTests {

  @Test
  def diagonalizeSimpleMatrixTest(): Unit = {
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
    m.diagonalize()
    assertEquals(d, m)
  }

}
