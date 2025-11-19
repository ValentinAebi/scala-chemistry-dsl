import chemistry.EquationWithCoefs
import org.junit.jupiter.api.Test
import dsl.*
import org.junit.jupiter.api.Assertions.assertEquals

class EquationBalancingTests {

  @Test
  def balanceRedoxEquationTest(): Unit = {
    val input = ~"SO3^2-" + "MnO4^-" + "H^+" --> "SO4^2-" + "Mn^2+" + "H2O"
    val result = EquationWithCoefs.balancedFrom(input).get
    val expected = "5 SO3^2- + 2 MnO4^- + 6 H^+ --> 5 SO4^2- + 2 Mn^2+ + 3 H2O"
    assertEquals(expected, result.toString)
  }

}
