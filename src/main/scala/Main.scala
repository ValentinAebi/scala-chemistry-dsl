import chemistry.{BalancedEquation, Molecule, NoCoefEquation}
import dsl.*
import units.*


object Main {

  def main(args: Array[String]): Unit = {

    val equation1 = ~"C6H12O6" + "O2" --> "H2O" + "CO2"
    balanceAndDisplay(equation1)

    val equation2 = ~"SO3^2-" + "MnO4^-" + "H^+" --> "SO4^2-" + "Mn^2+" + "H2O"
    balanceAndDisplay(equation2)

    val r = reaction of equation ~ "C6H12O6" + "O2" --> "H2O" + "CO2" using :
      2500.mol of "C6H12O6"
      25.g of "O2"
    println(r)

  }

  private def balanceAndDisplay(equation: NoCoefEquation): Unit = {
    println(BalancedEquation.balancedFrom(equation).getOrElse("equation could not be balanced"))
  }

  private def printMass(molecule: Molecule): Unit = {
    println(s"$molecule has mass ${molecule.mass}")
  }

}
