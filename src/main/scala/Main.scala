import chemistry.Molecule
import dsl.*


object Main {

  def main(args: Array[String]): Unit = {
    val eq = "C6H12O6" & "O2" ==> "H2O" & "CO2"
    println(eq)
  }

  private def printMass(molecule: Molecule): Unit = {
    println(s"$molecule has mass ${molecule.mass}")
  }

}
