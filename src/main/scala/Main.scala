import chemistry.Molecule
import dsl.*

import ImplicitMoleculeParsing.*

object Main {

  def main(args: Array[String]): Unit = {
    val molecule: Molecule = "C6H12O6"
    printMass(molecule)
  }

  private def printMass(molecule: Molecule): Unit = {
    println(s"$molecule has mass ${molecule.mass}")
  }

}
