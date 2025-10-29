import chemistry.Molecule
import dsl.*

object Main {

  def main(args: Array[String]): Unit = {

    // @formatter:off

    val `CH3COOH` = C.H(3).C.O.O.H
    printMass(`CH3COOH`.toMolecule)

    val `(CH3)3COH` = (C.H(3)!3).C.O.H
    printMass(`(CH3)3COH`.toMolecule)

    val `CO(NH2)2` = C.O(N.H(2)!2)
    printMass(`CO(NH2)2`.toMolecule)

    val glucoseCombustionEq = C(6).H(12).O(6) + O(2) ===> H(2).O + C.O(2)
    println(glucoseCombustionEq)

    // @formatter:on

  }

  private def printMass(molecule: Molecule): Unit = {
    println(s"$molecule has mass ${molecule.mass}")
  }

}
