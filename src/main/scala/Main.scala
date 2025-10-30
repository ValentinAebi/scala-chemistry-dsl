import chemistry.Molecule
import dsl.*


object Main {

  def main(args: Array[String]): Unit = {

    // @formatter:off

    // Usually, atoms are separated by dots.
    // Indices are represented using parentheses.
    val `CH3COOH` = C.H(3).C.O.O.H
    printMass(`CH3COOH`.toMolecule)

    // If a subgroup is repeated, the syntax is (<subgroup>!<idx>).
    val `(CH3)3COH` = (C.H(3)!3).C.O.H
    printMass(`(CH3)3COH`.toMolecule)

    // A subgroup is never preceded by a dot. If there are other atoms or subgroups before it,
    // then the subgroup is introduced by its opening parenthesis only.
    val `CO(NH2)2` = C.O(N.H(2)!2)
    printMass(`CO(NH2)2`.toMolecule)

    // Equations
    val glucoseCombustionEq = C(6).H(12).O(6) + O(2) ===> H(2).O + C.O(2)
    println(glucoseCombustionEq)

    // Charges
    val `Fe^2-` = Fe^2.-
    println(`Fe^2-`.toMolecule)
    val `NO3^-` = N.O(3)^1.+
    println(`NO3^-`.toMolecule)

    // @formatter:on

  }

  private def printMass(molecule: Molecule): Unit = {
    println(s"$molecule has mass ${molecule.mass}")
  }

}
