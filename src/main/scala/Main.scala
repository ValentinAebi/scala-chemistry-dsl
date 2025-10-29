import chemistry.Molecule
import chemistrydsl.*

object Main {

  def main(args: Array[String]): Unit = {

    val aceticAcid = C.H(3).C.O.O.H
    printMass(aceticAcid.toMolecule)

    val equation = C(6).H(12).O(6) + O(2) ===> H(2).O + C.O(2)
    println(equation)

  }

  private def printMass(molecule: Molecule): Unit = {
    println(s"$molecule has mass ${molecule.mass}")
  }

}
