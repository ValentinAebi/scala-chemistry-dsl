package dsl

import chemistry.{Equation, Molecule}
import units.{Gram, Mol}

final case class ReactionData(equation: Equation, reactants: Seq[(Molecule, Mol)]) {

  override def toString: String = {
    s"ReactionData {\n" +
      s"  $equation\n  " +
      reactants.map((molec, amount) => s"$amount of $molec").mkString("\n  ") +
      "\n}"
  }

}

object reaction {
  infix def of(equation: Equation): ExpectReactants = ExpectReactants(equation)

  final class ExpectReactants(private val equation: Equation) {
    inline def using(inline reactants: Reactant): ReactionData =
      ReactionData(equation, usingMacro(reactants))
  }

}

object equation {
  inline def ~(inline str: String): LeftMember = LeftMember(List(parseAndStaticCheckMolecule(str)))
}

enum Reactant {
  case GramReactant(moleculeStr: String, amount: Gram)
  case MolReactant(moleculeStr: String, amount: Mol)
}

extension (amount: Mol) infix def of(moleculeStr: String): Reactant =
  throw new AssertionError()

extension (amount: Gram) infix def of(moleculeStr: String): Reactant =
  throw new AssertionError()
