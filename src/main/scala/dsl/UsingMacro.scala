package dsl

import chemistry.{Equation, Molecule}
import units.{Mol, g, mol}

import scala.quoted.*

inline def usingMacro(inline reactants: Reactant): Seq[(Molecule, Mol)] = {
  ${ usingMacroImpl('reactants) }
}

def usingMacroImpl(reactants: Expr[Reactant])(using quotes: Quotes): Expr[Seq[(Molecule, Mol)]] = {
  import quotes.reflect.report
  reactants match {
    case '{ ($amountExpr: Double).mol.of($molecStr: String) } =>
      val amountOpt = amountExpr.value
      if (!amountOpt.exists(_ > 0)) {
        report.errorAndAbort("amount should be a positive literal", reactants)
      }
      val molecule = parseAndStaticCheckMoleculeMacroImpl(molecStr)
      '{ Seq($molecule -> $amountExpr.mol) }
    case '{ ($amountExpr: Double).g.of($molecStr: String) } =>
      val amount = amountExpr.value
      if (!amount.exists(_ > 0)) {
        report.errorAndAbort("amount should be a positive literal", reactants)
      }
      val molecule = parseAndStaticCheckMoleculeMacroImpl(molecStr)
      '{ Seq($molecule -> $amountExpr.g / $molecule.mass) }
    case '{ ($r1: Reactant) ; ($r2: Reactant) } =>
      '{ ${ usingMacroImpl(r1) } ++ ${ usingMacroImpl(r2) } }
    case _ => report.errorAndAbort("expected reactants", reactants)
  }
}
