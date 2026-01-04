package dsl

import chemistry.Reaction.Result
import chemistry.Reaction.Result.Failure
import chemistry.{Molecule, NoCoefEquation}
import units.{Gram, Mol, mol}

opaque type ParametersListPlaceholder = Context | IllegalParameterException
opaque type MoleculeAmount = (Molecule, Mol)

object reaction

extension (react: reaction.type) def apply(paramsList: Context ?=> Unit): ParametersListPlaceholder = {
  val ctx = new Context
  try {
    paramsList(using ctx)
  } catch {
    case ipe: IllegalParameterException =>
      return ipe
  }
  ctx
}

extension (eq: NoCoefEquation) infix def in(params: ParametersListPlaceholder): Result = params match {
  case ctx: Context =>
    val reaction = ctx.mkReactionFor(eq)
    val reactionResult = reaction.react
    if (ctx.printFlagIsRaised) {
      println(reactionResult)
    }
    reactionResult
  case IllegalParameterException(msg) =>
    Failure(msg)
}

object available

extension (a: available.type) def apply(reactants: MoleculeAmount*)(using ctx: Context): Unit = {
  for ((molecule, moles) <- reactants) {
    ctx.saveReactantConstraint(molecule, moles)
  }
}

object target

extension (t: target.type) def apply(products: MoleculeAmount*)(using ctx: Context): Unit = {
  for ((molecule, moles) <- products) {
    ctx.saveProductConstraint(molecule, moles)
  }
}

extension (amount: Gram) infix def of(molecule: Molecule): MoleculeAmount =
  molecule -> amount / molecule.mass

extension (amount: Mol) infix def of(molecule: Molecule): MoleculeAmount =
  molecule -> amount

object efficiency {
  def ==(percent: Percent)(using ctx: Context): Unit = {
    ctx.saveEfficiency(percent.value / 100.0)
  }
}

case class Percent(value: Int) extends AnyVal

extension (percent: Int) def percent: Percent = Percent(percent)

def PRINT(using ctx: Context): Unit = {
  ctx.raisePrintFlag()
}

extension (success: Result.Success) infix def amountOfReactant(molecule: Molecule): Mol =
  success.reactants.find(_._1 == molecule)
    .map(_._3)
    .getOrElse(0.mol)

extension (success: Result.Success) infix def amountOfProduct(molecule: Molecule): Mol =
  success.products.find(_._1 == molecule)
    .map(_._2)
    .getOrElse(0.mol)
