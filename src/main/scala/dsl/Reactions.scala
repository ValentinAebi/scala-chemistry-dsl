package dsl

import chemistry.Reaction.Result
import chemistry.{Molecule, NoCoefEquation, Reaction}
import units.{Gram, Mol}

opaque type ReactionParametersList = Context
opaque type MoleculeAmount = (Molecule, Mol)

object reaction

extension (react: reaction.type) def apply(paramsList: Context ?=> Unit): ReactionParametersList = {
  val ctx = Context()
  paramsList(using ctx)
  ctx
}

extension (eq: NoCoefEquation) infix def in(params: ReactionParametersList): Result = {
  val ctx: Context = params
  val reaction = ctx.mkReactionFor(eq)
  val reactionResult = reaction.react
  if (ctx.printFlagIsRaised) {
    println(reactionResult)
  }
  reactionResult
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
