package dsl

import chemistry.{Equation, Molecule, Reaction}
import units.Mol

import scala.collection.mutable

private final class Context {
  private val reactants = mutable.Map.empty[Molecule, Mol]
  private val products = mutable.Map.empty[Molecule, Mol]
  private var efficiencyOpt = Option.empty[Double]
  private var printFlag = false

  def saveReactantConstraint(molecule: Molecule, amount: Mol): Unit = {
    assertConstraintValidity(molecule, amount, reactants)
    reactants(molecule) = amount
  }

  def saveProductConstraint(molecule: Molecule, amount: Mol): Unit = {
    assertConstraintValidity(molecule, amount, products)
    products(molecule) = amount
  }
  
  def saveEfficiency(effValue: Double): Unit = {
    if (efficiencyOpt.exists(_ != effValue)){
      throw IllegalParameterException("efficiency set twice to different values")
    }
    val margin = 1e-4
    if (!(0.0 - margin <= effValue && effValue <= 1.0 + margin)){
      throw IllegalParameterException("illegal value of efficiency, must be between 0.percent and 100.percent")
    }
    efficiencyOpt = Some(effValue)
  }
  
  def printFlagIsRaised: Boolean = printFlag
  
  def raisePrintFlag(): Unit = {
    printFlag = true
  }
  
  def mkReactionFor(equation: Equation): Reaction = Reaction(
    equation,
    reactants.toMap,
    products.toMap,
    efficiencyOpt.getOrElse(1.0)
  )

  private def assertConstraintValidity(molecule: Molecule, amount: Mol, equationSide: mutable.Map[Molecule, Mol]): Unit = {
    if (amount.value < 0) {
      throw IllegalParameterException(s"negative amount of $molecule")
    }
    if (equationSide.get(molecule).exists(_ != amount)) {
      throw IllegalParameterException(s"amount of $molecule set twice to different values")
    }
  }

}

