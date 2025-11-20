package chemistry

import chemistry.Reaction.Result.{Failure, Success}
import units.*

final case class Reaction(equation: Equation, reactants: Seq[(Molecule, Mol)], efficiency: Double) {

  def react: Reaction.Result = {
    val balancedEquation = computeBalancedEquation() match {
      case None =>
        return Failure("could not balance equation")
      case Some(balancedEquation) => balancedEquation
    }
    findMissingReactant(balancedEquation) match {
      case Some(molecule) =>
        return Failure(s"no $molecule available for reaction")
      case None => ()
    }
    val reactantsMap = reactants.toMap
    val individuallyPossibleCoefs = balancedEquation.left.map { (molec, coef) =>
      molec -> efficiency * reactantsMap.apply(molec) / coef
    }
    val (limitingReactant, globalCoef) = individuallyPossibleCoefs.minBy(_._2.value)
    val products = balancedEquation.right.map { (molec, coef) =>
      molec -> (coef * globalCoef)
    }
    val reactantsWithUsage = individuallyPossibleCoefs.map {
      case (molec, indivCoef) =>
        val amountAvailable = reactantsMap.getOrElse(molec, 0.mol)
        (molec, amountAvailable, if indivCoef == 0.mol then 0.0 else globalCoef / indivCoef)
    }
    Success(reactantsWithUsage, balancedEquation, efficiency, products)
  }

  private def computeBalancedEquation(): Option[BalancedEquation] = equation match {
    case equation: BalancedEquation => Some(equation)
    case equation: NoCoefEquation =>
      BalancedEquation.balancedFrom(equation)
    case _ =>
      throw AssertionError(s"unexpected ${equation.getClass.getName}")
  }

  private def findMissingReactant(balancedEquation: BalancedEquation): Option[Molecule] = {
    val leftMemberMap = balancedEquation.left.toMap
    reactants.find { (molec, amount) =>
      amount <= 0.mol && leftMemberMap.getOrElse(molec, 0) > 0
    }.map(_._1)
  }

}

object Reaction {

  enum Result {
    case Failure(msg: String)
    case Success(reactants: Seq[(Molecule, Mol, Double)], balancedEquation: BalancedEquation, efficiency: Double, products: List[(Molecule, Mol)])

    override def toString: String = this match {
      case Result.Failure(msg) => s"Reaction failed: $msg"
      case Result.Success(reactants, balancedEquation, efficiency, products) =>
        "Reaction succeeded:\n" +
          ("using reactants\n" +
            formatReactantsAmounts(reactants).indent(2) +
            "according to " + balancedEquation.toString + "\n" +
            f"with an efficiency of ${efficiency * 100}%.2f" + "%\n" +
            "the products are\n" +
            formatProductsAmounts(products).indent(2)).indent(2)
    }
  }

  private def formatReactantsAmounts(reactants: Seq[(Molecule, Mol, Double)]) = {
    reactants.map { (molec, amount, useProport) =>
      f"$molec: $amount (${amount * molec.mass}) of which ${useProport * 100}%.2f" + "% were used"
    }.mkString("\n")
  }

  private def formatProductsAmounts(products: Seq[(Molecule, Mol)]) = {
    products.map { (molec, amount) =>
      s"$molec: $amount (${amount * molec.mass})"
    }.mkString("\n")
  }
}
