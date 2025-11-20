package chemistry

import chemistry.Reaction.Result.{Failure, Success}
import units.*

final case class Reaction(equation: Equation, reactants: Seq[(Molecule, Option[Mol])], efficiency: Double) {

  def react: Reaction.Result = {
    reactants.find((molec, amount) => amount.exists(_ < 0.mol)) match {
      case Some((molec, amount)) =>
        return Failure(s"negative amount of $molec")
      case _ => ()
    }
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
    val individuallyPossibleCoefs = balancedEquation.left.flatMap { (molec, coef) =>
      reactantsMap.apply(molec).map(availableAmount => molec -> efficiency * availableAmount / coef)
    }
    val (limitingReactant, globalCoef) = individuallyPossibleCoefs.minBy(_._2.value)
    val products = balancedEquation.right.map { (molec, coef) =>
      molec -> (coef * globalCoef)
    }
    val reactantsWithUsage = individuallyPossibleCoefs.map { (molec, indivCoef) =>
      val amountAvailable = reactantsMap.getOrElse(molec, Some(0.mol)).get
      (molec, Some(amountAvailable), if indivCoef == 0.mol then 0.0.mol else globalCoef / indivCoef * amountAvailable)
    } ++ balancedEquation.left
      .filter((molec, coef) => reactantsMap.apply(molec).isEmpty)
      .map((molec, coef) => (molec, None, globalCoef * coef))
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
    balancedEquation.left.find { (molec, coef) =>
      coef > 0 && reactants.toMap.get(molec).forall(_.exists(_ <= 0.mol))
    }.map(_._1)
  }

}

object Reaction {

  enum Result {
    case Failure(msg: String)
    case Success(reactants: Seq[(Molecule, Option[Mol], Mol)], balancedEquation: BalancedEquation, efficiency: Double, products: List[(Molecule, Mol)])

    def asSuccess: Success = this match {
      case failure: Failure =>
        throw AssertionError(failure.toString)
      case success: Success => success
    }

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

  private def formatReactantsAmounts(reactants: Seq[(Molecule, Option[Mol], Mol)]) = {
    reactants.map {
      case (molec, Some(amountAvailable), amountUsed) =>
        f"$molec: $amountAvailable (${amountAvailable * molec.mass}) of which ${amountUsed / amountAvailable * 100}%.2f" + "% were used"
      case (molec, None, amoundUsed) =>
        s"$molec: unlimited, $amoundUsed (${amoundUsed * molec.mass}) were used"
    }.mkString("\n")
  }

  private def formatProductsAmounts(products: Seq[(Molecule, Mol)]) = {
    products.map { (molec, amount) =>
      s"$molec: $amount (${amount * molec.mass})"
    }.mkString("\n")
  }
}
