package chemistry

import chemistry.Reaction.Result.{Failure, IncompatibleConstraints, Success}
import units.*

import scala.util.boundary

final case class Reaction(
                           equation: Equation,
                           reactants: Map[Molecule, Mol],
                           products: Map[Molecule, Mol],
                           efficiency: Double
                         ) {

  def react: Reaction.Result = boundary {
    val balancedEquation = computeBalancedEquation() match {
      case None =>
        boundary.break(Failure("could not balance equation"))
      case Some(balancedEquation) => balancedEquation
    }
    reactants.keys.find(balancedEquation.leftCoefOf(_) == 0).foreach { molec =>
      boundary.break(Failure(s"no reactant $molec in equation"))
    }
    products.keys.find(balancedEquation.rightCoefOf(_) == 0).foreach { molec =>
      boundary.break(Failure(s"no product $molec in equation"))
    }
    val reactantsLimit = reactants.map { (molec, mol) =>
      molec -> mol / balancedEquation.leftCoefOf(molec)
    }.minByOption(_._2)
    val productsLimit = products.map { (molec, mol) =>
      molec -> (mol / balancedEquation.rightCoefOf(molec))
    }.maxByOption(_._2)
    (reactantsLimit, productsLimit) match {
      case (Some((limitingReactant, minMulInReactants)), Some((mostDemandingProduct, maxMulInProductsWithPerfectEfficiency)))
        if minMulInReactants * efficiency < maxMulInProductsWithPerfectEfficiency
      => IncompatibleConstraints(balancedEquation, limitingReactant,
        maxMulInProductsWithPerfectEfficiency * balancedEquation.leftCoefOf(limitingReactant) / efficiency)
      case (_, Some((mostDemandingProduct, maxMulInProductsWithPerfectEfficiency))) =>
        mkSuccess(balancedEquation, maxMulInProductsWithPerfectEfficiency / efficiency, None)
      case (Some((limitingReactant, minMulInReactants)), None) =>
        mkSuccess(balancedEquation, minMulInReactants, Some(limitingReactant))
      case (None, None) =>
        Failure("no amount set for neither reactants nor products")
    }
  }

  private def computeBalancedEquation(): Option[BalancedEquation] = equation match {
    case equation: BalancedEquation => Some(equation)
    case equation: NoCoefEquation =>
      BalancedEquation.balancedFrom(equation)
    case _ =>
      throw AssertionError(s"unexpected ${equation.getClass.getName}")
  }

  private def mkSuccess(balancedEquation: BalancedEquation, reactionMul: Mol, limitingReactantOpt: Option[Molecule]): Success = Success(
    balancedEquation,
    balancedEquation.left.map { (reactant, coef) =>
      val consumed = reactionMul * coef
      (reactant, reactants.get(reactant), consumed)
    },
    balancedEquation.right.map { (product, coef) =>
      val produced = reactionMul * coef * efficiency
      (product, produced)
    },
    limitingReactantOpt,
    efficiency
  )

}

object Reaction {

  enum Result {
    case Failure(msg: String)
    case BalancedOnly(balancedEquation: BalancedEquation, msg: String)
    case IncompatibleConstraints(
                                  balancedEquation: BalancedEquation,
                                  limitingReactant: Molecule,
                                  requiredAmount: Mol
                                )
    case Success(
                  balancedEquation: BalancedEquation,
                  reactants: Seq[(Molecule, Option[Mol], Mol)],
                  products: List[(Molecule, Mol)],
                  limitingReactantOpt: Option[Molecule],
                  efficiency: Double
                )

    def asSuccess: Success = this match {
      case success: Success => success
      case _ =>
        throw AssertionError(toString)
    }

    override def toString: String = this match {
      case Result.Failure(msg) =>
        s"Reaction failed: $msg"
      case Result.BalancedOnly(balancedEquation, msg) =>
        s"$msg\nBalanced equation: $balancedEquation"
      case Result.IncompatibleConstraints(balancedEquation, limitingReactant, requiredAmount) =>
        val requiredMass = requiredAmount * limitingReactant.mass
        s"The desired amount of products cannot be obtained with less than $requiredMass ($requiredAmount) of $limitingReactant" +
          s"\nBalanced equation: $balancedEquation"
      case Result.Success(balancedEquation, reactants, products, limitingReactantOpt, efficiency) =>
        "Computation succeeded:\n" +
          ("using reactants\n" +
            formatReactantsAmounts(reactants).indent(2) +
            "according to " + balancedEquation.toString + "\n" +
            f"with an efficiency of ${efficiency * 100}%.2f" + "%\n" +
            limitingReactantOpt.map(lr => s"limiting reactant is $lr\n").getOrElse("") +
            "the products are\n" +
            formatProductsAmounts(products).indent(2)).indent(2)
    }
  }

  private def formatReactantsAmounts(reactants: Seq[(Molecule, Option[Mol], Mol)]) = {
    reactants.map {
      case (molec, Some(amountAvailable), amountUsed) =>
        f"$molec: $amountAvailable (${amountAvailable * molec.mass}) of which ${amountUsed / amountAvailable * 100}%.2f" + "% were used"
      case (molec, None, amoundUsed) =>
        s"$molec: $amoundUsed (${amoundUsed * molec.mass})"
    }.mkString("\n")
  }

  private def formatProductsAmounts(products: Seq[(Molecule, Mol)]) = {
    products.map { (molec, amount) =>
      s"$molec: $amount (${amount * molec.mass})"
    }.mkString("\n")
  }
}
