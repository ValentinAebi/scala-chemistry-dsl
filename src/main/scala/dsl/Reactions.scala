package dsl

import chemistry.{Equation, Molecule, Reaction}
import dsl.ReactionParam.ReactionEfficiency
import units.{Gram, Mol}

final case class ReactionParams(reactants: Seq[(Molecule, Mol)], efficiency: Double) {

  override def toString: String = {
    s"ReactionData {\n" +
      reactants.map((molec, amount) => s"$amount of $molec").mkString("\n  ") +
      s"efficiency = $efficiency\n" +
      "\n}"
  }

}

object reaction {
  infix def of(equation: Equation): ExpectAnd = ExpectAnd(equation)

  final case class ExpectAnd(private val equation: Equation) {
    infix def and[Out](p: (ReactionParams, ReactionCommand[Out])): Out = {
      val (reactionParams, cmd) = p
      val reaction = Reaction(equation, reactionParams.reactants, reactionParams.efficiency)
      cmd.applyTo(reaction)
    }
  }

}

object parameters {
  
  type OutType[Res, PC <: ReactionParam | ReactionCommand[Res]] = PC match {
    case ReactionParam => Reaction
    case ReactionCommand[Res] => Res
  }

  inline def apply[Res, PC <: ReactionParam | ReactionCommand[Res]](inline reactants: PC): (ReactionParams, ReactionCommand[OutType[Res, PC]]) = {
    val inputsB = Seq.newBuilder[(Molecule, Mol)]
    var efficiency = 1.0
    var command: ReactionCommand[?] = ret
    usingMacro(reactants) foreach {
      case p@(molec: Molecule, amount: Mol) =>
        inputsB.addOne(p)
      case ReactionEfficiency(eff) =>
        efficiency = eff
      case cmd: ReactionCommand[Res] =>
        command = cmd
    }
    (ReactionParams(inputsB.result(), efficiency), command.asInstanceOf[ReactionCommand[OutType[Res, PC]]])
  }

}

object equation {
  inline def ~(inline str: String): LeftMember = LeftMember(List(parseAndStaticCheckMolecule(str)))
}

enum ReactionParam {
  case GramReactant(moleculeStr: String, amount: Gram)
  case MolReactant(moleculeStr: String, amount: Mol)
  case ReactionEfficiency(efficiency: Double)
}

extension (amount: Mol) infix def of(moleculeStr: String): ReactionParam =
  throw new AssertionError()

extension (amount: Gram) infix def of(moleculeStr: String): ReactionParam =
  throw new AssertionError()

object efficiency {
  
  def ==(value: Double): ReactionEfficiency = ReactionEfficiency(value)
  
}

extension (value: Double) def percent: Double = value / 100

sealed trait ReactionCommand[Out] {
  def applyTo(reaction: Reaction): Out
}

case object ret extends ReactionCommand[Reaction] {
  override def applyTo(reaction: Reaction): Reaction = reaction
}

case object compute extends ReactionCommand[Reaction.Result] {
  infix def and(d: display.type): display.type = d

  override def applyTo(reaction: Reaction): Reaction.Result = reaction.react
}

case object display extends ReactionCommand[Reaction.Result] {
  override def applyTo(reaction: Reaction): Reaction.Result = {
    val result = reaction.react
    println(result)
    result
  }
}
