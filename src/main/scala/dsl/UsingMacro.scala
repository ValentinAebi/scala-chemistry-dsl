package dsl

import chemistry.{Molecule, Reaction, parseMolecule}
import dsl.ReactionParam.ReactionEfficiency
import dsl.parameters.OutType
import units.{Mol, g, mol}

import scala.collection.mutable
import scala.quoted.*

inline def usingMacro[Res, PC <: ReactionParam | ReactionCommand[Res]](inline reactants: PC): Seq[(Molecule, Mol) | ReactionEfficiency | ReactionCommand[OutType[Res, PC]]] = {
  ${ usingMacroImplExternal('reactants) }
}

def usingMacroImplExternal[Res, PC <: ReactionParam | ReactionCommand[Res]](params: Expr[PC])(using quotes: Quotes, pcT: Type[PC], resT: Type[Res]): Expr[Seq[(Molecule, Mol) | ReactionEfficiency | ReactionCommand[OutType[Res, PC]]]] = {
  val alreadySeenMolecules = mutable.Set.empty[Molecule]

  def checkNotRepeated(molecStr: Expr[String], report: quotes.reflect.report.type): Unit = {
    val alreadyKnown = !alreadySeenMolecules.add(parseMolecule(molecStr.valueOrAbort))
    if (alreadyKnown) {
      report.errorAndAbort(s"amount of ${molecStr.valueOrAbort} is set more than once", params)
    }
  }

  def checkAmount(amountExpr: Expr[Double], report: quotes.reflect.report.type): Unit = {
    val amountOpt = amountExpr.value
    if (!amountOpt.exists(_ > 0)) {
      report.errorAndAbort("amount should be a positive literal", params)
    }
  }

  def analyzeParam(param: Expr[ReactionParam]): Expr[(Molecule, Mol) | ReactionEfficiency] = {
    println("analyzeParam: " + param.show) // TODO remove
    import quotes.reflect.report
    param match {
      case '{ ($amountExpr: Double).mol.of($molecStr: String) } =>
        checkAmount(amountExpr, report)
        val molecule = parseAndStaticCheckMoleculeMacroImpl(molecStr)
        checkNotRepeated(molecStr, report)
        '{ $molecule -> $amountExpr.mol }
      case '{ ($amountExpr: Double).g.of($molecStr: String) } =>
        checkAmount(amountExpr, report)
        val molecule = parseAndStaticCheckMoleculeMacroImpl(molecStr)
        checkNotRepeated(molecStr, report)
        '{ $molecule -> $amountExpr.g / $molecule.mass }
      case param@'{ efficiency == ($efficiencyPercentExpr: Double).percent } =>
        efficiencyPercentExpr.value match {
          case Some(efficiencyPercentVal) if 0 <= efficiencyPercentVal && efficiencyPercentVal <= 100 =>
            param
          case _ =>
            report.errorAndAbort("efficiency must be a percentage between 0.percent and 100.percent")
        }
      case _ => report.errorAndAbort("unrecognized parameter", params)
    }
  }

  def analyzeLeftSeq(params: Expr[ReactionParam]): Expr[Seq[(Molecule, Mol) | ReactionEfficiency]] = {
    println("analyzeLeftSeq: " + params.show) // TODO remove
    import quotes.reflect.report
    params match {
      case '{ ($r1: ReactionParam) ; ($r2: ReactionParam) } =>
        val left = analyzeLeftSeq(r1)
        val right = analyzeLeftSeq(r2)
        '{ $left ++ $right }
      case _ =>
        '{ Seq(${ analyzeParam(params) }) }
    }
  }

  def analyzeRightSeq(params: Expr[PC]): (Expr[Seq[(Molecule, Mol) | ReactionEfficiency]], Expr[ReactionCommand[OutType[Res, PC]]]) = {
    println("analyzeRightSeq: " + params.show) // TODO remove
    import quotes.reflect.report
    params match {
      case '{ ($r1: ReactionParam) ; ($r2: PC) } =>
        val left = analyzeLeftSeq(r1)
        val (right, cmd) = analyzeRightSeq(r2)
        ('{ $left ++ $right }, cmd)
      case params@'{ compute } =>
        ('{ Seq.empty }, params.asInstanceOf[Expr[ReactionCommand[OutType[Res, PC]]]])
      case params@'{ compute.and(display) } =>
        ('{ Seq.empty }, params.asInstanceOf[Expr[ReactionCommand[OutType[Res, PC]]]])
      case '{ $reactParam: ReactionParam } =>
        ('{ Seq(${ analyzeParam(reactParam) }) }, '{ ret }.asInstanceOf[Expr[ReactionCommand[OutType[Res, PC]]]])
      case _ =>
        report.errorAndAbort("unrecognized command", params)
    }
  }

  val (paramsExpr, cmdExpr) = analyzeRightSeq(params)
  '{ ${ paramsExpr } :+ ${ cmdExpr } }
}
