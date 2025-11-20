package dsl

import chemistry.{Molecule, NoCoefEquation, parseMolecule}

import scala.quoted.{Expr, Quotes}

implicit inline def parseAndStaticCheckMolecule(inline str: String): Molecule = ${parseAndStaticCheckMoleculeMacroImpl('str)}

private def parseAndStaticCheckMoleculeMacroImpl(str: Expr[String])(using quotes: Quotes): Expr[Molecule] = {
  import quotes.reflect.report
  str.value match {
    case Some(value) =>
      try {
        parseMolecule(value)
      } catch {
        case MoleculeFormatException(msg) =>
          report.errorAndAbort(msg, str)
      }
      '{parseMolecule(${str})}
    case None =>
      report.errorAndAbort("an attempt to convert this String to a Molecule failed because it is not expressed as a literal", str)
  }
}
