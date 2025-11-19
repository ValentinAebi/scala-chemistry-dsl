package dsl

import chemistry.{Molecule, NoCoefEquation, parseMolecule}

import scala.quoted.{Expr, Quotes}

inline def parseAndStaticCheckMolecule(inline str: String): Molecule = ${parseAndStaticCheckMoleculeMacroImpl('str)}

private def parseAndStaticCheckMoleculeMacroImpl(str: Expr[String])(using Quotes): Expr[Molecule] = {
  str.value match {
    case Some(value) =>
      parseMolecule(value)
      '{parseMolecule(${str})}
    case None =>
      throw MoleculeFormatException("an attempt to convert this String to a Molecule failed because it is not expressed as a literal")
  }
}
