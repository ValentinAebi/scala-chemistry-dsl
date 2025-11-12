package dsl

import chemistry.{Atom, Molecule, parseMolecule}

import scala.quoted.{Expr, Quotes}

object ImplicitMoleculeParsing {

  // seems necessary to use the old style: https://users.scala-lang.org/t/given-style-implicit-conversion-with-inline-function/7536/2
  implicit inline def str2Molecule(str: String): Molecule = ${str2MoleculeMacroImpl('str)}

  private def str2MoleculeMacroImpl(str: Expr[String])(using Quotes): Expr[Molecule] = {
    str.value match {
      case Some(value) =>
        parseMolecule(value)
        '{parseMolecule(${str})}
      case None =>
        throw MoleculeFormatException("an attempt to convert this String to a Molecule failed because it is not expressed as a literal")
    }
  }

}
