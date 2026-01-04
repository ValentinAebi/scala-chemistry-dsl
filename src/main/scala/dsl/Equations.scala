package dsl

import chemistry.{Molecule, NoCoefEquation}
import dsl.parsing.parseAndStaticCheckMolecule

final case class LeftMember(molecules: List[Molecule])

object equation {
  inline def ~(inline str: String): LeftMember =
    LeftMember(List(parseAndStaticCheckMolecule(str)))
}

extension (inline l: LeftMember) inline def -->(inline r: Molecule): NoCoefEquation =
  NoCoefEquation(l.molecules, List(r))

extension (inline l: LeftMember) inline def -->(inline r: String): NoCoefEquation =
  l --> parseAndStaticCheckMolecule(r)

extension (inline l: LeftMember) inline def +(inline r: String): LeftMember =
  LeftMember(l.molecules :+ parseAndStaticCheckMolecule(r))

extension (inline l: NoCoefEquation) inline def +(inline r: String): NoCoefEquation =
  l.withAdditionalRight(parseAndStaticCheckMolecule(r))
