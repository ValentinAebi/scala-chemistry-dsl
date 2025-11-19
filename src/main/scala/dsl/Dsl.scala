package dsl

import chemistry.{Molecule, NoCoefEquation, parseMolecule}

final case class LeftMember(molecules: List[Molecule])

extension (inline str: String) inline def unary_~ =
  LeftMember(List(parseAndStaticCheckMolecule(str)))

extension (inline l: LeftMember) inline def -->(inline r: String): NoCoefEquation =
  NoCoefEquation(l.molecules, List(parseAndStaticCheckMolecule(r)))

extension (inline l: LeftMember) inline def +(inline r: String): LeftMember =
  LeftMember(l.molecules :+ parseAndStaticCheckMolecule(r))

extension (inline l: NoCoefEquation) inline def +(inline r: String): NoCoefEquation =
  l.withAdditionalRight(parseAndStaticCheckMolecule(r))
