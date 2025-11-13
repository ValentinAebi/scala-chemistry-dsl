package dsl

import chemistry.{Molecule, NoCoefEquation}

extension (l: Molecule) def ==>(r: Molecule): NoCoefEquation =
  NoCoefEquation(List(l), List(r))

extension (l: Molecule) def &(r: NoCoefEquation): NoCoefEquation =
  r.copy(leftMember = l :: r.leftMember)

extension (l: NoCoefEquation) def &(r: Molecule): NoCoefEquation =
  l.copy(rightMember = l.rightMember :+ r)
