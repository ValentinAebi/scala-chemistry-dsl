package chemistry

import chemistry.Molecule

final case class NoCoefEquation(leftMember: List[Molecule], rightMember: List[Molecule]) extends Equation {

  override def leftMemberMolecules: Set[Molecule] = leftMember.toSet

  override def rightMemberMolecules: Set[Molecule] = rightMember.toSet

  def withAdditionalLeft(additionalMolecule: Molecule): NoCoefEquation =
    copy(leftMember = additionalMolecule :: leftMember)
    
  def withAdditionalRight(additionalMolecule: Molecule): NoCoefEquation =
    copy(rightMember = rightMember :+ additionalMolecule)

  override def toString: String = s"${leftMember.mkString(" + ")} ==> ${rightMember.mkString(" + ")}"
  
}
