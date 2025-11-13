package chemistry

import chemistry.Molecule

final case class NoCoefEquation(leftMember: List[Molecule], rightMember: List[Molecule]) {

  override def toString: String = s"${leftMember.mkString(" & ")} ==> ${rightMember.mkString(" & ")}"
  
}
