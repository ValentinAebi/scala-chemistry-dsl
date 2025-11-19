package chemistry

import chemistry.Molecule

final case class NoCoefEquation(leftMember: List[Molecule], rightMember: List[Molecule]) {
  
  def withAdditionalLeft(additionalMolecule: Molecule): NoCoefEquation =
    copy(leftMember = additionalMolecule :: leftMember)
    
  def withAdditionalRight(additionalMolecule: Molecule): NoCoefEquation =
    copy(rightMember = rightMember :+ additionalMolecule)

  override def toString: String = s"${leftMember.mkString(" + ")} ==> ${rightMember.mkString(" + ")}"
  
  def allAtoms: Set[Atom] = {
    val atomsB = Set.newBuilder[Atom]
    for (molec <- leftMember){
      atomsB.addAll(molec.atoms.keys)
    }
    for (molec <- rightMember){
      atomsB.addAll(molec.atoms.keys)
    }
    atomsB.result()
  }
  
}
