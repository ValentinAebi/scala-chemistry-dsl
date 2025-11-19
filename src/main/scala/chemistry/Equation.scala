package chemistry

trait Equation {
  
  def leftMemberMolecules: Set[Molecule]
  def rightMemberMolecules: Set[Molecule]
  
  def allMolecules: Set[Molecule] = leftMemberMolecules ++ rightMemberMolecules
  
  def allAtoms: Set[Atom] = allMolecules.flatMap(_.atoms.keys)
  
}
