package chemistry

import units.{U, u}

final case class Molecule(atoms: Map[Atom, Int]) {

  override def toString: String =
    atoms.toList
      .sortBy(_._1.elementSymbol)
      .map {
        case (atom, 1) => s"${atom.elementSymbol}"
        case (atom, coef) => s"${atom.elementSymbol}($coef)"
      }.mkString


  def mass: U = {
    var mass = 0.u
    for ((atom, coef) <- atoms) {
      mass += atom.atomicMass * coef
    }
    mass
  }

}
