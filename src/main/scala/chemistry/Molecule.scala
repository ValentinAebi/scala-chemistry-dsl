package chemistry

import units.{AtomicMassUnit, u}

final case class Molecule(atoms: Map[Atom, Int], charge: Int) {

  override def toString: String = {
    val strWithoutCharge = atoms.toList
      .sortBy(_._1.elementSymbol)
      .map {
        case (atom, 1) => s"${atom.elementSymbol}"
        case (atom, coef) => s"${atom.elementSymbol}($coef)"
      }.mkString
    val chargeStr = if charge == 0 then "" else if charge > 0 then s"^$charge+" else s"^${-charge}-"
    strWithoutCharge ++ chargeStr
  }


  def mass: AtomicMassUnit = {
    var mass = 0.u
    for ((atom, coef) <- atoms) {
      mass += atom.atomicMass * coef
    }
    mass
  }

}
