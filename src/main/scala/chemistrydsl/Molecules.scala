package chemistrydsl

import chemistry.{Atom, Molecule}

import scala.collection.mutable

sealed trait SyntacticMolecule {

  def toMolecule: Molecule = {
    val atomsB = mutable.Map.empty[Atom, Int]
    for ((atom, coef) <- atoms) {
      atomsB.updateWith(atom) { prevCoefOpt =>
        Some(prevCoefOpt.getOrElse(0) + coef)
      }
    }
    Molecule(atomsB.toMap)
  }
  
  def +(that: SyntacticMolecule) = NoCoefEquation.Member(List(this, that))
  def +(eqMember: NoCoefEquation.Member) = NoCoefEquation.Member(this +: eqMember.molecules)

  def atoms: List[(Atom, Int)]

  def H = ExtensibleMolecule(atoms, Atom.Hydrogen)

  def He = ExtensibleMolecule(atoms, Atom.Helium)

  def Li = ExtensibleMolecule(atoms, Atom.Lithium)

  def Be = ExtensibleMolecule(atoms, Atom.Beryllium)

  def B = ExtensibleMolecule(atoms, Atom.Boron)

  def C = ExtensibleMolecule(atoms, Atom.Carbon)

  def N = ExtensibleMolecule(atoms, Atom.Nitrogen)

  def O = ExtensibleMolecule(atoms, Atom.Oxygen)

  def F = ExtensibleMolecule(atoms, Atom.Fluorine)

  def Ne = ExtensibleMolecule(atoms, Atom.Neon)

  def Na = ExtensibleMolecule(atoms, Atom.Sodium)

  def Mg = ExtensibleMolecule(atoms, Atom.Magnesium)

  def Al = ExtensibleMolecule(atoms, Atom.Aluminum)

  def Si = ExtensibleMolecule(atoms, Atom.Silicon)

  def P = ExtensibleMolecule(atoms, Atom.Phosphorus)

  def S = ExtensibleMolecule(atoms, Atom.Sulfur)

  def Cl = ExtensibleMolecule(atoms, Atom.Chlorine)

  def Ar = ExtensibleMolecule(atoms, Atom.Argon)

  def K = ExtensibleMolecule(atoms, Atom.Potassium)

  def Ca = ExtensibleMolecule(atoms, Atom.Calcium)

  def Sc = ExtensibleMolecule(atoms, Atom.Scandium)

  def Ti = ExtensibleMolecule(atoms, Atom.Titanium)

  def V = ExtensibleMolecule(atoms, Atom.Vanadium)

  def Cr = ExtensibleMolecule(atoms, Atom.Chromium)

  def Mn = ExtensibleMolecule(atoms, Atom.Manganese)

  def Fe = ExtensibleMolecule(atoms, Atom.Iron)

  def Co = ExtensibleMolecule(atoms, Atom.Cobalt)

  def Ni = ExtensibleMolecule(atoms, Atom.Nickel)

  def Cu = ExtensibleMolecule(atoms, Atom.Copper)

  def Zn = ExtensibleMolecule(atoms, Atom.Zinc)

  def Ga = ExtensibleMolecule(atoms, Atom.Gallium)

  def Ge = ExtensibleMolecule(atoms, Atom.Germanium)

  def As = ExtensibleMolecule(atoms, Atom.Arsenic)

  def Se = ExtensibleMolecule(atoms, Atom.Selenium)

  def Br = ExtensibleMolecule(atoms, Atom.Bromine)

  def Kr = ExtensibleMolecule(atoms, Atom.Krypton)

  def Rb = ExtensibleMolecule(atoms, Atom.Rubidium)

  def Sr = ExtensibleMolecule(atoms, Atom.Strontium)

  def Y = ExtensibleMolecule(atoms, Atom.Yttrium)

  def Zr = ExtensibleMolecule(atoms, Atom.Zirconium)

  def Nb = ExtensibleMolecule(atoms, Atom.Niobium)

  def Mo = ExtensibleMolecule(atoms, Atom.Molybdenum)

  def Tc = ExtensibleMolecule(atoms, Atom.Technetium)

  def Ru = ExtensibleMolecule(atoms, Atom.Ruthenium)

  def Rh = ExtensibleMolecule(atoms, Atom.Rhodium)

  def Pd = ExtensibleMolecule(atoms, Atom.Palladium)

  def Ag = ExtensibleMolecule(atoms, Atom.Silver)

  def Cd = ExtensibleMolecule(atoms, Atom.Cadmium)

  def In = ExtensibleMolecule(atoms, Atom.Indium)

  def Sn = ExtensibleMolecule(atoms, Atom.Tin)

  def Sb = ExtensibleMolecule(atoms, Atom.Antimony)

  def Te = ExtensibleMolecule(atoms, Atom.Tellurium)

  def I = ExtensibleMolecule(atoms, Atom.Iodine)

  def Xe = ExtensibleMolecule(atoms, Atom.Xenon)

  def Cs = ExtensibleMolecule(atoms, Atom.Cesium)

  def Ba = ExtensibleMolecule(atoms, Atom.Barium)

  def La = ExtensibleMolecule(atoms, Atom.Lanthanum)

  def Ce = ExtensibleMolecule(atoms, Atom.Cerium)

  def Pr = ExtensibleMolecule(atoms, Atom.Praseodymium)

  def Nd = ExtensibleMolecule(atoms, Atom.Neodymium)

  def Pm = ExtensibleMolecule(atoms, Atom.Promethium)

  def Sm = ExtensibleMolecule(atoms, Atom.Samarium)

  def Eu = ExtensibleMolecule(atoms, Atom.Europium)

  def Gd = ExtensibleMolecule(atoms, Atom.Gadolinium)

  def Tb = ExtensibleMolecule(atoms, Atom.Terbium)

  def Dy = ExtensibleMolecule(atoms, Atom.Dysprosium)

  def Ho = ExtensibleMolecule(atoms, Atom.Holmium)

  def Er = ExtensibleMolecule(atoms, Atom.Erbium)

  def Tm = ExtensibleMolecule(atoms, Atom.Thulium)

  def Yb = ExtensibleMolecule(atoms, Atom.Ytterbium)

  def Lu = ExtensibleMolecule(atoms, Atom.Lutetium)

  def Hf = ExtensibleMolecule(atoms, Atom.Hafnium)

  def Ta = ExtensibleMolecule(atoms, Atom.Tantalum)

  def W = ExtensibleMolecule(atoms, Atom.Wolfram)

  def Re = ExtensibleMolecule(atoms, Atom.Rhenium)

  def Os = ExtensibleMolecule(atoms, Atom.Osmium)

  def Ir = ExtensibleMolecule(atoms, Atom.Iridium)

  def Pt = ExtensibleMolecule(atoms, Atom.Platinum)

  def Au = ExtensibleMolecule(atoms, Atom.Gold)

  def Hg = ExtensibleMolecule(atoms, Atom.Mercury)

  def Tl = ExtensibleMolecule(atoms, Atom.Thallium)

  def Pb = ExtensibleMolecule(atoms, Atom.Lead)

  def Bi = ExtensibleMolecule(atoms, Atom.Bismuth)

  def Po = ExtensibleMolecule(atoms, Atom.Polonium)

  def At = ExtensibleMolecule(atoms, Atom.Astatine)

  def Rn = ExtensibleMolecule(atoms, Atom.Radon)

  def Fr = ExtensibleMolecule(atoms, Atom.Francium)

  def Ra = ExtensibleMolecule(atoms, Atom.Radium)

  def Ac = ExtensibleMolecule(atoms, Atom.Actinium)

  def Th = ExtensibleMolecule(atoms, Atom.Thorium)

  def Pa = ExtensibleMolecule(atoms, Atom.Protactinium)

  def U = ExtensibleMolecule(atoms, Atom.Uranium)

  def Np = ExtensibleMolecule(atoms, Atom.Neptunium)

  def Pu = ExtensibleMolecule(atoms, Atom.Plutonium)

  def Am = ExtensibleMolecule(atoms, Atom.Americium)

  def Cm = ExtensibleMolecule(atoms, Atom.Curium)

  def Bk = ExtensibleMolecule(atoms, Atom.Berkelium)

  def Cf = ExtensibleMolecule(atoms, Atom.Californium)

  def Es = ExtensibleMolecule(atoms, Atom.Einsteinium)

  def Fm = ExtensibleMolecule(atoms, Atom.Fermium)

  def Md = ExtensibleMolecule(atoms, Atom.Mendelevium)

  def No = ExtensibleMolecule(atoms, Atom.Nobelium)

  def Lr = ExtensibleMolecule(atoms, Atom.Lawrencium)

  def Rf = ExtensibleMolecule(atoms, Atom.Rutherfordium)

  def Db = ExtensibleMolecule(atoms, Atom.Dubnium)

  def Sg = ExtensibleMolecule(atoms, Atom.Seaborgium)

  def Bh = ExtensibleMolecule(atoms, Atom.Bohrium)

  def Hs = ExtensibleMolecule(atoms, Atom.Hassium)

  def Mt = ExtensibleMolecule(atoms, Atom.Meitnerium)

  def Ds = ExtensibleMolecule(atoms, Atom.Darmstadtium)

  def Rg = ExtensibleMolecule(atoms, Atom.Roentgenium)

  def Cn = ExtensibleMolecule(atoms, Atom.Copernicium)

  def Nh = ExtensibleMolecule(atoms, Atom.Nihonium)

  def Fl = ExtensibleMolecule(atoms, Atom.Flerovium)

  def Mc = ExtensibleMolecule(atoms, Atom.Moscovium)

  def Lv = ExtensibleMolecule(atoms, Atom.Livermorium)

  def Ts = ExtensibleMolecule(atoms, Atom.Tennessine)

  def Og = ExtensibleMolecule(atoms, Atom.Oganesson)
}

final case class UnextensibleMolecule(atoms: List[(Atom, Int)]) extends SyntacticMolecule

final case class ExtensibleMolecule(unextensibleAtoms: List[(Atom, Int)], pendingAtom: Atom) extends SyntacticMolecule {

  override def atoms: List[(Atom, Int)] = unextensibleAtoms :+ (pendingAtom, 1)

  def apply(coef: Int): UnextensibleMolecule = UnextensibleMolecule(unextensibleAtoms :+ (pendingAtom, coef))

}
