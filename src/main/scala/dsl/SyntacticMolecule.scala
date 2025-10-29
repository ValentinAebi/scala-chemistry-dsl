package dsl

import chemistry.{Atom, Molecule}

import scala.collection.mutable

final case class SyntacticMolecule(components: List[(Atom | SyntacticMolecule, Int)]) {

  def toMolecule: Molecule = {
    val atomsB = mutable.Map.empty[Atom, Int]

    def add(atom: Atom, coef: Int): Unit = {
      atomsB.updateWith(atom) { prevCoefOpt =>
        Some(prevCoefOpt.getOrElse(0) + coef)
      }
    }

    components.foreach {
      case (atom: Atom, coef) => add(atom, coef)
      case (molecule: SyntacticMolecule, superCoef) =>
        for ((atom, coef) <- molecule.toMolecule.atoms){
          add(atom, coef * superCoef)
        }
    }
    Molecule(atomsB.toMap)
  }

  def +(that: SyntacticMolecule) = NoCoefEquation.Member(List(this, that))
  def +(eqMember: NoCoefEquation.Member) = NoCoefEquation.Member(this +: eqMember.molecules)
  
  def !(coef: Int) = SyntacticMolecule(List(this -> coef))
  
  def apply(that: SyntacticMolecule) = SyntacticMolecule(this.components ++ that.components)

  def H(coef: Int): SyntacticMolecule = SyntacticMolecule(components :+ (Atom.Hydrogen, coef))

  def He(coef: Int): SyntacticMolecule = SyntacticMolecule(components :+ (Atom.Helium, coef))

  def Li(coef: Int): SyntacticMolecule = SyntacticMolecule(components :+ (Atom.Lithium, coef))

  def Be(coef: Int): SyntacticMolecule = SyntacticMolecule(components :+ (Atom.Beryllium, coef))

  def B(coef: Int): SyntacticMolecule = SyntacticMolecule(components :+ (Atom.Boron, coef))

  def C(coef: Int): SyntacticMolecule = SyntacticMolecule(components :+ (Atom.Carbon, coef))

  def N(coef: Int): SyntacticMolecule = SyntacticMolecule(components :+ (Atom.Nitrogen, coef))

  def O(coef: Int): SyntacticMolecule = SyntacticMolecule(components :+ (Atom.Oxygen, coef))

  def F(coef: Int): SyntacticMolecule = SyntacticMolecule(components :+ (Atom.Fluorine, coef))

  def Ne(coef: Int): SyntacticMolecule = SyntacticMolecule(components :+ (Atom.Neon, coef))

  def Na(coef: Int): SyntacticMolecule = SyntacticMolecule(components :+ (Atom.Sodium, coef))

  def Mg(coef: Int): SyntacticMolecule = SyntacticMolecule(components :+ (Atom.Magnesium, coef))

  def Al(coef: Int): SyntacticMolecule = SyntacticMolecule(components :+ (Atom.Aluminum, coef))

  def Si(coef: Int): SyntacticMolecule = SyntacticMolecule(components :+ (Atom.Silicon, coef))

  def P(coef: Int): SyntacticMolecule = SyntacticMolecule(components :+ (Atom.Phosphorus, coef))

  def S(coef: Int): SyntacticMolecule = SyntacticMolecule(components :+ (Atom.Sulfur, coef))

  def Cl(coef: Int): SyntacticMolecule = SyntacticMolecule(components :+ (Atom.Chlorine, coef))

  def Ar(coef: Int): SyntacticMolecule = SyntacticMolecule(components :+ (Atom.Argon, coef))

  def K(coef: Int): SyntacticMolecule = SyntacticMolecule(components :+ (Atom.Potassium, coef))

  def Ca(coef: Int): SyntacticMolecule = SyntacticMolecule(components :+ (Atom.Calcium, coef))

  def Sc(coef: Int): SyntacticMolecule = SyntacticMolecule(components :+ (Atom.Scandium, coef))

  def Ti(coef: Int): SyntacticMolecule = SyntacticMolecule(components :+ (Atom.Titanium, coef))

  def V(coef: Int): SyntacticMolecule = SyntacticMolecule(components :+ (Atom.Vanadium, coef))

  def Cr(coef: Int): SyntacticMolecule = SyntacticMolecule(components :+ (Atom.Chromium, coef))

  def Mn(coef: Int): SyntacticMolecule = SyntacticMolecule(components :+ (Atom.Manganese, coef))

  def Fe(coef: Int): SyntacticMolecule = SyntacticMolecule(components :+ (Atom.Iron, coef))

  def Co(coef: Int): SyntacticMolecule = SyntacticMolecule(components :+ (Atom.Cobalt, coef))

  def Ni(coef: Int): SyntacticMolecule = SyntacticMolecule(components :+ (Atom.Nickel, coef))

  def Cu(coef: Int): SyntacticMolecule = SyntacticMolecule(components :+ (Atom.Copper, coef))

  def Zn(coef: Int): SyntacticMolecule = SyntacticMolecule(components :+ (Atom.Zinc, coef))

  def Ga(coef: Int): SyntacticMolecule = SyntacticMolecule(components :+ (Atom.Gallium, coef))

  def Ge(coef: Int): SyntacticMolecule = SyntacticMolecule(components :+ (Atom.Germanium, coef))

  def As(coef: Int): SyntacticMolecule = SyntacticMolecule(components :+ (Atom.Arsenic, coef))

  def Se(coef: Int): SyntacticMolecule = SyntacticMolecule(components :+ (Atom.Selenium, coef))

  def Br(coef: Int): SyntacticMolecule = SyntacticMolecule(components :+ (Atom.Bromine, coef))

  def Kr(coef: Int): SyntacticMolecule = SyntacticMolecule(components :+ (Atom.Krypton, coef))

  def Rb(coef: Int): SyntacticMolecule = SyntacticMolecule(components :+ (Atom.Rubidium, coef))

  def Sr(coef: Int): SyntacticMolecule = SyntacticMolecule(components :+ (Atom.Strontium, coef))

  def Y(coef: Int): SyntacticMolecule = SyntacticMolecule(components :+ (Atom.Yttrium, coef))

  def Zr(coef: Int): SyntacticMolecule = SyntacticMolecule(components :+ (Atom.Zirconium, coef))

  def Nb(coef: Int): SyntacticMolecule = SyntacticMolecule(components :+ (Atom.Niobium, coef))

  def Mo(coef: Int): SyntacticMolecule = SyntacticMolecule(components :+ (Atom.Molybdenum, coef))

  def Tc(coef: Int): SyntacticMolecule = SyntacticMolecule(components :+ (Atom.Technetium, coef))

  def Ru(coef: Int): SyntacticMolecule = SyntacticMolecule(components :+ (Atom.Ruthenium, coef))

  def Rh(coef: Int): SyntacticMolecule = SyntacticMolecule(components :+ (Atom.Rhodium, coef))

  def Pd(coef: Int): SyntacticMolecule = SyntacticMolecule(components :+ (Atom.Palladium, coef))

  def Ag(coef: Int): SyntacticMolecule = SyntacticMolecule(components :+ (Atom.Silver, coef))

  def Cd(coef: Int): SyntacticMolecule = SyntacticMolecule(components :+ (Atom.Cadmium, coef))

  def In(coef: Int): SyntacticMolecule = SyntacticMolecule(components :+ (Atom.Indium, coef))

  def Sn(coef: Int): SyntacticMolecule = SyntacticMolecule(components :+ (Atom.Tin, coef))

  def Sb(coef: Int): SyntacticMolecule = SyntacticMolecule(components :+ (Atom.Antimony, coef))

  def Te(coef: Int): SyntacticMolecule = SyntacticMolecule(components :+ (Atom.Tellurium, coef))

  def I(coef: Int): SyntacticMolecule = SyntacticMolecule(components :+ (Atom.Iodine, coef))

  def Xe(coef: Int): SyntacticMolecule = SyntacticMolecule(components :+ (Atom.Xenon, coef))

  def Cs(coef: Int): SyntacticMolecule = SyntacticMolecule(components :+ (Atom.Cesium, coef))

  def Ba(coef: Int): SyntacticMolecule = SyntacticMolecule(components :+ (Atom.Barium, coef))

  def La(coef: Int): SyntacticMolecule = SyntacticMolecule(components :+ (Atom.Lanthanum, coef))

  def Ce(coef: Int): SyntacticMolecule = SyntacticMolecule(components :+ (Atom.Cerium, coef))

  def Pr(coef: Int): SyntacticMolecule = SyntacticMolecule(components :+ (Atom.Praseodymium, coef))

  def Nd(coef: Int): SyntacticMolecule = SyntacticMolecule(components :+ (Atom.Neodymium, coef))

  def Pm(coef: Int): SyntacticMolecule = SyntacticMolecule(components :+ (Atom.Promethium, coef))

  def Sm(coef: Int): SyntacticMolecule = SyntacticMolecule(components :+ (Atom.Samarium, coef))

  def Eu(coef: Int): SyntacticMolecule = SyntacticMolecule(components :+ (Atom.Europium, coef))

  def Gd(coef: Int): SyntacticMolecule = SyntacticMolecule(components :+ (Atom.Gadolinium, coef))

  def Tb(coef: Int): SyntacticMolecule = SyntacticMolecule(components :+ (Atom.Terbium, coef))

  def Dy(coef: Int): SyntacticMolecule = SyntacticMolecule(components :+ (Atom.Dysprosium, coef))

  def Ho(coef: Int): SyntacticMolecule = SyntacticMolecule(components :+ (Atom.Holmium, coef))

  def Er(coef: Int): SyntacticMolecule = SyntacticMolecule(components :+ (Atom.Erbium, coef))

  def Tm(coef: Int): SyntacticMolecule = SyntacticMolecule(components :+ (Atom.Thulium, coef))

  def Yb(coef: Int): SyntacticMolecule = SyntacticMolecule(components :+ (Atom.Ytterbium, coef))

  def Lu(coef: Int): SyntacticMolecule = SyntacticMolecule(components :+ (Atom.Lutetium, coef))

  def Hf(coef: Int): SyntacticMolecule = SyntacticMolecule(components :+ (Atom.Hafnium, coef))

  def Ta(coef: Int): SyntacticMolecule = SyntacticMolecule(components :+ (Atom.Tantalum, coef))

  def W(coef: Int): SyntacticMolecule = SyntacticMolecule(components :+ (Atom.Wolfram, coef))

  def Re(coef: Int): SyntacticMolecule = SyntacticMolecule(components :+ (Atom.Rhenium, coef))

  def Os(coef: Int): SyntacticMolecule = SyntacticMolecule(components :+ (Atom.Osmium, coef))

  def Ir(coef: Int): SyntacticMolecule = SyntacticMolecule(components :+ (Atom.Iridium, coef))

  def Pt(coef: Int): SyntacticMolecule = SyntacticMolecule(components :+ (Atom.Platinum, coef))

  def Au(coef: Int): SyntacticMolecule = SyntacticMolecule(components :+ (Atom.Gold, coef))

  def Hg(coef: Int): SyntacticMolecule = SyntacticMolecule(components :+ (Atom.Mercury, coef))

  def Tl(coef: Int): SyntacticMolecule = SyntacticMolecule(components :+ (Atom.Thallium, coef))

  def Pb(coef: Int): SyntacticMolecule = SyntacticMolecule(components :+ (Atom.Lead, coef))

  def Bi(coef: Int): SyntacticMolecule = SyntacticMolecule(components :+ (Atom.Bismuth, coef))

  def Po(coef: Int): SyntacticMolecule = SyntacticMolecule(components :+ (Atom.Polonium, coef))

  def At(coef: Int): SyntacticMolecule = SyntacticMolecule(components :+ (Atom.Astatine, coef))

  def Rn(coef: Int): SyntacticMolecule = SyntacticMolecule(components :+ (Atom.Radon, coef))

  def Fr(coef: Int): SyntacticMolecule = SyntacticMolecule(components :+ (Atom.Francium, coef))

  def Ra(coef: Int): SyntacticMolecule = SyntacticMolecule(components :+ (Atom.Radium, coef))

  def Ac(coef: Int): SyntacticMolecule = SyntacticMolecule(components :+ (Atom.Actinium, coef))

  def Th(coef: Int): SyntacticMolecule = SyntacticMolecule(components :+ (Atom.Thorium, coef))

  def Pa(coef: Int): SyntacticMolecule = SyntacticMolecule(components :+ (Atom.Protactinium, coef))

  def U(coef: Int): SyntacticMolecule = SyntacticMolecule(components :+ (Atom.Uranium, coef))

  def Np(coef: Int): SyntacticMolecule = SyntacticMolecule(components :+ (Atom.Neptunium, coef))

  def Pu(coef: Int): SyntacticMolecule = SyntacticMolecule(components :+ (Atom.Plutonium, coef))

  def Am(coef: Int): SyntacticMolecule = SyntacticMolecule(components :+ (Atom.Americium, coef))

  def Cm(coef: Int): SyntacticMolecule = SyntacticMolecule(components :+ (Atom.Curium, coef))

  def Bk(coef: Int): SyntacticMolecule = SyntacticMolecule(components :+ (Atom.Berkelium, coef))

  def Cf(coef: Int): SyntacticMolecule = SyntacticMolecule(components :+ (Atom.Californium, coef))

  def Es(coef: Int): SyntacticMolecule = SyntacticMolecule(components :+ (Atom.Einsteinium, coef))

  def Fm(coef: Int): SyntacticMolecule = SyntacticMolecule(components :+ (Atom.Fermium, coef))

  def Md(coef: Int): SyntacticMolecule = SyntacticMolecule(components :+ (Atom.Mendelevium, coef))

  def No(coef: Int): SyntacticMolecule = SyntacticMolecule(components :+ (Atom.Nobelium, coef))

  def Lr(coef: Int): SyntacticMolecule = SyntacticMolecule(components :+ (Atom.Lawrencium, coef))

  def Rf(coef: Int): SyntacticMolecule = SyntacticMolecule(components :+ (Atom.Rutherfordium, coef))

  def Db(coef: Int): SyntacticMolecule = SyntacticMolecule(components :+ (Atom.Dubnium, coef))

  def Sg(coef: Int): SyntacticMolecule = SyntacticMolecule(components :+ (Atom.Seaborgium, coef))

  def Bh(coef: Int): SyntacticMolecule = SyntacticMolecule(components :+ (Atom.Bohrium, coef))

  def Hs(coef: Int): SyntacticMolecule = SyntacticMolecule(components :+ (Atom.Hassium, coef))

  def Mt(coef: Int): SyntacticMolecule = SyntacticMolecule(components :+ (Atom.Meitnerium, coef))

  def Ds(coef: Int): SyntacticMolecule = SyntacticMolecule(components :+ (Atom.Darmstadtium, coef))

  def Rg(coef: Int): SyntacticMolecule = SyntacticMolecule(components :+ (Atom.Roentgenium, coef))

  def Cn(coef: Int): SyntacticMolecule = SyntacticMolecule(components :+ (Atom.Copernicium, coef))

  def Nh(coef: Int): SyntacticMolecule = SyntacticMolecule(components :+ (Atom.Nihonium, coef))

  def Fl(coef: Int): SyntacticMolecule = SyntacticMolecule(components :+ (Atom.Flerovium, coef))

  def Mc(coef: Int): SyntacticMolecule = SyntacticMolecule(components :+ (Atom.Moscovium, coef))

  def Lv(coef: Int): SyntacticMolecule = SyntacticMolecule(components :+ (Atom.Livermorium, coef))

  def Ts(coef: Int): SyntacticMolecule = SyntacticMolecule(components :+ (Atom.Tennessine, coef))

  def Og(coef: Int): SyntacticMolecule = SyntacticMolecule(components :+ (Atom.Oganesson, coef))

  def H: SyntacticMolecule = H(1)

  def He: SyntacticMolecule = He(1)

  def Li: SyntacticMolecule = Li(1)

  def Be: SyntacticMolecule = Be(1)

  def B: SyntacticMolecule = B(1)

  def C: SyntacticMolecule = C(1)

  def N: SyntacticMolecule = N(1)

  def O: SyntacticMolecule = O(1)

  def F: SyntacticMolecule = F(1)

  def Ne: SyntacticMolecule = Ne(1)

  def Na: SyntacticMolecule = Na(1)

  def Mg: SyntacticMolecule = Mg(1)

  def Al: SyntacticMolecule = Al(1)

  def Si: SyntacticMolecule = Si(1)

  def P: SyntacticMolecule = P(1)

  def S: SyntacticMolecule = S(1)

  def Cl: SyntacticMolecule = Cl(1)

  def Ar: SyntacticMolecule = Ar(1)

  def K: SyntacticMolecule = K(1)

  def Ca: SyntacticMolecule = Ca(1)

  def Sc: SyntacticMolecule = Sc(1)

  def Ti: SyntacticMolecule = Ti(1)

  def V: SyntacticMolecule = V(1)

  def Cr: SyntacticMolecule = Cr(1)

  def Mn: SyntacticMolecule = Mn(1)

  def Fe: SyntacticMolecule = Fe(1)

  def Co: SyntacticMolecule = Co(1)

  def Ni: SyntacticMolecule = Ni(1)

  def Cu: SyntacticMolecule = Cu(1)

  def Zn: SyntacticMolecule = Zn(1)

  def Ga: SyntacticMolecule = Ga(1)

  def Ge: SyntacticMolecule = Ge(1)

  def As: SyntacticMolecule = As(1)

  def Se: SyntacticMolecule = Se(1)

  def Br: SyntacticMolecule = Br(1)

  def Kr: SyntacticMolecule = Kr(1)

  def Rb: SyntacticMolecule = Rb(1)

  def Sr: SyntacticMolecule = Sr(1)

  def Y: SyntacticMolecule = Y(1)

  def Zr: SyntacticMolecule = Zr(1)

  def Nb: SyntacticMolecule = Nb(1)

  def Mo: SyntacticMolecule = Mo(1)

  def Tc: SyntacticMolecule = Tc(1)

  def Ru: SyntacticMolecule = Ru(1)

  def Rh: SyntacticMolecule = Rh(1)

  def Pd: SyntacticMolecule = Pd(1)

  def Ag: SyntacticMolecule = Ag(1)

  def Cd: SyntacticMolecule = Cd(1)

  def In: SyntacticMolecule = In(1)

  def Sn: SyntacticMolecule = Sn(1)

  def Sb: SyntacticMolecule = Sb(1)

  def Te: SyntacticMolecule = Te(1)

  def I: SyntacticMolecule = I(1)

  def Xe: SyntacticMolecule = Xe(1)

  def Cs: SyntacticMolecule = Cs(1)

  def Ba: SyntacticMolecule = Ba(1)

  def La: SyntacticMolecule = La(1)

  def Ce: SyntacticMolecule = Ce(1)

  def Pr: SyntacticMolecule = Pr(1)

  def Nd: SyntacticMolecule = Nd(1)

  def Pm: SyntacticMolecule = Pm(1)

  def Sm: SyntacticMolecule = Sm(1)

  def Eu: SyntacticMolecule = Eu(1)

  def Gd: SyntacticMolecule = Gd(1)

  def Tb: SyntacticMolecule = Tb(1)

  def Dy: SyntacticMolecule = Dy(1)

  def Ho: SyntacticMolecule = Ho(1)

  def Er: SyntacticMolecule = Er(1)

  def Tm: SyntacticMolecule = Tm(1)

  def Yb: SyntacticMolecule = Yb(1)

  def Lu: SyntacticMolecule = Lu(1)

  def Hf: SyntacticMolecule = Hf(1)

  def Ta: SyntacticMolecule = Ta(1)

  def W: SyntacticMolecule = W(1)

  def Re: SyntacticMolecule = Re(1)

  def Os: SyntacticMolecule = Os(1)

  def Ir: SyntacticMolecule = Ir(1)

  def Pt: SyntacticMolecule = Pt(1)

  def Au: SyntacticMolecule = Au(1)

  def Hg: SyntacticMolecule = Hg(1)

  def Tl: SyntacticMolecule = Tl(1)

  def Pb: SyntacticMolecule = Pb(1)

  def Bi: SyntacticMolecule = Bi(1)

  def Po: SyntacticMolecule = Po(1)

  def At: SyntacticMolecule = At(1)

  def Rn: SyntacticMolecule = Rn(1)

  def Fr: SyntacticMolecule = Fr(1)

  def Ra: SyntacticMolecule = Ra(1)

  def Ac: SyntacticMolecule = Ac(1)

  def Th: SyntacticMolecule = Th(1)

  def Pa: SyntacticMolecule = Pa(1)

  def U: SyntacticMolecule = U(1)

  def Np: SyntacticMolecule = Np(1)

  def Pu: SyntacticMolecule = Pu(1)

  def Am: SyntacticMolecule = Am(1)

  def Cm: SyntacticMolecule = Cm(1)

  def Bk: SyntacticMolecule = Bk(1)

  def Cf: SyntacticMolecule = Cf(1)

  def Es: SyntacticMolecule = Es(1)

  def Fm: SyntacticMolecule = Fm(1)

  def Md: SyntacticMolecule = Md(1)

  def No: SyntacticMolecule = No(1)

  def Lr: SyntacticMolecule = Lr(1)

  def Rf: SyntacticMolecule = Rf(1)

  def Db: SyntacticMolecule = Db(1)

  def Sg: SyntacticMolecule = Sg(1)

  def Bh: SyntacticMolecule = Bh(1)

  def Hs: SyntacticMolecule = Hs(1)

  def Mt: SyntacticMolecule = Mt(1)

  def Ds: SyntacticMolecule = Ds(1)

  def Rg: SyntacticMolecule = Rg(1)

  def Cn: SyntacticMolecule = Cn(1)

  def Nh: SyntacticMolecule = Nh(1)

  def Fl: SyntacticMolecule = Fl(1)

  def Mc: SyntacticMolecule = Mc(1)

  def Lv: SyntacticMolecule = Lv(1)

  def Ts: SyntacticMolecule = Ts(1)

  def Og: SyntacticMolecule = Og(1)
}

