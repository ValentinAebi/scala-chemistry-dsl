package dsl

import chemistry.{Atom, Molecule}

import scala.collection.mutable
import scala.quoted.{Expr, Quotes}

sealed trait SyntacticMolecule {
  def components: List[(Atom | NoChargeMolecule, Int)]
  def charge: Int

  def toMolecule: Molecule = {
    val atomsB = mutable.Map.empty[Atom, Int]

    def add(atom: Atom, coef: Int): Unit = {
      atomsB.updateWith(atom) { prevCoefOpt =>
        Some(prevCoefOpt.getOrElse(0) + coef)
      }
    }

    components.foreach {
      case (atom: Atom, coef) => add(atom, coef)
      case (molecule: NoChargeMolecule, superCoef) =>
        for ((atom, coef) <- molecule.toMolecule.atoms) {
          add(atom, coef * superCoef)
        }
    }
    Molecule(atomsB.toMap, charge)
  }
}

final case class NoChargeMolecule(components: List[(Atom | NoChargeMolecule, Int)]) extends SyntacticMolecule {

  override def charge: Int = 0

  def +(that: NoChargeMolecule) = NoCoefEquation.Member(List(this, that))

  def +(eqMember: NoCoefEquation.Member) = NoCoefEquation.Member(this +: eqMember.molecules)

  def !(coef: Int) = NoChargeMolecule(List(this -> coef))

  inline def ^(inline chargeFunc: Int => Int): ChargedMolecule = {
    val charge = chargeMacro(chargeFunc)
    ChargedMolecule(components, charge)
  }

  def apply(that: NoChargeMolecule) = NoChargeMolecule(this.components ++ that.components)

  def H(coef: Int): NoChargeMolecule = NoChargeMolecule(components :+ (Atom.Hydrogen, coef))

  def He(coef: Int): NoChargeMolecule = NoChargeMolecule(components :+ (Atom.Helium, coef))

  def Li(coef: Int): NoChargeMolecule = NoChargeMolecule(components :+ (Atom.Lithium, coef))

  def Be(coef: Int): NoChargeMolecule = NoChargeMolecule(components :+ (Atom.Beryllium, coef))

  def B(coef: Int): NoChargeMolecule = NoChargeMolecule(components :+ (Atom.Boron, coef))

  def C(coef: Int): NoChargeMolecule = NoChargeMolecule(components :+ (Atom.Carbon, coef))

  def N(coef: Int): NoChargeMolecule = NoChargeMolecule(components :+ (Atom.Nitrogen, coef))

  def O(coef: Int): NoChargeMolecule = NoChargeMolecule(components :+ (Atom.Oxygen, coef))

  def F(coef: Int): NoChargeMolecule = NoChargeMolecule(components :+ (Atom.Fluorine, coef))

  def Ne(coef: Int): NoChargeMolecule = NoChargeMolecule(components :+ (Atom.Neon, coef))

  def Na(coef: Int): NoChargeMolecule = NoChargeMolecule(components :+ (Atom.Sodium, coef))

  def Mg(coef: Int): NoChargeMolecule = NoChargeMolecule(components :+ (Atom.Magnesium, coef))

  def Al(coef: Int): NoChargeMolecule = NoChargeMolecule(components :+ (Atom.Aluminum, coef))

  def Si(coef: Int): NoChargeMolecule = NoChargeMolecule(components :+ (Atom.Silicon, coef))

  def P(coef: Int): NoChargeMolecule = NoChargeMolecule(components :+ (Atom.Phosphorus, coef))

  def S(coef: Int): NoChargeMolecule = NoChargeMolecule(components :+ (Atom.Sulfur, coef))

  def Cl(coef: Int): NoChargeMolecule = NoChargeMolecule(components :+ (Atom.Chlorine, coef))

  def Ar(coef: Int): NoChargeMolecule = NoChargeMolecule(components :+ (Atom.Argon, coef))

  def K(coef: Int): NoChargeMolecule = NoChargeMolecule(components :+ (Atom.Potassium, coef))

  def Ca(coef: Int): NoChargeMolecule = NoChargeMolecule(components :+ (Atom.Calcium, coef))

  def Sc(coef: Int): NoChargeMolecule = NoChargeMolecule(components :+ (Atom.Scandium, coef))

  def Ti(coef: Int): NoChargeMolecule = NoChargeMolecule(components :+ (Atom.Titanium, coef))

  def V(coef: Int): NoChargeMolecule = NoChargeMolecule(components :+ (Atom.Vanadium, coef))

  def Cr(coef: Int): NoChargeMolecule = NoChargeMolecule(components :+ (Atom.Chromium, coef))

  def Mn(coef: Int): NoChargeMolecule = NoChargeMolecule(components :+ (Atom.Manganese, coef))

  def Fe(coef: Int): NoChargeMolecule = NoChargeMolecule(components :+ (Atom.Iron, coef))

  def Co(coef: Int): NoChargeMolecule = NoChargeMolecule(components :+ (Atom.Cobalt, coef))

  def Ni(coef: Int): NoChargeMolecule = NoChargeMolecule(components :+ (Atom.Nickel, coef))

  def Cu(coef: Int): NoChargeMolecule = NoChargeMolecule(components :+ (Atom.Copper, coef))

  def Zn(coef: Int): NoChargeMolecule = NoChargeMolecule(components :+ (Atom.Zinc, coef))

  def Ga(coef: Int): NoChargeMolecule = NoChargeMolecule(components :+ (Atom.Gallium, coef))

  def Ge(coef: Int): NoChargeMolecule = NoChargeMolecule(components :+ (Atom.Germanium, coef))

  def As(coef: Int): NoChargeMolecule = NoChargeMolecule(components :+ (Atom.Arsenic, coef))

  def Se(coef: Int): NoChargeMolecule = NoChargeMolecule(components :+ (Atom.Selenium, coef))

  def Br(coef: Int): NoChargeMolecule = NoChargeMolecule(components :+ (Atom.Bromine, coef))

  def Kr(coef: Int): NoChargeMolecule = NoChargeMolecule(components :+ (Atom.Krypton, coef))

  def Rb(coef: Int): NoChargeMolecule = NoChargeMolecule(components :+ (Atom.Rubidium, coef))

  def Sr(coef: Int): NoChargeMolecule = NoChargeMolecule(components :+ (Atom.Strontium, coef))

  def Y(coef: Int): NoChargeMolecule = NoChargeMolecule(components :+ (Atom.Yttrium, coef))

  def Zr(coef: Int): NoChargeMolecule = NoChargeMolecule(components :+ (Atom.Zirconium, coef))

  def Nb(coef: Int): NoChargeMolecule = NoChargeMolecule(components :+ (Atom.Niobium, coef))

  def Mo(coef: Int): NoChargeMolecule = NoChargeMolecule(components :+ (Atom.Molybdenum, coef))

  def Tc(coef: Int): NoChargeMolecule = NoChargeMolecule(components :+ (Atom.Technetium, coef))

  def Ru(coef: Int): NoChargeMolecule = NoChargeMolecule(components :+ (Atom.Ruthenium, coef))

  def Rh(coef: Int): NoChargeMolecule = NoChargeMolecule(components :+ (Atom.Rhodium, coef))

  def Pd(coef: Int): NoChargeMolecule = NoChargeMolecule(components :+ (Atom.Palladium, coef))

  def Ag(coef: Int): NoChargeMolecule = NoChargeMolecule(components :+ (Atom.Silver, coef))

  def Cd(coef: Int): NoChargeMolecule = NoChargeMolecule(components :+ (Atom.Cadmium, coef))

  def In(coef: Int): NoChargeMolecule = NoChargeMolecule(components :+ (Atom.Indium, coef))

  def Sn(coef: Int): NoChargeMolecule = NoChargeMolecule(components :+ (Atom.Tin, coef))

  def Sb(coef: Int): NoChargeMolecule = NoChargeMolecule(components :+ (Atom.Antimony, coef))

  def Te(coef: Int): NoChargeMolecule = NoChargeMolecule(components :+ (Atom.Tellurium, coef))

  def I(coef: Int): NoChargeMolecule = NoChargeMolecule(components :+ (Atom.Iodine, coef))

  def Xe(coef: Int): NoChargeMolecule = NoChargeMolecule(components :+ (Atom.Xenon, coef))

  def Cs(coef: Int): NoChargeMolecule = NoChargeMolecule(components :+ (Atom.Cesium, coef))

  def Ba(coef: Int): NoChargeMolecule = NoChargeMolecule(components :+ (Atom.Barium, coef))

  def La(coef: Int): NoChargeMolecule = NoChargeMolecule(components :+ (Atom.Lanthanum, coef))

  def Ce(coef: Int): NoChargeMolecule = NoChargeMolecule(components :+ (Atom.Cerium, coef))

  def Pr(coef: Int): NoChargeMolecule = NoChargeMolecule(components :+ (Atom.Praseodymium, coef))

  def Nd(coef: Int): NoChargeMolecule = NoChargeMolecule(components :+ (Atom.Neodymium, coef))

  def Pm(coef: Int): NoChargeMolecule = NoChargeMolecule(components :+ (Atom.Promethium, coef))

  def Sm(coef: Int): NoChargeMolecule = NoChargeMolecule(components :+ (Atom.Samarium, coef))

  def Eu(coef: Int): NoChargeMolecule = NoChargeMolecule(components :+ (Atom.Europium, coef))

  def Gd(coef: Int): NoChargeMolecule = NoChargeMolecule(components :+ (Atom.Gadolinium, coef))

  def Tb(coef: Int): NoChargeMolecule = NoChargeMolecule(components :+ (Atom.Terbium, coef))

  def Dy(coef: Int): NoChargeMolecule = NoChargeMolecule(components :+ (Atom.Dysprosium, coef))

  def Ho(coef: Int): NoChargeMolecule = NoChargeMolecule(components :+ (Atom.Holmium, coef))

  def Er(coef: Int): NoChargeMolecule = NoChargeMolecule(components :+ (Atom.Erbium, coef))

  def Tm(coef: Int): NoChargeMolecule = NoChargeMolecule(components :+ (Atom.Thulium, coef))

  def Yb(coef: Int): NoChargeMolecule = NoChargeMolecule(components :+ (Atom.Ytterbium, coef))

  def Lu(coef: Int): NoChargeMolecule = NoChargeMolecule(components :+ (Atom.Lutetium, coef))

  def Hf(coef: Int): NoChargeMolecule = NoChargeMolecule(components :+ (Atom.Hafnium, coef))

  def Ta(coef: Int): NoChargeMolecule = NoChargeMolecule(components :+ (Atom.Tantalum, coef))

  def W(coef: Int): NoChargeMolecule = NoChargeMolecule(components :+ (Atom.Wolfram, coef))

  def Re(coef: Int): NoChargeMolecule = NoChargeMolecule(components :+ (Atom.Rhenium, coef))

  def Os(coef: Int): NoChargeMolecule = NoChargeMolecule(components :+ (Atom.Osmium, coef))

  def Ir(coef: Int): NoChargeMolecule = NoChargeMolecule(components :+ (Atom.Iridium, coef))

  def Pt(coef: Int): NoChargeMolecule = NoChargeMolecule(components :+ (Atom.Platinum, coef))

  def Au(coef: Int): NoChargeMolecule = NoChargeMolecule(components :+ (Atom.Gold, coef))

  def Hg(coef: Int): NoChargeMolecule = NoChargeMolecule(components :+ (Atom.Mercury, coef))

  def Tl(coef: Int): NoChargeMolecule = NoChargeMolecule(components :+ (Atom.Thallium, coef))

  def Pb(coef: Int): NoChargeMolecule = NoChargeMolecule(components :+ (Atom.Lead, coef))

  def Bi(coef: Int): NoChargeMolecule = NoChargeMolecule(components :+ (Atom.Bismuth, coef))

  def Po(coef: Int): NoChargeMolecule = NoChargeMolecule(components :+ (Atom.Polonium, coef))

  def At(coef: Int): NoChargeMolecule = NoChargeMolecule(components :+ (Atom.Astatine, coef))

  def Rn(coef: Int): NoChargeMolecule = NoChargeMolecule(components :+ (Atom.Radon, coef))

  def Fr(coef: Int): NoChargeMolecule = NoChargeMolecule(components :+ (Atom.Francium, coef))

  def Ra(coef: Int): NoChargeMolecule = NoChargeMolecule(components :+ (Atom.Radium, coef))

  def Ac(coef: Int): NoChargeMolecule = NoChargeMolecule(components :+ (Atom.Actinium, coef))

  def Th(coef: Int): NoChargeMolecule = NoChargeMolecule(components :+ (Atom.Thorium, coef))

  def Pa(coef: Int): NoChargeMolecule = NoChargeMolecule(components :+ (Atom.Protactinium, coef))

  def U(coef: Int): NoChargeMolecule = NoChargeMolecule(components :+ (Atom.Uranium, coef))

  def Np(coef: Int): NoChargeMolecule = NoChargeMolecule(components :+ (Atom.Neptunium, coef))

  def Pu(coef: Int): NoChargeMolecule = NoChargeMolecule(components :+ (Atom.Plutonium, coef))

  def Am(coef: Int): NoChargeMolecule = NoChargeMolecule(components :+ (Atom.Americium, coef))

  def Cm(coef: Int): NoChargeMolecule = NoChargeMolecule(components :+ (Atom.Curium, coef))

  def Bk(coef: Int): NoChargeMolecule = NoChargeMolecule(components :+ (Atom.Berkelium, coef))

  def Cf(coef: Int): NoChargeMolecule = NoChargeMolecule(components :+ (Atom.Californium, coef))

  def Es(coef: Int): NoChargeMolecule = NoChargeMolecule(components :+ (Atom.Einsteinium, coef))

  def Fm(coef: Int): NoChargeMolecule = NoChargeMolecule(components :+ (Atom.Fermium, coef))

  def Md(coef: Int): NoChargeMolecule = NoChargeMolecule(components :+ (Atom.Mendelevium, coef))

  def No(coef: Int): NoChargeMolecule = NoChargeMolecule(components :+ (Atom.Nobelium, coef))

  def Lr(coef: Int): NoChargeMolecule = NoChargeMolecule(components :+ (Atom.Lawrencium, coef))

  def Rf(coef: Int): NoChargeMolecule = NoChargeMolecule(components :+ (Atom.Rutherfordium, coef))

  def Db(coef: Int): NoChargeMolecule = NoChargeMolecule(components :+ (Atom.Dubnium, coef))

  def Sg(coef: Int): NoChargeMolecule = NoChargeMolecule(components :+ (Atom.Seaborgium, coef))

  def Bh(coef: Int): NoChargeMolecule = NoChargeMolecule(components :+ (Atom.Bohrium, coef))

  def Hs(coef: Int): NoChargeMolecule = NoChargeMolecule(components :+ (Atom.Hassium, coef))

  def Mt(coef: Int): NoChargeMolecule = NoChargeMolecule(components :+ (Atom.Meitnerium, coef))

  def Ds(coef: Int): NoChargeMolecule = NoChargeMolecule(components :+ (Atom.Darmstadtium, coef))

  def Rg(coef: Int): NoChargeMolecule = NoChargeMolecule(components :+ (Atom.Roentgenium, coef))

  def Cn(coef: Int): NoChargeMolecule = NoChargeMolecule(components :+ (Atom.Copernicium, coef))

  def Nh(coef: Int): NoChargeMolecule = NoChargeMolecule(components :+ (Atom.Nihonium, coef))

  def Fl(coef: Int): NoChargeMolecule = NoChargeMolecule(components :+ (Atom.Flerovium, coef))

  def Mc(coef: Int): NoChargeMolecule = NoChargeMolecule(components :+ (Atom.Moscovium, coef))

  def Lv(coef: Int): NoChargeMolecule = NoChargeMolecule(components :+ (Atom.Livermorium, coef))

  def Ts(coef: Int): NoChargeMolecule = NoChargeMolecule(components :+ (Atom.Tennessine, coef))

  def Og(coef: Int): NoChargeMolecule = NoChargeMolecule(components :+ (Atom.Oganesson, coef))

  def H: NoChargeMolecule = H(1)

  def He: NoChargeMolecule = He(1)

  def Li: NoChargeMolecule = Li(1)

  def Be: NoChargeMolecule = Be(1)

  def B: NoChargeMolecule = B(1)

  def C: NoChargeMolecule = C(1)

  def N: NoChargeMolecule = N(1)

  def O: NoChargeMolecule = O(1)

  def F: NoChargeMolecule = F(1)

  def Ne: NoChargeMolecule = Ne(1)

  def Na: NoChargeMolecule = Na(1)

  def Mg: NoChargeMolecule = Mg(1)

  def Al: NoChargeMolecule = Al(1)

  def Si: NoChargeMolecule = Si(1)

  def P: NoChargeMolecule = P(1)

  def S: NoChargeMolecule = S(1)

  def Cl: NoChargeMolecule = Cl(1)

  def Ar: NoChargeMolecule = Ar(1)

  def K: NoChargeMolecule = K(1)

  def Ca: NoChargeMolecule = Ca(1)

  def Sc: NoChargeMolecule = Sc(1)

  def Ti: NoChargeMolecule = Ti(1)

  def V: NoChargeMolecule = V(1)

  def Cr: NoChargeMolecule = Cr(1)

  def Mn: NoChargeMolecule = Mn(1)

  def Fe: NoChargeMolecule = Fe(1)

  def Co: NoChargeMolecule = Co(1)

  def Ni: NoChargeMolecule = Ni(1)

  def Cu: NoChargeMolecule = Cu(1)

  def Zn: NoChargeMolecule = Zn(1)

  def Ga: NoChargeMolecule = Ga(1)

  def Ge: NoChargeMolecule = Ge(1)

  def As: NoChargeMolecule = As(1)

  def Se: NoChargeMolecule = Se(1)

  def Br: NoChargeMolecule = Br(1)

  def Kr: NoChargeMolecule = Kr(1)

  def Rb: NoChargeMolecule = Rb(1)

  def Sr: NoChargeMolecule = Sr(1)

  def Y: NoChargeMolecule = Y(1)

  def Zr: NoChargeMolecule = Zr(1)

  def Nb: NoChargeMolecule = Nb(1)

  def Mo: NoChargeMolecule = Mo(1)

  def Tc: NoChargeMolecule = Tc(1)

  def Ru: NoChargeMolecule = Ru(1)

  def Rh: NoChargeMolecule = Rh(1)

  def Pd: NoChargeMolecule = Pd(1)

  def Ag: NoChargeMolecule = Ag(1)

  def Cd: NoChargeMolecule = Cd(1)

  def In: NoChargeMolecule = In(1)

  def Sn: NoChargeMolecule = Sn(1)

  def Sb: NoChargeMolecule = Sb(1)

  def Te: NoChargeMolecule = Te(1)

  def I: NoChargeMolecule = I(1)

  def Xe: NoChargeMolecule = Xe(1)

  def Cs: NoChargeMolecule = Cs(1)

  def Ba: NoChargeMolecule = Ba(1)

  def La: NoChargeMolecule = La(1)

  def Ce: NoChargeMolecule = Ce(1)

  def Pr: NoChargeMolecule = Pr(1)

  def Nd: NoChargeMolecule = Nd(1)

  def Pm: NoChargeMolecule = Pm(1)

  def Sm: NoChargeMolecule = Sm(1)

  def Eu: NoChargeMolecule = Eu(1)

  def Gd: NoChargeMolecule = Gd(1)

  def Tb: NoChargeMolecule = Tb(1)

  def Dy: NoChargeMolecule = Dy(1)

  def Ho: NoChargeMolecule = Ho(1)

  def Er: NoChargeMolecule = Er(1)

  def Tm: NoChargeMolecule = Tm(1)

  def Yb: NoChargeMolecule = Yb(1)

  def Lu: NoChargeMolecule = Lu(1)

  def Hf: NoChargeMolecule = Hf(1)

  def Ta: NoChargeMolecule = Ta(1)

  def W: NoChargeMolecule = W(1)

  def Re: NoChargeMolecule = Re(1)

  def Os: NoChargeMolecule = Os(1)

  def Ir: NoChargeMolecule = Ir(1)

  def Pt: NoChargeMolecule = Pt(1)

  def Au: NoChargeMolecule = Au(1)

  def Hg: NoChargeMolecule = Hg(1)

  def Tl: NoChargeMolecule = Tl(1)

  def Pb: NoChargeMolecule = Pb(1)

  def Bi: NoChargeMolecule = Bi(1)

  def Po: NoChargeMolecule = Po(1)

  def At: NoChargeMolecule = At(1)

  def Rn: NoChargeMolecule = Rn(1)

  def Fr: NoChargeMolecule = Fr(1)

  def Ra: NoChargeMolecule = Ra(1)

  def Ac: NoChargeMolecule = Ac(1)

  def Th: NoChargeMolecule = Th(1)

  def Pa: NoChargeMolecule = Pa(1)

  def U: NoChargeMolecule = U(1)

  def Np: NoChargeMolecule = Np(1)

  def Pu: NoChargeMolecule = Pu(1)

  def Am: NoChargeMolecule = Am(1)

  def Cm: NoChargeMolecule = Cm(1)

  def Bk: NoChargeMolecule = Bk(1)

  def Cf: NoChargeMolecule = Cf(1)

  def Es: NoChargeMolecule = Es(1)

  def Fm: NoChargeMolecule = Fm(1)

  def Md: NoChargeMolecule = Md(1)

  def No: NoChargeMolecule = No(1)

  def Lr: NoChargeMolecule = Lr(1)

  def Rf: NoChargeMolecule = Rf(1)

  def Db: NoChargeMolecule = Db(1)

  def Sg: NoChargeMolecule = Sg(1)

  def Bh: NoChargeMolecule = Bh(1)

  def Hs: NoChargeMolecule = Hs(1)

  def Mt: NoChargeMolecule = Mt(1)

  def Ds: NoChargeMolecule = Ds(1)

  def Rg: NoChargeMolecule = Rg(1)

  def Cn: NoChargeMolecule = Cn(1)

  def Nh: NoChargeMolecule = Nh(1)

  def Fl: NoChargeMolecule = Fl(1)

  def Mc: NoChargeMolecule = Mc(1)

  def Lv: NoChargeMolecule = Lv(1)

  def Ts: NoChargeMolecule = Ts(1)

  def Og: NoChargeMolecule = Og(1)
}

case object ChargeExpansionError extends Error {
  override def getMessage: String = "charge should have the following form: ^2.+, ^3.-, etc."
}

final case class ChargedMolecule(components: List[(Atom | NoChargeMolecule, Int)], charge: Int) extends SyntacticMolecule

inline def chargeMacro(inline chargeFunc: Int => Int): Int = ${ chargeMacroImpl('chargeFunc) }

def chargeMacroImpl(expr: Expr[Int => Int])(using Quotes): Expr[Int] = {
  expr match {
    case '{ (j: Int) => ($i: Int).+(j) } => i
    case '{ (j: Int) => ($i: Int).-(j) } => '{ -$i }
    case _ => throw ChargeExpansionError
  }
}
