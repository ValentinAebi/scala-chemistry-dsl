package chemistry

import dsl.MoleculeFormatException

import scala.collection.mutable
import scala.compiletime.uninitialized


def parseMolecule(str: String): Molecule = {
  val parsingIter = ParsingIterator(str)
  val atomsMap = mutable.Map.empty[Atom, Int]
  val pending = mutable.Map.empty[Atom, Int]

  def consumePending(coef: Int): Unit = {
    for ((at, additionalCoef) <- pending) {
      atomsMap.updateWith(at) {
        case None => Some(additionalCoef * coef)
        case Some(oldCoef) => Some(oldCoef + additionalCoef * coef)
      }
    }
    pending.clear()
  }

  while (parsingIter.hasNext) {
    val token = parsingIter.next()
    if (token.head.isLetter) {
      consumePending(1)
      pending.put(Atom.parse(token).getOrElse {
        throw MoleculeFormatException(s"unrecognized atom: $token")
      }, 1)
    } else if (token.head.isDigit) {
      if (pending.isEmpty) {
        throw MoleculeFormatException("unexpected coefficient not preceded by an atom")
      }
      consumePending(token.toInt)
    } else if (token.head == '(') {
      val subMolecule = parseMolecule(token.substring(1, token.length - 1))
      if (subMolecule.charge != 0) {
        throw MoleculeFormatException("nested molecule cannot have a charge")
      }
      consumePending(1)
      pending.addAll(subMolecule.atoms)
    } else if (token.head == '^') {
      consumePending(1)
      if (!parsingIter.hasNext) {
        throw MoleculeFormatException("illegal notation of charge")
      }
      val nextChar = parsingIter.next()
      var chargeAbsValToken: String = null
      var plusOrMinusToken: String = null
      if (nextChar == "+" || nextChar == "-") {
        plusOrMinusToken = nextChar
        chargeAbsValToken = "1"
      } else {
        if (!parsingIter.hasNext || !nextChar.forall(_.isDigit)){
          throw MoleculeFormatException("illegal value of charge")
        }
        plusOrMinusToken = parsingIter.next()
        chargeAbsValToken = nextChar
      }
      if (plusOrMinusToken == null) {
        plusOrMinusToken = parsingIter.next()
      }
      if (plusOrMinusToken != "+" && plusOrMinusToken != "-") {
        throw MoleculeFormatException(s"expected '+' or '-', found $plusOrMinusToken")
      }
      if (parsingIter.hasNext) {
        throw MoleculeFormatException("molecule must end after charge")
      }
      val charge = if plusOrMinusToken == "+" then chargeAbsValToken.toInt else -chargeAbsValToken.toInt
      val molecule = Molecule(atomsMap.toMap, charge)
      molecule.originalStr = str
      return molecule
    } else {
      throw MoleculeFormatException(s"token unexpected at its position: $token")
    }
  }
  consumePending(1)
  val molecule = Molecule(atomsMap.toMap, 0)
  molecule.originalStr = str
  molecule
}
