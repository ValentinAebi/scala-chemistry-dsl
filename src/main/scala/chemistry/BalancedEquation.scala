package chemistry

import math.{Matrix, gcd, lcm}

final case class BalancedEquation private(left: List[(Molecule, Int)], right: List[(Molecule, Int)]) extends Equation {

  override def leftMemberMolecules: Set[Molecule] = left.map(_._1).toSet

  override def rightMemberMolecules: Set[Molecule] = right.map(_._1).toSet
  
  def leftCoefOf(molecule: Molecule): Int =
    left.find(_._1 == molecule)
      .map(_._2)
      .getOrElse(0)
      
  def rightCoefOf(molecule: Molecule): Int =
    right.find(_._1 == molecule)
      .map(_._2)
      .getOrElse(0)

  override def toString: String = s"${memberToString(left)} --> ${memberToString(right)}"

  private def memberToString(member: List[(Molecule, Int)]): String =
    member.map {
      case (molec, 1) => molec.toString
      case (molec, coef) => s"$coef $molec"
    }.mkString(" + ")

}

object BalancedEquation {

  def balancedFrom(noCoefEq: NoCoefEquation): Option[BalancedEquation] = {
    val matrix = generateMatrix(noCoefEq)
    try {
      matrix.gaussianElimination()
    } catch {
      case arithmeticException: ArithmeticException =>
        return None
    }
    val diagLcm = lcm(matrix.diagonal)
    val coefs = (0 until matrix.nCols()).map { colIdx =>
      if colIdx < matrix.nRows() then -diagLcm * matrix(colIdx, matrix.nCols() - 1) / matrix(colIdx, colIdx)
      else if colIdx == matrix.nCols() - 1 then diagLcm
      else 0
    }
    if (coefs.exists(_ < 0)) {
      return None
    }
    val coefGcd = gcd(coefs)
    val minimizedCoefsIter = coefs.iterator.map(_ / coefGcd)
    val leftMemberB = List.newBuilder[(Molecule, Int)]
    for (molec <- noCoefEq.leftMember) {
      leftMemberB.addOne(molec, minimizedCoefsIter.next())
    }
    val rightMemberB = List.newBuilder[(Molecule, Int)]
    for (molec <- noCoefEq.rightMember) {
      rightMemberB.addOne(molec, minimizedCoefsIter.next())
    }
    Some(BalancedEquation(leftMemberB.result(), rightMemberB.result()))
  }

  private def generateMatrix(noCoefEq: NoCoefEquation): Matrix = {
    val coefRows = noCoefEq.allAtoms.toSeq.sortBy(_.ordinal).map(matrixRow(noCoefEq, _))
    Matrix(coefRows ++ chargesRow(noCoefEq))
  }

  private def matrixRow(noCoefEq: NoCoefEquation, atom: Atom): Seq[Int] = {
    val seqB = Seq.newBuilder[Int]
    for (molec <- noCoefEq.leftMember) {
      seqB.addOne(molec.atomCnt(atom))
    }
    for (molec <- noCoefEq.rightMember) {
      seqB.addOne(-molec.atomCnt(atom))
    }
    seqB.result()
  }

  private def chargesRow(noCoefEq: NoCoefEquation): Option[Seq[Int]] = {
    var allZeros = true
    val seqB = Seq.newBuilder[Int]
    for (molec <- noCoefEq.leftMember) {
      seqB.addOne(molec.charge)
      allZeros &= molec.charge == 0
    }
    for (molec <- noCoefEq.rightMember) {
      seqB.addOne(-molec.charge)
      allZeros &= molec.charge == 0
    }
    Option.when(!allZeros)(seqB.result())
  }


}
