package math

import java.util.{Objects, StringJoiner}


final class Matrix(val nRows: Int, val nCols: Int) {
  private val cells = Array.fill(nRows)(new Array[Int](nCols))

  private val rowsRange = 0 until nRows
  private val colsRange = 0 until nCols
  private val squareRange = 0 until Math.min(nRows, nCols)

  def update(pos: (Int, Int), value: Int): Unit = {
    val (row, col) = pos
    cells(row)(col) = value
  }

  def apply(pos: (Int, Int)): Int = {
    val (row, col) = pos
    cells(row)(col)
  }

  def diagonal: Seq[Int] = squareRange.map(idx => this (idx, idx))

  override def toString: String = {
    val strRepr = cells.map(_.map(_.toString))
    val colsWidth = (0 until nCols).map { c =>
      (0 until nRows).foldLeft(0) { (acc, r) =>
        Math.max(acc, strRepr(r)(c).length)
      }
    }
    val rowsJoiner = StringJoiner("\n")
    for (r <- rowsRange) {
      val colsJoiner = StringJoiner(" ")
      for (c <- colsRange) {
        colsJoiner.add(this (r, c).toString.padLeft(colsWidth(c)))
      }
      rowsJoiner.add(colsJoiner.toString)
    }
    rowsJoiner.toString
  }

  override def equals(thatObj: Any): Boolean = thatObj match {
    case that: Matrix =>
      if (!(this.nRows == that.nRows && this.nCols == that.nCols)) {
        return false
      }
      var r = 0
      while (r < nRows) {
        var c = 0
        while (c < nCols) {
          if (this (r, c) != that(r, c)) {
            return false
          }
          c += 1
        }
        r += 1
      }
      true
    case _ => false
  }

  override def hashCode(): Int = Objects.hash(nRows, nCols, cells)

  def diagonalize(): Unit = {
    eliminateLow()
    eliminateUp()
  }

  private def eliminateLow(): Unit = {
    for (colIdx <- squareRange) {
      nullifyColDown(colIdx)
      minimizeRow(colIdx)
    }
  }

  private def eliminateUp(): Unit = {
    for (colIdx <- squareRange.reverse) {
      nullifyColUp(colIdx)
      minimizeRow(colIdx)
    }
  }

  private def nullifyColDown(colIdx: Int): Unit = {
    ensurePivotIsNonZero(colIdx)
    val pivot = this (colIdx, colIdx)
    for (rowIdx <- (colIdx + 1) until nRows) {
      val headCoef = this (rowIdx, colIdx)
      val d = gcd(pivot, headCoef)
      combineRows(rowIdx, pivot / d, colIdx, -headCoef / d, rowIdx)
    }
  }

  private def nullifyColUp(colIdx: Int): Unit = {
    val pivot = this (colIdx, colIdx)
    for (rowIdx <- 0 until colIdx) {
      val tailCoef = this (rowIdx, colIdx)
      val d = gcd(pivot, tailCoef)
      combineRows(rowIdx, pivot / d, colIdx, -tailCoef / d, rowIdx)
    }
  }

  private def combineRows(row1Idx: Int, coef1: Int, row2Idx: Int, coef2: Int, outRowIdx: Int): Unit = {
    var colIdx = 0
    while (colIdx < nCols) {
      this ((outRowIdx, colIdx)) = coef1 * this (row1Idx, colIdx) + coef2 * this (row2Idx, colIdx)
      colIdx += 1
    }
  }

  private def ensurePivotIsNonZero(pivotIdx: Int): Unit = {
    var otherRowIdx = pivotIdx
    while (this (otherRowIdx, pivotIdx) == 0) {
      otherRowIdx += 1
      if (otherRowIdx >= nRows) {
        throw ArithmeticException()
      }
    }
    if (otherRowIdx != pivotIdx) {
      combineRows(pivotIdx, 1, otherRowIdx, 1, pivotIdx)
      minimizeRow(pivotIdx)
    }
  }

  private def minimizeRow(rowIdx: Int): Unit = {
    val line = cells(rowIdx)
    var d = gcd(line)
    if (line.find(_ != 0).exists(_ < 0)) {
      d = -d
    }
    line.mapInPlace(_ / d)
  }

  extension (str: String) private def padLeft(len: Int): String = {
    if (str.length >= len) {
      str
    } else {
      val padding = " " * (len - str.length)
      padding ++ str
    }
  }

}

object Matrix {

  def apply(coefs: Seq[Seq[Int]]): Matrix = {
    val nRows = coefs.size
    val nCols = coefs.head.size
    for (line <- coefs) {
      if (line.size != nCols) {
        throw IllegalArgumentException("cannot build a matric from a table with an irregular number of columns")
      }
    }
    val matrix = new Matrix(nRows, nCols)
    for (rIdx <- coefs.indices; cIdx <- coefs.head.indices) {
      matrix((rIdx, cIdx)) = coefs(rIdx)(cIdx)
    }
    matrix
  }

}
