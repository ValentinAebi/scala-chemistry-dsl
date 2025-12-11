package math

import java.util.{Objects, StringJoiner}
import scala.collection.mutable


final class Matrix(nRowsInit: Int, nColsInit: Int) {
  private val cells = mutable.ListBuffer.fill(nRowsInit)(mutable.ListBuffer.fill(nColsInit)(0))

  def nRows(): Int = cells.length

  def nCols(): Int = cells.head.length

  def sqSize(): Int = Math.min(nRows(), nCols())

  def update(pos: (Int, Int), value: Int): Unit = {
    val (row, col) = pos
    cells(row)(col) = value
  }

  def apply(pos: (Int, Int)): Int = {
    val (row, col) = pos
    cells(row)(col)
  }

  def diagonal: Seq[Int] = (0 until sqSize()).map(idx => this (idx, idx))

  override def toString: String = {
    val strRepr = cells.map(_.map(_.toString))
    val colsWidth = (0 until nCols()).map { c =>
      (0 until nRows()).foldLeft(0) { (acc, r) =>
        Math.max(acc, strRepr(r)(c).length)
      }
    }
    val rowsJoiner = StringJoiner("\n")
    var r = 0
    while (r < nRows()) {
      val colsJoiner = StringJoiner(" ")
      var c = 0
      while (c < nCols()) {
        colsJoiner.add(this (r, c).toString.padLeft(colsWidth(c)))
        c += 1
      }
      rowsJoiner.add(colsJoiner.toString)
      r += 1
    }
    rowsJoiner.toString
  }

  override def equals(thatObj: Any): Boolean = thatObj match {
    case that: Matrix =>
      if (!(this.nRows() == that.nRows() && this.nCols() == that.nCols())) {
        return false
      }
      var r = 0
      while (r < nRows()) {
        var c = 0
        while (c < nCols()) {
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

  override def hashCode(): Int = Objects.hash(nRows(), nCols(), cells)

  def gaussianElimination(): Unit = {
    eliminateLow()
    eliminateUp()
  }

  private def eliminateLow(): Unit = {
    var colIdx = 0
    while (colIdx < sqSize()) {
      val removed = removeZeroRows()
      if (!removed) {
        nullifyColDown(colIdx)
        minimizeRow(colIdx)
        colIdx += 1
      }
    }
  }

  private def eliminateUp(): Unit = {
    var colIdx = 0
    while (colIdx < sqSize()) {
      nullifyColUp(colIdx)
      minimizeRow(colIdx)
      colIdx += 1
    }
  }

  private def removeZeroRows(): Boolean = {
    val preSize = cells.size
    cells.filterInPlace(_.exists(_ != 0))
    cells.size < preSize
  }

  private def nullifyColDown(colIdx: Int): Unit = {
    ensurePivotIsNonZero(colIdx)
    val pivot = this (colIdx, colIdx)
    for (rowIdx <- (colIdx + 1) until nRows()) {
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
    while (colIdx < nCols()) {
      this ((outRowIdx, colIdx)) = coef1 * this (row1Idx, colIdx) + coef2 * this (row2Idx, colIdx)
      colIdx += 1
    }
  }

  private def ensurePivotIsNonZero(pivotIdx: Int): Unit = {
    var otherRowIdx = pivotIdx
    while (otherRowIdx < nRows() && this (otherRowIdx, pivotIdx) == 0) {
      otherRowIdx += 1
    }
    if (otherRowIdx < nRows() && otherRowIdx != pivotIdx) {
      combineRows(pivotIdx, 1, otherRowIdx, 1, pivotIdx)
      minimizeRow(pivotIdx)
    }
  }

  private def minimizeRow(rowIdx: Int): Unit = {
    val line = cells(rowIdx)
    var d = gcd(line.toSeq)
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
        throw IllegalArgumentException("cannot build a matrix from a table with an irregular number of columns")
      }
    }
    val matrix = new Matrix(nRows, nCols)
    for (rIdx <- coefs.indices; cIdx <- coefs.head.indices) {
      matrix((rIdx, cIdx)) = coefs(rIdx)(cIdx)
    }
    matrix
  }

}
