package dsl.parsing

import dsl.MoleculeFormatException


class ParsingIterator(str: String) extends Iterator[String] {
  require(str.nonEmpty)

  private var pendingChar = Option(str.head)
  private val strIter = str.tail.iterator

  override def hasNext: Boolean = pendingChar.isDefined

  override def next(): String = {
    var token = advance().toString
    if (token.head == '(') {
      var level = 1
      while (level > 0) {
        if (!strIter.hasNext) {
          throw MoleculeFormatException("missing closing parenthesis")
        }
        val c = advance()
        token += c
        if (c == '(') level += 1
        else if (c == ')') level -= 1
      }
    } else if (token.head == ')') {
      throw MoleculeFormatException("unexpected closing parenthesis")
    } else if (token.head.isLetter) {
      while (pendingChar.exists(_.isLower)) {
        token += advance()
      }
    } else if (token.head.isDigit) {
      while (pendingChar.exists(_.isDigit)) {
        token += advance()
      }
    } else if (token.head != '^' && token.head != '+' && token.head != '-') {
      throw MoleculeFormatException(s"unexpected char in Molecule: ${token.head}")
    }
    token
  }

  private def advance(): Char = {
    val result = pendingChar.getOrElse {
      throw new NoSuchElementException()
    }
    pendingChar = Option.when(strIter.hasNext)(strIter.next())
    result
  }

}
