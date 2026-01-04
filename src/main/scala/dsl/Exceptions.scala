package dsl

case class MoleculeFormatException(msg: String) extends Exception(msg)

case class IllegalParameterException(msg: String) extends Exception(msg)
