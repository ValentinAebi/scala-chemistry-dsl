package units

trait PhysicalUnit[T <: PhysicalUnit[T]] {
  l =>

  def value: Double

  def newValue(value: Double): T

  val + : T => T = linearBinop(_ + _)
  val - : T => T = linearBinop(_ - _)

  val * : Double => T = scalarBinop(_ * _)
  val / : Double => T = scalarBinop(_ / _)

  def /(r: T): Double = l.value / r.value

  //@formatter:off
  val < : T => Boolean = comparisonOp(_ < _)
  val > : T => Boolean = comparisonOp(_ > _)
  val <= : T => Boolean = comparisonOp(_ <= _)
  val >= : T => Boolean = comparisonOp(_ >= _)
  //@formatter:on

  private def linearBinop(op: (Double, Double) => Double)(that: T): T = newValue(op(this.value, that.value))

  private def scalarBinop(op: (Double, Double) => Double)(d: Double): T = newValue(op(this.value, d))

  private def comparisonOp(op: (Double, Double) => Boolean)(that: T): Boolean = op(this.value, that.value)

  def unary_- : T = newValue(-value)

}

/**
 * Unified atomic mass unit (Dalton)
 */
final case class AtomicMassUnit(value: Double) extends PhysicalUnit[AtomicMassUnit] {
  override def newValue(value: Double): AtomicMassUnit = AtomicMassUnit(value)

  override def toString: String = f"$value%.3f u"
}

final case class Gram(value: Double) extends PhysicalUnit[Gram] {
  override def newValue(value: Double): Gram = Gram(value)

  override def toString: String = f"$value%.3f g"

  def /(amu: AtomicMassUnit): Mol = Mol(this.value / amu.value)
}

final case class Mol(value: Double) extends PhysicalUnit[Mol] {
  override def newValue(value: Double): Mol = Mol(value)

  override def toString: String = f"$value%.3f mol"

  def *(amu: AtomicMassUnit): Gram = Gram(value * amu.value)
}

extension (value: Double) {

  def *[T <: PhysicalUnit[T]](that: T): T = that * value

  def u: AtomicMassUnit = AtomicMassUnit(value)

  def g: Gram = Gram(value)

  def mol: Mol = Mol(value)
}
