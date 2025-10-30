package units

trait PhysicalUnit[T <: PhysicalUnit[T]] {

  def value: Double
  def newValue(value: Double): T

  val + : T => T = linearBinop(_ + _)
  val - : T => T = linearBinop(_ - _)

  val * : Double => T = scalarBinop(_ * _)
  val / : Double => T = scalarBinop(_ / _)
  
  val < : T => Boolean = comparisonOp(_ < _)
  val > : T => Boolean = comparisonOp(_ > _)

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

  override def toString: String = s"$value u"
}

extension (value: Double) {
  
  def *[T <: PhysicalUnit[T]](that: T): T = that * value
  
  def u: AtomicMassUnit = AtomicMassUnit(value)
}
