package units

/**
 * Unified atomic mass unit (Dalton)
 */
final case class U(value: Double) extends Arithmetic[U] {
  override def newValue(value: Double): U = U(value)

  override def toString: String = s"$value u"
}

extension (d: Double) {
  
  def *[T <: Arithmetic[T]](that: T): T = that * d
  
  /**
   * Unified atomic mass unit (Dalton)
   */
  def u = U(d)
}

trait Arithmetic[T <: Arithmetic[T]] {

  def value: Double
  def newValue(value: Double): T
  
  val + = linearBinop(_ + _)
  val - = linearBinop(_ - _)
  
  val * = scalarBinop(_ * _)
  val / = scalarBinop(_ / _)
  
  private def linearBinop(op: (Double, Double) => Double)(that: T): T = newValue(op(this.value, that.value))
  
  private def scalarBinop(op: (Double, Double) => Double)(d: Double): T = newValue(op(this.value, d))
  
}
