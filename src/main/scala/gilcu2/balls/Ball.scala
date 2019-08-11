package gilcu2.balls

object Ball {
  def apply[T](center: T, radio: Double): Ball[T] = new Ball(center, radio)
}

class Ball[T](val center: T, val radio: Double) {
  override def toString: String = s"Ball($center,$radio)"
}