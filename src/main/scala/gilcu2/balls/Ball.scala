package gilcu2.balls

object Ball {
  def apply[T](center: T, radio: Double): Ball[T] = new Ball(center, radio)

  def isBallInsideBall[T](b1: Ball[T], b2: Ball[T], distance: Double): Boolean = distance + b1.radio <= b2.radio

  def isBallInterceptingBall[T](b1: Ball[T], b2: Ball[T], distance: Double): Boolean = distance <= b1.radio + b2.radio

  def areBallsSeparated[T](b1: Ball[T], b2: Ball[T], distance: Double): Boolean = distance > b1.radio + b2.radio
}

class Ball[+T](val center: T, val radio: Double) {
  override def toString: String = s"Ball($center,$radio)"
}