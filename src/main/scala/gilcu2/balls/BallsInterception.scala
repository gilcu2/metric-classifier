package gilcu2.balls

import gilcu2.spaces.Metric

sealed trait BallsInterception[+T] {
  override def toString: String = s"{${BallsInterception.toStringAux(this)}}"
}

case object BallsInterceptionEmpty extends BallsInterception[Nothing]

case class BallsInterceptionNonEmpty[T](balls: Set[Ball[T]])
  extends BallsInterception[T]

object BallsInterception {

  def apply[T](ball: Ball[T]): BallsInterception[T] = BallsInterceptionNonEmpty(Set(ball))

  def build_without_check_interceptions[T](balls: Ball[T]*): BallsInterception[T] =
    if (balls.isEmpty) BallsInterceptionEmpty
    else BallsInterceptionNonEmpty(balls.toSet)

  def toStringAux[T](input: BallsInterception[T]): String =
    input match {
      case BallsInterceptionEmpty => "{}"
      case BallsInterceptionNonEmpty(balls) => s"{${balls.mkString(",")}}"
    }

  def intercept[T](inter1: BallsInterception[T], inter2: BallsInterception[T], metric: Metric[T])
  : BallsInterception[T] = (inter1, inter2) match {
    case (BallsInterceptionEmpty, _) => BallsInterceptionEmpty
    case (_, BallsInterceptionEmpty) => BallsInterceptionEmpty
    case (i1: BallsInterceptionNonEmpty[T], i2: BallsInterceptionNonEmpty[T]) =>
      val b1 = i1.balls.head
      val b2 = i2.balls.head
      val d = metric.distance(b1.center, b2.center)
      if (d > b1.radio + b2.radio)
        BallsInterceptionEmpty
      else if (d + b1.radio <= b2.radio)
        inter1
      else if (d + b2.radio <= b1.radio)
        inter2
      else
        build_without_check_interceptions(b1, b2)

  }

}


