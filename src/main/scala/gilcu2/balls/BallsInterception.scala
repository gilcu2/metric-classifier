package gilcu2.balls

import gilcu2.spaces.Metric
import Ball._

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

  def build_without_check_interceptions[T](inter1: BallsInterception[T], inter2: BallsInterception[T])
  : BallsInterception[T] = (inter1, inter2) match {
    case (BallsInterceptionEmpty, _) => inter2
    case (_, BallsInterceptionEmpty) => inter1
    case (i1: BallsInterceptionNonEmpty[T], i2: BallsInterceptionNonEmpty[T]) =>
      BallsInterceptionNonEmpty(i1.balls union i2.balls)
  }


  def toStringAux[T](input: BallsInterception[T]): String =
    input match {
      case BallsInterceptionEmpty => ""
      case BallsInterceptionNonEmpty(balls) => s"${balls.mkString(",")}"
    }

  def intercept[T](inter1: BallsInterception[T], inter2: BallsInterception[T])(implicit metric: Metric[T])
  : BallsInterception[T] = (inter1, inter2) match {
    case (BallsInterceptionEmpty, _) => BallsInterceptionEmpty
    case (_, BallsInterceptionEmpty) => BallsInterceptionEmpty
    case (i1: BallsInterceptionNonEmpty[T], i2: BallsInterceptionNonEmpty[T]) =>
      val b1 = i1.balls.head
      val b2 = i2.balls.head
      if (isBallInsideInterception(b1, inter2))
        i1
      else if (isBallInsideInterception(b2, inter1))
        i2
      else if (isBallOverlappedWithInterception(b1, i2))
        build_without_check_interceptions(i1, i2)
      else if (isBallOverlappedWithInterception(b2, i1))
        build_without_check_interceptions(i1, i2)
      else
        BallsInterceptionEmpty

  }

  def isBallInsideInterception[T](ball: Ball[T], interception: BallsInterception[T])(implicit metric: Metric[T])
  : Boolean = interception match {
    case BallsInterceptionEmpty => false
    case BallsInterceptionNonEmpty(balls) =>
      balls.forall(otherBall => isBallInsideBall(ball, otherBall,
        metric.distance(ball.center, otherBall.center)))
  }

  def isBallOverlappedWithInterception[T](ball: Ball[T], interception: BallsInterception[T])(implicit metric: Metric[T])
  : Boolean = interception match {
    case BallsInterceptionEmpty => false
    case BallsInterceptionNonEmpty(balls) =>
      balls.forall(otherBall => isBallInterceptingBall(ball, otherBall,
        metric.distance(ball.center, otherBall.center)))
  }

}


