package gilcu2.optimizations

import gilcu2.balls.Ball
import org.scalatest.{FlatSpec, GivenWhenThen, Matchers}
import gilcu2.optimizations.OptimizerFunctionTwoPointsWithConstraints._
import gilcu2.spaces.RNDensePoint

class OptimizerFunctionTwoPointsWithConstraintsTest extends FlatSpec with Matchers with GivenWhenThen {

  behavior of "OptimizerFunctionTwoPointsWithConstraints"

  implicit val epsilon = 0.0001

  def sqr(x: Double): Double = x * x

  it should "find the minimum distance between to balls, initials in line with solution" in {

    Given("the function,restriction, jacobians and balls")

    val ball1 = Ball(RNDensePoint(0.0, 0.0), 1.0)
    val ball2 = Ball(RNDensePoint(3.0, 0.0), 1.0)

    def distance(x1: RNDensePoint, x2: RNDensePoint): Double =
      (x1 - x2).norm

    def gradDistanceX1(x1: RNDensePoint, x2: RNDensePoint): RNDensePoint =
      x1 - x2

    def gradDistanceX2(x1: RNDensePoint, x2: RNDensePoint): RNDensePoint =
      x2 - x1

    def fConstraintBall1(x: RNDensePoint): Double =
      (x - ball1.center).norm

    def jacobianConstraintBall1(x: RNDensePoint): RNDensePoint =
      (x - ball1.center)

    val constraintBall1 = new Constraint(fConstraintBall1, jacobianConstraintBall1, 0, ball1.radio)

    def fConstraintBall2(x: RNDensePoint): Double =
      (x - ball2.center).norm

    def jacobianConstraintBall2(x: RNDensePoint): RNDensePoint =
      (x - ball2.center)

    val constraintBall2 = new Constraint(fConstraintBall2, jacobianConstraintBall2, 0, ball2.radio)

    val (initialX1, initialX2) = (ball1.center, ball2.center)
    val initialDelta = math.max(ball1.radio, ball2.radio)

    When("the function is minimized")
    val r = minimize(distance, gradDistanceX1, gradDistanceX2,
      Vector(constraintBall1), Vector(constraintBall2))(initialX1, initialX2, initialDelta)

    Then("results must be the expected")
    r.x1 shouldBe RNDensePoint(1.0, 0.0)
    r.x2 shouldBe RNDensePoint(2.0, 0.0)
    r.fValue shouldBe 1.0
  }

}
