package gilcu2.optimizations

import org.scalatest.{FlatSpec, GivenWhenThen, Matchers}
import gilcu2.optimizations.OptimizerFunctionTwoPointsWithConstraints._

class OptimizerFunctionTwoPointsWithConstraintsTest extends FlatSpec with Matchers with GivenWhenThen {

  behavior of "OptimizerFunctionTwoPointsWithConstraints"

  def sqr(x: Double): Double = x * x

  it should "find the minimum distance between to balls" in {

    Given("the function,restriction, jacobians and initial points")

    def f(arg: Argument): Double =
      (arg.x1 - arg.x2).norm

    def grad(argument: Argument): Argument = {

    }
  }

}
