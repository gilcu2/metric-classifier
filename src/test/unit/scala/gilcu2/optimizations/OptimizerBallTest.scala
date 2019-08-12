package gilcu2.optimizations

import gilcu2.balls.Ball
import gilcu2.spaces.RNDensePoint
import org.scalatest.{FlatSpec, GivenWhenThen, Matchers}
import optimus.optimization.enums.SolverLib

class OptimizerBallTest extends FlatSpec with Matchers with GivenWhenThen {

  behavior of "Optimizer"

  val epsilon = 0.000001

  it should "find the minimum distance from point to circumference" in {

    Given("the circumference and point")
    val circumference = Ball(RNDensePoint(0.0, 0.0), radio = 1.0)
    val point = RNDensePoint(2.0, 0.0)

    And("the expected results")
    val xExpected = 1.0
    val yExpected = 0.0
    val dExpected = 1.0

    When("the minimum distance is computed")
    val (xmin, ymin, dmin) = Optimizer.minimizeDistancesFromPointToBallR2(point, circumference)

    Then("the values must be the expected")
    xmin shouldBe xExpected +- epsilon
    ymin shouldBe yExpected +- epsilon
    dmin shouldBe dExpected +- epsilon

  }

  it should "find the maximum distance from point to circumference" in {

    Given("the circumference and point")
    val circumference = Ball(RNDensePoint(0.0, 0.0), radio = 1.0)
    val point = RNDensePoint(2.0, 0.0)

    And("the expected results")
    val xExpected = -1.0
    val yExpected = 0.0
    val dExpected = 3.0

    When("the minimum distance is computed")
    val (xmin, ymin, dmax) = Optimizer.maximizeDistancesFromPointToBallR2(point, circumference,
      solver = SolverLib.Gurobi)

    Then("the values must be the expected")
    xmin shouldBe xExpected +- epsilon
    ymin shouldBe yExpected +- epsilon
    dmax shouldBe dExpected +- epsilon

  }


}
