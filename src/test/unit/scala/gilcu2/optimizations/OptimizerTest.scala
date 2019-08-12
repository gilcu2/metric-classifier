package gilcu2.optimizations

import gilcu2.balls.Ball
import gilcu2.spaces.RNDensePoint
import org.scalatest.{FlatSpec, GivenWhenThen, Matchers}
import optimus.optimization.model.{MPBinaryVar, MPConstraint, MPFloatVar, MPIntVar}
import optimus.optimization._
import optimus.optimization.enums.SolverLib

class OptimizerTest extends FlatSpec with Matchers with GivenWhenThen {

  behavior of "Optimizer"

  it should "find the maximun value of linear problem" in {

    val (x, y, max) = Optimizer.maximizeLinearR2(100, 200, 80, 170)
    max shouldBe 650
  }

  it should "find the minimum value of quadratic problem with linear constraint" in {

    val (x, y, min) = Optimizer.minimizeQuadraticR2LinearConstraints(0, 0, 0, 0)
    min shouldBe <(0)
  }

  it should "find the minimum value of quadratic problem with quadratic constraint" in {

    val (x, y, min) = Optimizer.minimizeQuadraticR2QuadraticConstraints(0, 0, 0, 0)
    min shouldBe <(0)
  }

  it should "find the minimum distance from point to circumference" in {

    Given("the circumference and point")
    val circumference = Ball(RNDensePoint(0.0, 0.0), radio = 1.0)
    val point = RNDensePoint(2.0, 0.0)

    When("the minimum distance is computed")
    val (xmin, ymin, dmin) = Optimizer.minimizeDistancesFromPointToBallR2(point, circumference)

    Then("the values must be the expected")
    xmin shouldBe 1.0
    ymin shouldBe 0.0
    dmin shouldBe 1.0

    val (x, y, min) = Optimizer.minimizeQuadraticR2LinearConstraints(0, 0, 0, 0)
    min shouldBe <(0)
  }

  it should "compute gurobi example" in {
    implicit val cp: MPModel = MPModel(SolverLib.Gurobi)

    val x = MPFloatVar("x", 100, 200)
    val y = MPFloatVar("y", 80, 170)

    maximize(-5)

    add(x >:= 5)
    add(y <:= 100)

    start()

  }

}
