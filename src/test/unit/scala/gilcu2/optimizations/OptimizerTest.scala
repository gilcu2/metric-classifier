package gilcu2.optimizations

import breeze.numerics.abs
import gilcu2.balls.Ball
import gilcu2.spaces.RNDensePoint
import optimus.optimization._
import optimus.optimization.enums.SolverLib
import optimus.optimization.model.MPFloatVar
import org.scalatest.{FlatSpec, GivenWhenThen, Matchers}

class OptimizerTest extends FlatSpec with Matchers with GivenWhenThen {

  behavior of "Optimizer"

  it should "find the maximun value of linear problem" in {

    val (x, y, max) = Optimizer.maximizeLinearR2(100, 200, 80, 170)
    max shouldBe 650
  }

  it should "find the minimum value of quadratic problem with linear constraint" in {

    val (x, y, min) = Optimizer.minimizeQuadraticR2LinearConstraints(0, 0, 0, 0)
    min shouldBe -31
  }

  //  it should "find the minimum value of quadratic problem with quadratic constraint" in {
  //
  //    val (x, y, min) = Optimizer.minimizeQuadraticR2QuadraticConstraints(0, 0, 0, 0)
  //    min shouldBe <(0)
  //  }


  //  it should "compute gurobi example" in {
  //    implicit val cp: MPModel = MPModel(SolverLib.Gurobi)
  //
  //    val x = MPFloatVar("x", 100, 200)
  //    val y = MPFloatVar("y", 80, 170)
  //
  //    maximize(-5)
  //
  //    add(x >:= 5)
  //    add(y <:= 100)
  //
  //    start()
  //
  //  }

  it should "compute mosek example" in {
    implicit val qp: MPModel = MPModel(SolverLib.Mosek)

    val x = MPFloatVar("x")
    val y = MPFloatVar("y", -0.5, 0.5)

    maximize(x)
    add(x * x + y * y <:= 1)

    start()

  }

}
