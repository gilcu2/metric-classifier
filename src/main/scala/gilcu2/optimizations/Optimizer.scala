package gilcu2.optimizations

import gilcu2.balls.Ball
import gilcu2.spaces.RNDensePoint
import optimus.optimization._
import optimus.optimization.enums.SolverLib
import optimus.optimization.model.MPFloatVar

object Optimizer {

  def maximizeLinearR2(xMin: Double, xMax: Double, yMin: Double, yMax: Double): (Double, Double, Double) = {

    implicit val model = MPModel(SolverLib.oJSolver)

    val x = MPFloatVar("x", xMin, xMax)
    val y = MPFloatVar("y", yMin, yMax)

    maximize(-2 * x + 5 * y)
    subjectTo(
      y >:= 200 - x
    )

    start()

    val r = (x.value.get, y.value.get, objectiveValue)

    release()

    r
  }

  def minimizeQuadraticR2LinearConstraints(xMin: Double, xMax: Double, yMin: Double, yMax: Double): (Double, Double, Double) = {

    implicit val model = MPModel(SolverLib.oJSolver)

    val x = MPFloatVar.positive("x")
    val y = MPFloatVar.positive("y")

    minimize(-8 * x - 16 * y + x * x + 4 * y * y)
    subjectTo(
      x + y <:= 5,
      x <:= 3
    )

    start()

    val r = (x.value.get, y.value.get, objectiveValue)

    release()

    r
  }

  def minimizeQuadraticR2QuadraticConstraints(xMin: Double, xMax: Double, yMin: Double, yMax: Double): (Double, Double, Double) = {

    implicit val model = MPModel(SolverLib.Gurobi)

    val x = MPFloatVar.positive("x")
    val y = MPFloatVar.positive("y")

    minimize(-8 * x - 16 * y + x * x + 4 * y * y)
    subjectTo(
      x + y * y <:= 5,
      x <:= 3
    )

    start()

    val r = (x.value.get, y.value.get, objectiveValue)

    release()

    r
  }

  def minimizeDistancesFromPointToBallR2(point: RNDensePoint, ball: Ball[RNDensePoint]): (Double, Double, Double) = {

    val xp0 = point.coordinates(0)
    val yp0 = point.coordinates(1)

    val xb0 = ball.center.coordinates(0)
    val yb0 = ball.center.coordinates(1)

    implicit val model = MPModel(SolverLib.oJSolver)

    val x = MPFloatVar("x", 0, 2)
    val y = MPFloatVar("y", -1, 1)

    minimize(x * x - 4 * x + 4 + y * y)
    subjectTo(
      x * x + y * y <:= 1.0
    )

    start()

    val r = (x.value.get, y.value.get, objectiveValue)

    release()

    r
  }


}
