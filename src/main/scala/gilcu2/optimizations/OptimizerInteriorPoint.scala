package gilcu2.optimizations

import gilcu2.balls.Ball
import gilcu2.spaces.RNDensePoint
import optimus.optimization._
import optimus.optimization.enums.SolverLib
import optimus.optimization.model.MPFloatVar

object OptimizerInteriorPoint {

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

    val minX = math.min(xp0, xb0) - ball.radio
    val maxX = math.max(xp0, xb0) + ball.radio

    val minY = math.min(yp0, yb0) - ball.radio
    val maxY = math.max(yp0, yb0) + ball.radio

    implicit val model = MPModel(SolverLib.Mosek)

    val x = MPFloatVar("x", minX, maxX)
    val y = MPFloatVar("y", minY, maxY)

    minimize((x - xp0) * (x - xp0) + (y - yp0) * (y - yp0))
    subjectTo(
      (x - xb0) * (x - xb0) + (y - yb0) * (y - yb0) <:= ball.radio
    )

    start()

    val r = (x.value.get, y.value.get, objectiveValue)

    release()

    r
  }

  def maximizeDistancesFromPointToBallR2(point: RNDensePoint, ball: Ball[RNDensePoint],
                                         solver: SolverLib = SolverLib.Mosek): (Double, Double, Double) = {

    val xp0 = point.coordinates(0)
    val yp0 = point.coordinates(1)

    val xb0 = ball.center.coordinates(0)
    val yb0 = ball.center.coordinates(1)

    val minX = math.min(xp0, xb0) - ball.radio
    val maxX = math.max(xp0, xb0) + ball.radio

    val minY = math.min(yp0, yb0) - ball.radio
    val maxY = math.max(yp0, yb0) + ball.radio

    implicit val model = MPModel(solver)

    val x = MPFloatVar("x", minX, maxX)
    val y = MPFloatVar("y", minY, maxY)

    maximize((x - xp0) * (x - xp0) + (y - yp0) * (y - yp0))
    subjectTo(
      (x - xb0) * (x - xb0) + (y - yb0) * (y - yb0) <:= ball.radio
    )

    start()

    val r = (x.value.get, y.value.get, objectiveValue)

    release()

    r
  }


}
