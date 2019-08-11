package gilcu2.optimizations

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


  def minimizeQuadraticR2(xMin: Double, xMax: Double, yMin: Double, yMax: Double): (Double, Double, Double) = {

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

}
