package gilcu2.optimizations

import gilcu2.spaces.RNDensePoint

case class Result(x1: RNDensePoint, x2: RNDensePoint, fValue: Double)

class Constraint(val f: RNDensePoint => Double,
                 val jacobian: RNDensePoint => RNDensePoint,
                 val lowLimit: Double, val highLimit: Double) {
  def satisfy(x: RNDensePoint): Boolean = {
    val value = f(x)
    value >= lowLimit && value <= highLimit
  }
}

object OptimizerFunctionTwoPointsWithConstraints {

  def minimize(f: (RNDensePoint, RNDensePoint) => Double,
               gradFX1: (RNDensePoint, RNDensePoint) => RNDensePoint,
               gradFX2: (RNDensePoint, RNDensePoint) => RNDensePoint,
               constraintsX1: Vector[Constraint],
               constraintsX2: Vector[Constraint]
              )(initialX1: RNDensePoint, initialX2: RNDensePoint, initialDelta: Double)(implicit epsilon: Double): Result = {
    var delta = initialDelta
    var prevX1 = initialX1
    var prevX2 = initialX2
    var prevF = f(prevX1, prevX2)
    var cond = true
    while (cond) {
      val gradFX1Value = gradFX1(prevX1, prevX2)

      val newX1Try = prevX1 + gradFX1Value * delta
      val newX1 = if (constraintsX1.forall(_.satisfy(newX1Try)))
        newX1Try
      else
        prevX1

      val gradFX2Value = gradFX2(prevX1, prevX2)
      val newX2Try = prevX2 + gradFX2Value * delta
      val newX2 = if (constraintsX2.forall(_.satisfy(newX2Try)))
        newX2Try
      else
        prevX2

      if ((newX1 - prevX1).norm > 0.0 || (newX2 - prevX2).norm > 0.0) {
        val newF = f(newX1, newX2)
        if (prevF > newF) {
          prevX1 = newX1
          prevX2 = newX2
          prevF = newF
        } else
          cond = false
      }
      else if (delta > epsilon)
        delta /= 2
      else
        cond = false
    }
    Result(prevX1, prevX2, prevF)
  }

}
