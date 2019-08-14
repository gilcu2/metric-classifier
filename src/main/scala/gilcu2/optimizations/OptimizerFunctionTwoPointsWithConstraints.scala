package gilcu2.optimizations

import gilcu2.spaces.RNDensePoint

object OptimizerFunctionTwoPointsWithConstraints {

  case class Argument(x1: RNDensePoint, x2: RNDensePoint)

  case class Result(x1: RNDensePoint, x2: RNDensePoint, fValue: Double)

  def minimize(
                f: Argument => Double,
                gradF: Argument => Argument,
                constraints: Vector[Argument => Double],
                constraintsJacobian: Vector[Argument => Argument]
              )(initial: Argument): Result = {
    Result(initial.x1, initial.x2, 0.0)
  }

}
