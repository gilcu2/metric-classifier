package gilcu2.optimizations

import gilcu2.spaces.RNDensePoint

case class Result(x1: RNDensePoint, x2: RNDensePoint, fValue: Double, lastDelta: Double)

class Constraint(val f: RNDensePoint => Double,
                 val jacobian: RNDensePoint => RNDensePoint,
                 val limit: Double) {
  def satisfy(x: RNDensePoint): Boolean = {
    val value = f(x)
    value <= limit
  }
}

object OptimizerFunctionTwoPointsWithConstraints {

  def minimize(f: (RNDensePoint, RNDensePoint) => Double,
               gradFX1: (RNDensePoint, RNDensePoint) => RNDensePoint,
               gradFX2: (RNDensePoint, RNDensePoint) => RNDensePoint,
               constraintsX1: Vector[Constraint],
               constraintsX2: Vector[Constraint]
              )(initialX1: RNDensePoint, initialX2: RNDensePoint, initialDelta: Double)(
                implicit epsilon: Double): Result = {
    var cond = true
    var rPrevious = findFirstGoodDeltaOneDirection(f, gradFX1, gradFX2, constraintsX1, constraintsX2)(
      initialX1, initialX2, initialDelta)
    var rNew = rPrevious.copy()

    while (cond) {
      val newX1 = getNewXOtherDirection(constraintsX1, rNew.lastDelta, rNew.x1)
      rNew = optimizeOneDirection(f, gradFX1, gradFX2, constraintsX1, constraintsX2)(
        newX1, rNew.x2, rNew.lastDelta)

      val newX2 = getNewXOtherDirection(constraintsX2, rNew.lastDelta, rNew.x2)
      rNew = optimizeOneDirection(f, gradFX1, gradFX2, constraintsX1, constraintsX2)(
        rNew.x1, newX2, rNew.lastDelta)

      if (rNew.fValue < rPrevious.fValue) {
        rPrevious = rNew
        rNew = rPrevious.copy()
      }
      else
        cond = false
    }
    rPrevious
  }

  def optimizeOneDirection(f: (RNDensePoint, RNDensePoint) => Double,
                           gradFX1: (RNDensePoint, RNDensePoint) => RNDensePoint,
                           gradFX2: (RNDensePoint, RNDensePoint) => RNDensePoint,
                           constraintsX1: Vector[Constraint],
                           constraintsX2: Vector[Constraint])(
                            initialX1: RNDensePoint, initialX2: RNDensePoint,
                            initialDelta: Double
                          )(implicit epsilon: Double): Result = {
    var delta = initialDelta
    var prevX1 = initialX1
    var prevX2 = initialX2
    var prevF = f(prevX1, prevX2)
    var cond = true
    var inside = isInsideRegion(constraintsX1, initialX1) || isInsideRegion(constraintsX2, initialX2)

    while (cond) {
      val (newX1, insideX1) = getNewX1InLine(gradFX1, constraintsX1, delta, prevX1, prevX2)
      val (newX2, insideX2) = getNewX2InLine(gradFX2, constraintsX2, delta, prevX1, prevX2)
      if (inside != insideX1 || insideX2) {
        cond = false
        inside = insideX1 || insideX2
      }

      println(s"X1: $newX1 x2: $newX2 delta: $delta")

      if (inside) {
        val newF = f(newX1, newX2)
        if (prevF > newF) {
          prevX1 = newX1
          prevX2 = newX2
          prevF = newF
        } else
          cond = false
      }

      if (delta > epsilon)
        delta /= 2
      else
        cond = false

    }
    Result(prevX1, prevX2, prevF, delta)
  }

  private def getNewX2InLine(gradFX2: (RNDensePoint, RNDensePoint) => RNDensePoint,
                             constraintsX2: Vector[Constraint], delta: Double,
                             prevX1: RNDensePoint, prevX2: RNDensePoint):
  (RNDensePoint, Boolean) = {
    val gradFX2Value = gradFX2(prevX1, prevX2)
    val newX2Try = prevX2 - gradFX2Value * delta
    if (isInsideRegion(constraintsX2, newX2Try))
      (newX2Try, true)
    else
      (prevX2, false)
  }

  private def getNewX1InLine(gradFX1: (RNDensePoint, RNDensePoint) => RNDensePoint,
                             constraintsX1: Vector[Constraint],
                             delta: Double, prevX1: RNDensePoint, prevX2: RNDensePoint)
  : (RNDensePoint, Boolean) = {
    val gradFX1Value = gradFX1(prevX1, prevX2)

    val newX1Try = prevX1 - gradFX1Value * delta
    if (isInsideRegion(constraintsX1, newX1Try))
      (newX1Try, true)
    else
      (prevX1, false)
  }

  private def isInsideRegion(constraintsX1: Vector[Constraint], newX1Try: RNDensePoint): Boolean = {
    constraintsX1.forall(_.satisfy(newX1Try))
  }

  private def getNewXOtherDirection(constraintsX1: Vector[Constraint],
                                    delta: Double, prevX: RNDensePoint)(
                                     implicit epsilon: Double
                                   ): RNDensePoint

  = {

    val constraints_equal = constraintsX1.filter(constraint => (constraint.f(prevX) - constraint.limit) < epsilon)

    if (constraints_equal.length == 1) {
      val newX = prevX - constraints_equal.head.jacobian(prevX) * delta
      newX
    }
    else
      prevX

  }

  def findFirstGoodDeltaOneDirection(f: (RNDensePoint, RNDensePoint) => Double,
                                     gradFX1: (RNDensePoint, RNDensePoint) => RNDensePoint,
                                     gradFX2: (RNDensePoint, RNDensePoint) => RNDensePoint,
                                     constraintsX1: Vector[Constraint],
                                     constraintsX2: Vector[Constraint])(
                                      initialX1: RNDensePoint, initialX2: RNDensePoint,
                                      initialDelta: Double
                                    )(implicit epsilon: Double): Result = {
    var delta = initialDelta
    var prevX1 = initialX1
    var prevX2 = initialX2
    var prevF = f(prevX1, prevX2)
    var cond = true

    while (cond) {
      val (newX1, insideX1) = getNewX1InLine(gradFX1, constraintsX1, delta, prevX1, prevX2)
      val (newX2, insideX2) = getNewX2InLine(gradFX2, constraintsX2, delta, prevX1, prevX2)
      if (insideX1 || insideX2) {
        cond = false

        val newF = f(newX1, newX2)
        if (prevF > newF) {
          prevX1 = newX1
          prevX2 = newX2
          prevF = newF
        }
      } else if (delta > epsilon)
        delta /= 2
      else
        cond = false
    }

    Result(prevX1, prevX2, prevF, delta)
  }

  private def areNewPointsDifferent(prevX1: RNDensePoint, prevX2: RNDensePoint,
                                    newX1: RNDensePoint, newX2: RNDensePoint): Boolean

  =
    (newX1 - prevX1).norm > 0.0 || (newX2 - prevX2).norm > 0.0

}
