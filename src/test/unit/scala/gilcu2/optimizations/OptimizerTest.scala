package gilcu2.optimizations

import org.scalatest.{FlatSpec, GivenWhenThen, Matchers}

class OptimizerTest extends FlatSpec with Matchers with GivenWhenThen {

  behavior of "Optimizer"

  it should "find the maximun value of linear problem" in {

    val (x, y, max) = Optimizer.maximizeLinearR2(100, 200, 80, 170)
    max shouldBe 650
  }

  it should "find the minimum value of quadratic problem" in {

    val (x, y, min) = Optimizer.minimizeQuadraticR2(0, 0, 0, 0)
    min shouldBe <(0)
  }

}
