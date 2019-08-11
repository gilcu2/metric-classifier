package gilcu2.spaces

import org.scalatest.{FlatSpec, GivenWhenThen, Matchers}

class EuclideanTest extends FlatSpec with Matchers with GivenWhenThen {

  behavior of "Euclidean"

  it should "compute the euclidean distance between points" in {

    Given("two points")
    val p1 = RNDensePoint(0.0, 0.0)
    val p2 = RNDensePoint(1.0, 0.0)

    When("compute the distance")
    val d = Euclidean(p1, p2)

    Then("the distance must be the expected")
    d shouldBe 1

  }

}
