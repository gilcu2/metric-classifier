package scalism

import gilcu2.points.RNDensePoint
import org.scalatest.{FlatSpec, GivenWhenThen, Matchers}

class VantagePointTreeTest extends FlatSpec with Matchers with GivenWhenThen {

  behavior of "VantagePointTree"

  implicit val rnd = Random(42)

  it should "find the nearest point" in {

    Given("some points")
    val points = Seq(
      RNDensePoint(Vector(0, 0)),
      RNDensePoint(Vector(0, 1)),
      RNDensePoint(Vector(0, 2))
    )

    And("the tree with the points")
    val tree = VantagePointTree(points, RNDensePoint.euclidean)

    And("an objetive point")
    val objetive = RNDensePoint(Vector(1, 1))

    And("the expected results")
    val expected = RNDensePoint(Vector(0, 1))

    When("the tree find the nearest")
    val nearests = tree.findKNearestNeighbours(objetive, 1)

    Then("the nearest must be the expected")
    nearests(0) shouldBe expected
  }


  it should "find the points in a ball around" in {

    Given("some points")
    val points = Seq(
      RNDensePoint(Vector(0, 0)),
      RNDensePoint(Vector(0, 1)),
      RNDensePoint(Vector(0, 2))
    )

    And("the tree with the points")
    val tree = VantagePointTree(points, RNDensePoint.euclidean)

    And("an objetive point")
    val objetive = RNDensePoint(Vector(2, 1))

    And("the ball radio")
    val radio = 2.0

    And("the expected results")
    val expected = Set(
      RNDensePoint(Vector(0, 1)),
      RNDensePoint(Vector(0, 1))
    )

    When("the tree find points in the ball")
    val closest = tree.findEpsilonNeighbours(objetive, radio)

    Then("the nearest must be the expected")
    closest.toSet shouldBe expected
  }

}
