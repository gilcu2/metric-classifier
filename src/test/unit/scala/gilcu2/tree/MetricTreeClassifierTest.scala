package gilcu2.tree

import gilcu2.spaces.RNDensePoint
import org.scalatest.{FlatSpec, GivenWhenThen, Matchers}

class MetricTreeClassifierTest extends FlatSpec with Matchers with GivenWhenThen {

  behavior of "MetricTreeClassifier"

  it should "classify in the class of the nearest point" in {

    Given("a tree classifier")
    val treeCla = MetricTreeClassifier(1, 0, 1.0, RNDensePoint.euclidean)

    And("points with their classes")
    val trainPoint = Vector(
      RNDensePoint(Vector(-1, 0)),
      RNDensePoint(Vector(1, 0))
    )
    val trainClasses = Vector(1, 2)

    And("the object to classify and its expected class")
    val testPoints = Vector(
      RNDensePoint(Vector(0.5, 0))
    )
    val expectedClasses = Vector(2)

    When("the tree is trained and classify")
    treeCla.train(trainPoint, trainClasses)
    val results = treeCla.classify(testPoints)

    Then("the classes must be the expected")
    results shouldBe expectedClasses

  }

}
