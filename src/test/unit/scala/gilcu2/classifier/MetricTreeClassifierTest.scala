package gilcu2.classifier

import gilcu2.spaces.{Euclidean, RNDensePoint}
import org.scalatest.{FlatSpec, GivenWhenThen, Matchers}

class MetricTreeClassifierTest extends FlatSpec with Matchers with GivenWhenThen {

  behavior of "MetricTreeClassifier"

  ignore should "classify in the class of the nearest point" in {

    Given("a tree classifier")
    val treeCla = MetricTreeClassifier(1, 0, 1.0, Euclidean)

    And("points with their classes")
    val trainPoint = Vector(
      RNDensePoint(-1.0, 0.0),
      RNDensePoint(1.0, 0.0)
    )
    val trainClasses = Vector(1, 2)

    And("the object to classify and its expected class")
    val testPoints = Vector(
      RNDensePoint(0.5, 0.0)
    )
    val expectedClasses = Vector(2)

    When("the tree is trained and classify")
    treeCla.train(trainPoint, trainClasses)
    val results = treeCla.classify(testPoints)

    Then("the classes must be the expected")
    results shouldBe expectedClasses

  }

}
