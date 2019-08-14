package gilcu2.balls

import gilcu2.spaces.{Euclidean, RNDensePoint}
import org.scalatest.{FlatSpec, GivenWhenThen, Matchers}

class BallsInterceptionEuclideanOneTwoBallsTest extends FlatSpec with Matchers with GivenWhenThen {

  behavior of "BallsInterception"

  implicit val metric = Euclidean

  ignore should "return empty ball interception when the interception of two balls are no overlapped with the other" in {

    Given("two balls overlapped")
    val inter1 = BallsInterception.build_without_check_interceptions(
      Ball(RNDensePoint(0.0, 0.0), 1.0),
      Ball(RNDensePoint(0.0, 1.5), 1.0)
    )

    And("a ball not overlapped with the interception")
    val inter2 = BallsInterception(Ball(RNDensePoint(0.0, -0.6), 1.0))

    When("the balls interceptions are intercepted")
    val r = BallsInterception.intercept(inter1, inter2)

    Then("the result must be empty")
    r shouldBe BallsInterceptionEmpty
  }

  it should "return the ball contained when one ball is contained in the interception, first case" in {

    Given("one ball contained in the other")
    val inter1 = BallsInterception.build_without_check_interceptions(
      Ball(RNDensePoint(-1.0, 0.0), 2.0),
      Ball(RNDensePoint(1.0, 0.0), 2.0)
    )
    val inter2 = BallsInterception(Ball(RNDensePoint(0.0, 0.0), 1.0))

    When("the balls interceptions are intercepted")
    val r = BallsInterception.intercept(inter1, inter2)

    Then("the result must be the contained ball")
    r shouldBe inter2
  }

  it should "return the ball contained when one ball is contained in the interception, second case" in {

    Given("one ball contained in the other")
    val inter1 = BallsInterception.build_without_check_interceptions(
      Ball(RNDensePoint(-1.0, 0.0), 2.0),
      Ball(RNDensePoint(1.0, 0.0), 2.0)
    )
    val inter2 = BallsInterception(Ball(RNDensePoint(0.0, 0.0), 1.0))

    When("the balls interceptions are intercepted")
    val r = BallsInterception.intercept(inter2, inter1)

    Then("the result must be the contained ball")
    r shouldBe inter2
  }

  //  it should "return the two balls when the balls are overlapped" in {
  //
  //    Given("one ball overlapped with the other")
  //    val ball1 = Ball(RNDensePoint(0.0, 0.0), 1.0)
  //    val ball2 = Ball(RNDensePoint(1.0, 0.0), 1.0)
  //    val inter1 = BallsInterception(ball1)
  //    val inter2 = BallsInterception(ball2)
  //
  //    When("the balls interceptions are intercepted")
  //    val r = BallsInterception.intercept(inter1, inter2, Euclidean)
  //
  //    Then("the result must be the two balls")
  //    r shouldBe BallsInterception.build_without_check_interceptions(ball2, ball1)
  //  }

}
