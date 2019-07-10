package gilcu2.points

import scalism.Metric
import scala.math.sqrt

object RNDensePoint {

  val euclidean = Metric[RNDensePoint]((p1, p2) => (p1 - p2).norm)
}

//TODO Consider the case when the length are different

case class RNDensePoint(coordinates: Vector[Double]) {

  def -(other: RNDensePoint): RNDensePoint =
    RNDensePoint(this.coordinates.zip(other.coordinates).map { case (c1, c2) => c1 - c2 })

  def norm: Double = sqrt(this.coordinates.map(c => c * c).sum)
}