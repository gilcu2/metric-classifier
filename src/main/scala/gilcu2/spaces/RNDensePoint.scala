package gilcu2.spaces

import scala.math.sqrt

object Euclidean extends Metric[RNDensePoint] {

  override def distance(p1: RNDensePoint, p2: RNDensePoint) = (p1 - p2).norm
}

//TODO Consider the case when the lengths are different

object RNDensePoint {

  def apply(coordinates: Vector[Double]): RNDensePoint = new RNDensePoint(coordinates)

  def apply(coordinates: Double*): RNDensePoint = new RNDensePoint(coordinates.toVector)

}

class RNDensePoint(val coordinates: Vector[Double]) {

  def -(other: RNDensePoint): RNDensePoint =
    new RNDensePoint(this.coordinates.zip(other.coordinates).map { case (c1, c2) => c1 - c2 })

  def norm: Double = sqrt(this.coordinates.map(c => c * c).sum)

  override def toString: String = s"(${coordinates.mkString(",")})"
}