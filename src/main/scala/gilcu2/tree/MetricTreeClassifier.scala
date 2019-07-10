package gilcu2.tree

import gilcu2.tree.MetricTreeClassifier._
import scalism.Metric


case class MetricTreeClassifier[T](alpha: Int, beta: Int, gamma: Double, metric: Metric[T]) {

  def train(points: Vector[T], classes: Vector[Class]): Unit = {}

  def classify(points: Iterable[T]): Classification = Vector[Classes]()

}

object MetricTreeClassifier {

  type Class = Int
  type Classes = Set[Class]
  type Classification = Vector[Classes]

}