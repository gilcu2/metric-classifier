package gilcu2.classifier

import gilcu2.spaces.Metric
import gilcu2.classifier.MetricTreeClassifier._


case class MetricTreeClassifier[T](alpha: Int, beta: Int, gamma: Double, metric: Metric[T]) {

  def train(points: Vector[T], classes: Vector[Class]): Unit = {}

  def classify(points: Iterable[T]): Classification = Vector[Classes]()

}

object MetricTreeClassifier {

  type Class = Int
  type Classes = Set[Class]
  type Classification = Vector[Classes]

}