package gilcu2.tree

import scalism.Metric

sealed trait MetricTreeClassifier[T] {

  def metric: Metric[T]

}

object MetricTreeClassifier {

  def apply[T](): MetricTreeClassifier[T] = new MetricTreeClassifier[T]

}