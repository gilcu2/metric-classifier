package gilcu2.spaces

trait Metric[A] {

  def distance(x: A, y: A): Double

  def apply(x: A, y: A): Double = distance(x, y)

}
