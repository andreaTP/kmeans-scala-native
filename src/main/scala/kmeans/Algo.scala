package kmeans

import java.lang.Math.sqrt

object Algo {
  val n = 10
  val iters = 15

  class Point(val x: Double, val y: Double) {
    def /(k: Double): Point = new Point(x / k, y / k)

    def +(p: Point) = new Point(x + p.x, y + p.y)
    def -(p: Point) = new Point(x - p.x, y - p.y)

    def modulus = sqrt(sq(x) + sq(y))
  }

  def run(xs: List[Point]) = {
    var centroids = xs take n

    var _iters = iters
    while (_iters > 0) {
      centroids = clusters(xs, centroids) map average

      _iters -= 1
    }

    clusters(xs, centroids)
  }

  def clusters(xs: List[Point], centroids: List[Point]) =
    (xs groupBy { x => closest(x, centroids) }).values.toList

  def closest(x: Point, choices: List[Point]) =
    choices minBy { y => dist(x, y) }

  def sq(x: Double) = x * x

  def dist(x: Point, y: Point) = (x - y).modulus

  def average(xs: List[Point]) = xs.reduce(_ + _) / xs.size
}
