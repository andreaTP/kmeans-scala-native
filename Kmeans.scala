
import scalanative.native._
import scalanative.libc.stdlib._
import scala.math.sqrt

@struct
class Point(val x: Double = 0, val y: Double = 0) {
  @inline def /(k: Double): Point = new Point(x / k, y / k)

  @inline def +(p: Point) = new Point(x + p.x, y + p.y)

  @inline def -(p: Point) = new Point(x - p.x, y - p.y)

  @inline private def sq(x: Double) = x * x

  @inline def modulus = sqrt(sq(x) + sq(y))
}

@link("jansson")
@extern object Jansson {
  // in C enums become ints at runtime
  type json_type = Int

  @struct
  class json_t(
    val typ: json_type = 0,
    val refcount: CInt = 0
  )

  @struct
  class json_error_t(
    val line: CInt = 0,
    val column: CInt = 0,
    val position: CInt = 0
    // can't express source field due to #35
    // can't express test field due to #35
  )

  def json_load_file(path: Ptr[Byte], flags: CSize,
      error: Ptr[json_error_t]): Ptr[json_t] = extern

  def json_array_get(array: Ptr[json_t], index: CInt): Ptr[json_t] = extern

  def json_number_value(json: Ptr[json_t]): CDouble = extern
}

@name("sys/time")
@extern object SysTime {

  @struct
  class timeval(
    val tv_sec: CLong = 0L,
    val tv_usec: CLong = 0L
  )

  def gettimeofday(tp: Ptr[timeval], tzp: Ptr[_]): CInt = extern
}

import Jansson._
import SysTime._

object Main {
  final val n = 10
  final val iters = 15
  final val executions = 100

  def dist(x: Point, y: Point) = (x - y).modulus

  def closest(p: Point, choices: Ptr[Point], choicesSize: Int): Int = {
    var i = 1
    var minDist = dist(p, choices(0))
    val ret = malloc(sizeof[Int]).cast[Ptr[Int]]
    !ret = 0
    var actDist = minDist
    while (i < choicesSize) {
      actDist = dist(p, choices(i))
      if (actDist < minDist) {
        minDist = actDist
        !ret = i
      }
      i += 1
    }
    !ret
  }

  def average(xs: Ptr[Point], size: Int): Point = {
    val point = stackalloc[Point]
    !point = new Point(0,0)
    var i = 0
    while (i < size) {
      !point = !point + xs(i)
      i += 1
    }
    !point = !point / size.toDouble
    !point
  }

  def clusters(xs: Ptr[Point], centroids: Ptr[Point], xsSize: Int, centroidsSize: Int) = {
    var i = 0
    val cn = malloc(sizeof[Int] * centroidsSize).cast[Ptr[Int]]
    val xsDists = malloc(sizeof[Int] * xsSize).cast[Ptr[Int]]
    val newCentroids = malloc(sizeof[Point] * centroidsSize).cast[Ptr[Point]]

    while (i < xsSize) {
      xsDists(i) = closest(xs(i), centroids, centroidsSize)
      i += 1
    }

    i = 0
    while (i < centroidsSize) {
      newCentroids(i) = new Point(0,0)
      cn(i) = 0
      i += 1
    }
    i = 0
    while (i < xsSize) {
      newCentroids(xsDists(i)) = newCentroids(xsDists(i)) + xs(i)
      cn(xsDists(i)) = cn(xsDists(i)) + 1
      i += 1
    }
    i = 0
    while (i < centroidsSize) {
      newCentroids(i) = newCentroids(i) / cn(i).toDouble
      i += 1
    }

    newCentroids
  }

  def main(args: Array[String]): Unit = {
    var error = malloc(512).cast[Ptr[json_error_t]]

    val json = json_load_file(c"points.json", 0, error)

    if ((!json).typ != 1) {
      fprintf(stdout, c"Error parsing Json file")
      return
    }

    val xs = malloc(sizeof[Point] * 100000).cast[Ptr[Point]]
    var i = 0

    while (i < 100000) {
      val value = json_array_get(json, i)
      val x = json_number_value(json_array_get(value,0))
      val y = json_number_value(json_array_get(value,1))

      xs(i) = new Point(x, y)

      i+=1
    }

    var centroids = malloc(sizeof[Point] * n).cast[Ptr[Point]]

    val before = malloc(sizeof[timeval]).cast[Ptr[timeval]]
    val after = malloc(sizeof[timeval]).cast[Ptr[timeval]]

    gettimeofday(before, null)

    var j = 0
    while (j < executions) {

      i = 0
      while(i < n) {
        centroids(i) = new Point(xs(i).x, xs(i).y)
        i+=1
      }

      i = 0
      while(i < iters) {
        centroids = clusters(xs, centroids, 100000, n)
        i+=1
      }

      /*
      if (j+1 == executions) {
        i = n
        while (i > 0) {
          i-=1
          fprintf(stdout, c"centroid %d  (%f ,", centroids(i).x)
          fprintf(stdout, c"%f)\n", centroids(i).y)
        }
      }
      */
      j+=1
    }

    gettimeofday(after, null)

    val res =
      ((((!after).tv_sec - (!before).tv_sec) * 1000) +
      (((!after).tv_usec - (!before).tv_usec) / 1000)) / executions

    fprintf(stdout, c"da qui %d\n", res)

  }
}
