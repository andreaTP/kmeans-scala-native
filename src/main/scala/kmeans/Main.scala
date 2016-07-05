package kmeans

import scalanative.native._, stdlib._, stdio._
import Jansson._
import SysTime._
import Algo.Point

object Main {
/*
  def readPoints(path: String) = {
    val json = Source.fromFile(path).mkString
    implicit val formats = DefaultFormats

    parse(json).extract[List[List[Double]]] map { case List(a, b) => new Algo.Point(a, b) }
  }

  val iterations = 1
  val points = readPoints("../points.json")
  val start = System.currentTimeMillis
  for (i <- 1 to iterations) {
    Algo.run(points)
  }
  val time = (System.currentTimeMillis - start) / iterations

  println(s"Made $iterations iterations with an average of $time milliseconds")
*/

  def main(args: Array[String]): Unit = {
    val error = malloc(512).cast[Ptr[json_error_t]]

    val json = json_load_file(c"/home/andrea/workspace/kmeans/points.json", 0, error)

    if ((!json).typ != 1) {
      fprintf(stdout, c"Error parsing Json file")
      return
    }

    val xs = new Array[Point](100000)
    var i = 0

    while (i < 100000) {
      val value = json_array_get(json, i)
      val x = json_number_value(json_array_get(value,0))
      val y = json_number_value(json_array_get(value,1))

      xs(i) = new Point(x, y)

      i+=1
    }

    val before = malloc(sizeof[timeval]).cast[Ptr[timeval]]
    val after = malloc(sizeof[timeval]).cast[Ptr[timeval]]

    gettimeofday(before, null)

    var iterations = 1
    while (iterations > 0) {
      Algo.run(xs.toList)
      iterations -= 1
    }

    gettimeofday(after, null)

    val res =
      ((((!after).tv_sec - (!before).tv_sec) * 1000) +
      (((!after).tv_usec - (!before).tv_usec) / 1000)) / iterations

    fprintf(stdout, c"Average time is %d ms\n", res)
  }

}
