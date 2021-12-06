package twentytwentyone

import scala.io.Source
import scala.annotation.tailrec

object Day5 {

  def input(): Seq[String] = Source.fromResource("2021/input_day_5.txt").getLines.toSeq

  val lines: Seq[LineSegment] =
    input.map(_.split(" -> ").map(_.split(',').map(Integer.parseInt).toSeq).toSeq).map(LineSegment)

  val partOne =
    overlaps(
      lines
        .filter(ls => (ls.gradient._1 == 0 || ls.gradient._2 == 0))
    )

  val partTwo = overlaps(lines)

  def overlaps(ls: Seq[LineSegment]) = ls
    .flatMap(_.points.toSeq)
    .groupBy(identity)
    .view
    .mapValues(_.length)
    .count(_._2 > 1)

  def main(args: Array[String]): Unit = {
    println(partOne)
    println(partTwo)
  }

  case class LineSegment(input: Seq[Seq[Int]]) {
    val start = (input(0)(0), input(0)(1))
    val end = (input(1)(0), input(1)(1))
    val gradient = (math.signum(end._1 - start._1), math.signum(end._2 - start._2))
    lazy val points = {
      @tailrec
      def pointsAcc(pointer: (Int, Int), acc: Set[(Int, Int)]): Set[(Int, Int)] = {
        if ((pointer._1 + gradient._1, pointer._2 + gradient._2) == end)
          acc + pointer + end
        else
          pointsAcc((pointer._1 + gradient._1, pointer._2 + gradient._2), acc + pointer)
      }
      pointsAcc(start, Set.empty)
    }
  }
}
