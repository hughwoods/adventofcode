package twentytwentyone

import scala.io.Source

object Day1 {
  def main(args: Array[String]): Unit = {
    println(s"Part 1: ${increases(input())}")
    println(s"Part 2: ${countOfIncreasingReadings(3)(input())}")
  }

  def increases: Seq[Int] => Int = countOfIncreasingReadings(1)

  def countOfIncreasingReadings(windowSize: Int)(input: Seq[Int]): Int =
    input
      .sliding(windowSize)
      .map(_.sum)
      .sliding(2)
      .count(pair => pair(0) < pair(1))

  def input(): Seq[Int] =
    Source.fromResource("2021/input_day_1.txt").getLines.map(_.toInt).toSeq
}
