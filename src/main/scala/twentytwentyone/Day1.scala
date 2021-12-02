package twentytwentyone

import scala.io.Source

object Day1 {
  def main(args: Array[String]): Unit = {
    println(s"Part 1: ${increases(input())}")
    println(s"Part 2: ${tripletIncreases(input())}")
  }

  def increases(input: Seq[Int]): Int = input.sliding(2).count(pair => pair(0) < pair(1))

  def input(): Seq[Int] = Source.fromResource("2021/input_day_1.txt").getLines.map(_.toInt).toSeq

  private def tripletSums(input: Seq[Int]): Seq[Int] = input.sliding(3).toSeq.map(_.sum)

  def tripletIncreases: Seq[Int] => Int = increases _ compose tripletSums _
}
