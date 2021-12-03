package twentytwentyone

import scala.collection.MapView
import scala.io.Source

object Day3 {

  def parseBits(bs: Seq[Char]): Int = Integer.parseInt(bs.mkString, 2)

  def main(args: Array[String]): Unit = {
    println(Part1.gamma(input()) * Part1.epsilon(input()))
    println(Part2.o2(input()) * Part2.co2(input()))
  }

  def input(): Seq[String] = Source.fromResource("2021/input_day_3.txt").getLines.toSeq

  object Part1 {
    def gammaChars(input: Seq[String]) =
      input.transpose.map(_.groupBy(identity).view.mapValues(_.length).maxBy(_._2)._1)
    val gamma = (input: Seq[String]) => parseBits(gammaChars(input).mkString)
    val epsilon = (input: Seq[String]) => (math.pow(2, input.head.length) - 1 - gamma(input)).toInt
  }

  object Part2 {
    val o2 = (input: Seq[String]) => parseBits(find(input, 0)(_ == _))
    val co2 = (input: Seq[String]) => parseBits(find(input, 0)(_ != _))
    def tieBreak(groups: MapView[Char, Int]): Char =
      if (groups.values.toSet.size == 1) '1' else groups.maxBy(_._2)._1

    def find(in: Seq[String], pos: Int)(pred: (Char, Char) => Boolean): String = {
      lazy val c: Char = tieBreak(
        in.map(_.apply(pos)).groupBy(identity).view.mapValues(_.length)
      )
      in match {
        case x :: Nil => x
        case _        => find(in.filter(l => pred(l(pos), c)), pos + 1)(pred)
      }
    }
  }
}
