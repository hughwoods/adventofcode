package twentytwentyone
import cats.data.NonEmptyList
import cats.effect.{IO, IOApp}
import cats.implicits._
import cats.parse.Parser
import cats.parse.Rfc5234.digit
import twentytwentyone.Day6.parsers.ages

import scala.annotation.tailrec
import scala.io.Source

object Day6 extends IOApp.Simple {

  type Population = Map[Int, Long]

  val run: IO[Unit] =
    for (
      str <- input();
      _ <- printResult(str)
    ) yield ()

  def printResult(str: String): IO[Unit] =
    ages.parse(str) match {
      case Left(err) => IO { println(s"Failed: $err") }
      case Right((_, parsed)) =>
        IO {
          println(result(parsed.toList, 80))
          println(result(parsed.toList, 256))
        }
    }

  def result(in: Seq[Int], cycles: Int): Long = simulateN(group(in), cycles).map(_._2).sum

  def group(s: Seq[Int]): Population = s.groupBy(identity).view.mapValues(_.length.toLong).toMap

  def simulateN(pop: Population, n: Int): Population = {
    @tailrec
    def iter(pop: Population, i: Int): Population =
      if (i == n) pop
      else iter(simulateOnce(pop), i + 1)
    iter(pop, 0)
  }

  def simulateOnce(pop: Population): Population = {
    val (r, others) = pop.map { case (k, v) => (k - 1, v) }.partition(_._1 < 0)
    val rs = r.map(_._2).sum
    others |+| Map(6 -> rs, 8 -> rs)
  }

  def input(): IO[String] =
    IO.blocking(Source.fromResource("2021/input_day_6.txt").getLines.toSeq.head)

  object parsers {
    private lazy val age: Parser[Int] = digit.string.map(_.toInt)
    private lazy val comma: Parser[Unit] = Parser.char(',').void
    val ages: Parser[NonEmptyList[Int]] = age.repSep(comma)
  }
}
