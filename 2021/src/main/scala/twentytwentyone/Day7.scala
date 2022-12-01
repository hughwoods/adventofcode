package twentytwentyone

import cats.data.NonEmptyList
import cats.effect.{IO, IOApp}
import cats.parse.Parser
import cats.parse.Rfc5234.digit
import twentytwentyone.Day7.parsers.positions

import scala.io.Source

object Day7 extends IOApp.Simple {
  override def run: IO[Unit] = for (
    str <- input();
    _ <- printResult(str)
  ) yield ()

  def input(): IO[String] =
    IO.blocking(Source.fromResource("2021/input_day_7.txt").getLines.toSeq.head)

  def printResult(str: String): IO[Unit] =
    positions.parse(str) match {
      case Left(err) => IO { println(s"Failed: $err") }
      case Right((_, parsed)) =>
        IO { println(result1(parsed.toList)) } >>
          IO { println(result2(parsed.toList)) }
    }

  def result1(in: Seq[Int]): Int = {
    val target = in.sortWith(_ > _).apply(in.length / 2)
    in.map(s => math.abs(s - target)).sum
  }

  def result2(in: Seq[Int]): Int = {
    val bruteForce =
      for (target <- in.reduce(Math.min) to in.reduce(Math.max))
        yield in
          .map(s => {
            val dist = math.abs(s - target)
            (dist * (dist + 1)) / 2
          })
          .sum

    bruteForce.reduce(Math.min)
  }

  object parsers {
    val pos: Parser[Int] = digit.rep.string.map(_.toInt)
    val comma: Parser[Unit] = Parser.char(',').void
    val positions: Parser[NonEmptyList[Int]] = pos.repSep(comma)
  }
}
