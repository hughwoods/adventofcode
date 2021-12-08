package twentytwentyone

import cats.data.{NonEmptyList => NEL}
import cats.effect.{IO, IOApp}
import cats.parse.Parser
import cats.parse.Rfc5234.{alpha, sp}
import twentytwentyone.Day8.parsers.inputLine

import scala.io.Source
import scala.language.postfixOps

object Day8 extends IOApp.Simple {

  val parseLines = (lines: Seq[String]) =>
    lines.map(inputLine.parse).partitionMap(identity) match {
      case (Nil, rights) => Right(rights)
      case (lefts, _)    => Left(lefts)
    }

  override def run: IO[Unit] = for (
    str <- input();
    _ <- printResult(str)
  ) yield ()

  def input(): IO[Seq[String]] =
    IO.blocking(Source.fromResource("2021/input_day_8.txt").getLines().toSeq)

  def printResult(strs: Seq[String]): IO[Unit] =
    parseLines(strs) match {
      case Left(errs) => IO { println(s"Failed: ${errs(0)}") }
      case Right(lines) =>
        IO { println(result1(lines.map(_._2))) } >>
          IO { println(result2(lines.map(_._2))) }
    }

  def result1(in: Seq[InputLine]): Int =
    in.map(_.message.map(_.size).filter(Set(2, 3, 4, 7)).length).sum

  def result2(in: Seq[InputLine]): Int =
    in.map(intForLine).sum

  def intForLine(line: InputLine) =
    Integer.parseInt(
      (for (digit <- line.message.toList)
        yield {
          val sc = simpleChars(line.training)
          digit match {
            case s if s.size == 2                       => '1'
            case s if s.size == 3                       => '7'
            case s if s.size == 4                       => '4'
            case s if s.size == 7                       => '8'
            case s if (sc('7') union sc('4')).forall(s) => '9'
            case s
                if (sc('7') union s) == s
                  && (sc('4') union s) == sc('8') =>
              '0'
            case s
                if s.size == 6
                  && (s.removedAll(sc('4')).size == 3) =>
              '6'
            case s
                if s.size == 5
                  && (s.removedAll(sc('4')).size == 3) =>
              '2'
            case s
                if s.size == 5
                  && (s.removedAll(sc('1')).size == 3) =>
              '3'
            case _ => '5'
          }
        }).mkString
    )

  def simpleChars(training: NEL[Set[Char]]): Map[Char, Set[Char]] = training.collect {
    case s if s.size == 2 => ('1' -> s)
    case s if s.size == 3 => ('7' -> s)
    case s if s.size == 4 => ('4' -> s)
    case s if s.size == 7 => ('8' -> s)
  }.toMap

  case class InputLine(training: NEL[Set[Char]], message: NEL[Set[Char]])

  object parsers {
    val pipe: Parser[Unit] = Parser.char('|').void
    val segment: Parser[Char] = alpha
    val segments: Parser[Set[Char]] = alpha.rep.map(_.iterator.toSet)
    val positions: Parser[NEL[Set[Char]]] = segments.repSep(sp)
    val inputLine: Parser[InputLine] =
      ((positions <* pipe.surroundedBy(sp)) ~ positions).map(InputLine.tupled)
  }
}
