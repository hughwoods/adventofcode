package twentytwentyone

import cats.parse.Parser.Error
import cats.parse.{Parser, Parser0}

import scala.Function.tupled
import scala.annotation.tailrec
import scala.io.Source

object Day10 {

  val input = Source.fromResource("2021/input_day_10.txt").getLines().toSeq
  val chunkDefinitions: List[(Char, Char)] = List(('<', '>'), ('[', ']'), ('{', '}'), ('(', ')'))

  def main(args: Array[String]): Unit = {
    println(s"part1: ${result1(input)}")
    println(s"part2: ${result2(input)}")
  }

  def result1(input: Seq[String]): Long = input
    .map(s => (s, Day10.parsers.chunks.parse(s)))
    .collect { case (s, Left(Error(offset, _))) => s(offset) }
    .map(autoCompleteScore)
    .sum

  def autoCompleteScore = Map(')' -> 3, ']' -> 57, '}' -> 1197, '>' -> 25137)

  def result2(input: Seq[String]): Long =
    midpointValue(
      input
        .map(Day10.parsers.chunks.parse)
        .collect { case Right(("", chunks)) if chunks._2.isDefined => chunks._2 }
        .map(complete)
        .map(scoreCompletion)
    )

  def midpointValue[A](l: Seq[A])(implicit ord: Ordering[A]): A = l.sorted.apply(l.length / 2)

  def complete(oc: Option[OpenChunk]) = {
    @tailrec
    def completeAcc(oc: Option[OpenChunk], acc: String): String =
      oc match {
        case None        => acc
        case Some(chunk) => completeAcc(chunk.tail, chunk.t + acc)
      }
    completeAcc(oc, "")
  }

  def scoreCompletion(com: String): Long = {
    @tailrec
    def scoreCompletionAcc(com: Seq[Char], acc: Long): Long =
      com match {
        case Nil     => acc
        case c :: cs => scoreCompletionAcc(cs, (acc * 5) + charScore(c))
      }
    scoreCompletionAcc(com.toList, 0)
  }

  def charScore = Map(')' -> 1, ']' -> 2, '}' -> 3, '>' -> 4)

  case class Chunk(s: Char, t: Char, content: Seq[Chunk])

  case class OpenChunk(s: Char, t: Char, head: Seq[Chunk], tail: Option[OpenChunk])
  object OpenChunk {
    def apply(s: Char, t: Char)(tuple: (Seq[Chunk], Option[OpenChunk])): OpenChunk =
      OpenChunk(s, t, tuple._1, tuple._2)
  }

  object parsers {
    lazy val chunk: Parser[Chunk] =
      Parser.recursive[Chunk](recurse => {
        def chunkGen(s: Char, t: Char): Parser[Chunk] =
          (Parser.char(s) *> recurse.rep0 <* Parser.char(t)).map(Chunk(s, t, _))
        Parser.oneOf(chunkDefinitions.map(tupled(chunkGen)))
      })

    lazy val openChunk: Parser[OpenChunk] =
      Parser.recursive[OpenChunk](recurse => {
        def openGen(s: Char, t: Char): Parser[OpenChunk] =
          (Parser.char(s) *> chunk.backtrack.rep0.soft ~ recurse.?).map(OpenChunk(s, t))
        Parser.oneOf(chunkDefinitions.map(tupled(openGen)))
      })

    lazy val chunks: Parser0[(List[Chunk], Option[OpenChunk])] =
      (chunk.backtrack.rep0.soft ~ openChunk.?) <* Parser.end
  }
}
