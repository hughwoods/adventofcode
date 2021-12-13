package twentytwentyone

import cats.parse.Parser.Error
import cats.parse.{Parser, Parser0}

import scala.Function.tupled
import scala.io.Source

object Day10 {

  val input = Source.fromResource("2021/input_day_10.txt").getLines().toSeq
  val chunkDefinitions: List[(Char, Char)] = List(('<', '>'), ('[', ']'), ('{', '}'), ('(', ')'))

  def result1(input: Seq[String]): Long = input
    .map(s => (s, Day10.parsers.chunks.parse(s)))
    .collect { case (s, Left(Error(offset, _))) => s(offset) }
    .map {
      case ')' => 3
      case ']' => 57
      case '}' => 1197
      case '>' => 25137
    }
    .sum

  def result2(input: Seq[String]): Long = {
    val completionScores = input
      .map(Day10.parsers.chunks.parse)
      .collect { case Right(("", chunks)) => chunks._2 }
      .filter(_.isDefined)
      .map(complete)
      .map(scoreCompletion)

    val index = completionScores.length / 2
    completionScores.sorted.apply(index)
  }

  def scoreCompletion(com: String): Long = {
    def scoreCompletionAcc(com: Seq[Char], acc: Long): Long =
      com match {
        case Nil    => acc
        case h :: t => scoreCompletionAcc(t, (acc * 5) + scoreChar(h))
      }
    scoreCompletionAcc(com.toList, 0)
  }

  def scoreChar(c: Char): Int = c match {
    case ')' => 1
    case ']' => 2
    case '}' => 3
    case '>' => 4
  }

  def complete(oc: Option[OpenChunk]) = completeContinue(oc, "")

  def completeContinue(oc: Option[OpenChunk], acc: String): String =
    oc match {
      case None        => acc
      case Some(chunk) => completeContinue(chunk.tail, chunk.t + acc)
    }

  def main(args: Array[String]): Unit = {
    println(s"part1: ${result1(input)}")
    println(s"part2: ${result2(input)}")
  }

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
