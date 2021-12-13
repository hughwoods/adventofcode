package twentytwentyone

import cats.parse.Parser.Error
import cats.parse.{Parser, Parser0}

import scala.io.Source

object Day10 {

  val input = Source.fromResource("2021/input_day_10.txt").getLines().toSeq

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

  def scoreCompletion(com: String): Long = scoreCompletionAcc(com.toList, 0)
  def scoreCompletionAcc(com: Seq[Char], acc: Long): Long =
    com match {
      case Nil    => acc
      case h :: t => scoreCompletionAcc(t, (acc * 5) + scoreChar(h))
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
      case Some(chunk) => completeContinue(chunk.tail, chunk.closedBy + acc)
    }

  def main(args: Array[String]): Unit = {
    println(s"part1: ${result1(input)}")
    println(s"part2: ${result2(input)}")
  }

  sealed trait Chunk

  sealed trait OpenChunk {
    val head: Seq[Chunk]
    val tail: Option[OpenChunk]
    val closedBy: Char
  }

  case class ParenChunk(content: Seq[Chunk]) extends Chunk
  case class AngleChunk(content: Seq[Chunk]) extends Chunk
  case class SquareChunk(content: Seq[Chunk]) extends Chunk
  case class CurlyChunk(content: Seq[Chunk]) extends Chunk

  case class ParenOpen(head: Seq[Chunk], tail: Option[OpenChunk]) extends OpenChunk {
    val closedBy = ')'
  }
  case class AngleOpen(head: Seq[Chunk], tail: Option[OpenChunk]) extends OpenChunk {
    val closedBy = '>'
  }
  case class SquareOpen(head: Seq[Chunk], tail: Option[OpenChunk]) extends OpenChunk {
    val closedBy = ']'
  }
  case class CurlyOpen(head: Seq[Chunk], tail: Option[OpenChunk]) extends OpenChunk {
    val closedBy = '}'
  }

  object parsers {
    lazy val chunk: Parser[Chunk] =
      Parser.recursive[Chunk](recurse => {
        def chunk(s: Char, t: Char): Parser[List[Chunk]] =
          Parser.char(s) *> recurse.rep0 <* Parser.char(t)
        val angle = chunk('<', '>').map(AngleChunk)
        val square = chunk('[', ']').map(SquareChunk)
        val curly = chunk('{', '}').map(CurlyChunk)
        val paren = chunk('(', ')').map(ParenChunk)
        Parser.oneOf(angle :: square :: curly :: paren :: Nil)
      })

    lazy val openChunk: Parser[OpenChunk] =
      Parser.recursive[OpenChunk](recurse => {
        def open(s: Char): Parser[(List[Chunk], Option[OpenChunk])] =
          Parser.char(s) *> chunk.backtrack.rep0.soft ~ recurse.?
        val angle = open('<').map(AngleOpen.tupled)
        val square = open('[').map(SquareOpen.tupled)
        val curly = open('{').map(CurlyOpen.tupled)
        val paren = open('(').map(ParenOpen.tupled)
        Parser.oneOf(angle :: square :: curly :: paren :: Nil)
      })

    lazy val chunks: Parser0[(List[Chunk], Option[OpenChunk])] =
      (chunk.backtrack.rep0.soft ~ openChunk.?) <* Parser.end
  }
}
