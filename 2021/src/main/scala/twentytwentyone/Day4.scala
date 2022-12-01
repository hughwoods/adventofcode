package twentytwentyone

import scala.annotation.tailrec
import scala.io.Source

object Day4 {
  val numbers = input().head.split(',').map(Integer.parseInt)
  val cards: Seq[Card] =
    input().tail
      .grouped(6)
      .map(_.tail)
      .toSeq
      .map(_.map(s => s.grouped(3).toSeq.map(x => Some(Integer.parseInt(x.trim)))))
      .map(Card)

  def resultCalc(c: Card, n: Int) =
    (for (row <- c.content; cell <- row; value <- cell) yield value).sum * n

  def main(args: Array[String]): Unit = {
    println(s"part1: ${Part1.result}")
    println(s"part2: ${Part2.result}")
  }

  def input(): Seq[String] = Source.fromResource("2021/input_day_4.txt").getLines.toSeq

  case class Card(content: Seq[Seq[Option[Int]]]) {
    lazy val transposed = content.transpose
    def row: Int => Seq[Option[Int]] = content.apply
    def column: Int => Seq[Option[Int]] = transposed.apply
    def mark(x: Int): Card = Card(content.map(_.map { case Some(`x`) => None; case d => d }))
    def winning: Boolean =
      content.map(_.toSet).contains(Set(None)) || transposed.map(_.toSet).contains(Set(None))
  }

  object Part1 {
    val (winningCard, lastCalled) = callForWinner(numbers, cards)
    val result = resultCalc(winningCard, lastCalled)

    @tailrec
    def callForWinner(nums: Seq[Int], cards: Seq[Card]): (Card, Int) = {
      val markedCards = cards.map(_.mark(nums.head))
      markedCards.find(_.winning) match {
        case Some(winner) => (winner, nums.head)
        case None         => callForWinner(nums.tail, markedCards)
      }
    }
  }

  object Part2 {
    val (winningCard, lastCalled) = callForLastWinner(numbers, cards)
    val result = resultCalc(winningCard, lastCalled)

    @tailrec
    def callForLastWinner(nums: Seq[Int], cards: Seq[Card]): (Card, Int) = {
      val markedCards = cards.map(_.mark(nums.head)).filter(!_.winning)
      markedCards match {
        case Nil => (cards.head.mark(nums.head), nums.head)
        case _   => callForLastWinner(nums.tail, markedCards)
      }
    }
  }
}
