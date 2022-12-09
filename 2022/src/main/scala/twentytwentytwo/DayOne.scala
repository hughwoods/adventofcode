package twentytwentytwo

import cats.effect.IO

object DayOne extends AdventOfCodeApp(2022, 1) {

  sealed trait Input

  object Input {
    final case object Empty extends Input

    final case class FoodItem(calories: Int) extends Input

    def parse(line: String): Input =
      if (line.isEmpty) Empty
      else FoodItem(calories = line.toInt)
  }

  object PartOne {
    case class Acc(best: Int, current: Int)

    object Acc {
      val zero = Acc(0, 0)
    }

    def accumulateElves(s: Acc, in: Input): Acc =
      in match {
        case Input.Empty => if (s.current > s.best) Acc(s.current, 0) else Acc(s.best, 0)
        case Input.FoodItem(calories) => Acc(s.best, s.current + calories)
      }
  }

  object PartTwo {
    case class Top3(_1: Int, _2: Int, _3: Int) {
      lazy val asList = List(_1, _2, _3).sorted

      lazy val total = _1 + _2 + _3

      def upsert(newValue: Int): Top3 = if (newValue > this.asList.head)
        Top3(newValue, this.asList(1), this.asList(2))
      else this
    }

    case class Acc(best: Top3, current: Int)

    object Acc {
      val zero = Acc(Top3(0, 0, 0), 0)
    }

    def accumulateElves(s: Acc, in: Input): Acc =
      in match {
        case Input.Empty => Acc(s.best.upsert(s.current), 0)
        case Input.FoodItem(calories) => Acc(s.best, s.current + calories)
      }
  }

  val zero: (PartOne.Acc, PartTwo.Acc) = (PartOne.Acc.zero, PartTwo.Acc.zero)

  def accumulate(acc: (PartOne.Acc, PartTwo.Acc), in: Input) =
    (PartOne.accumulateElves(acc._1, in), PartTwo.accumulateElves(acc._2, in))

  def solve(input: PuzzleInput): IO[Either[AOCError, String]] =
    input
      .map(Input.parse)
      .fold(zero)(accumulate)
      .map{
        case (p1, p2) => s"Part One: ${p1.best} \r\nPart Two: ${p2.best.total}"
      }.compile.string.map(Right[AOCError, String])
}
