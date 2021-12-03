package twentytwentyone

import scala.io.Source

object Day2 {
  import commanding.Direction._
  import commanding._

  lazy val commands: Seq[Command] = parseInput(input())

  def main(args: Array[String]): Unit = {
    println(Part1.result)
    println(Part2.result)
    println(TerseSolution.partOneResult)
    println(TerseSolution.partTwoResult)
  }

  def input(): Seq[String] =
    Source.fromResource("2021/input_day_2.txt").getLines.toSeq

  def parseInput(in: Seq[String]): Seq[Command] =
    in.map(_.split(' '))
      .map(pair => Command(dir = Direction.parse(pair(0)), magnitude = pair(1).toInt))

  object commanding {
    sealed trait Direction

    case class Command(dir: Direction, magnitude: Int)

    object Direction {
      def parse: String => Direction = {
        case "up"      => Up
        case "down"    => Down
        case "forward" => Forward
        case x         => throw new IllegalArgumentException(s"cannot parse $x")
      }

      case object Up extends Direction
      case object Down extends Direction
      case object Forward extends Direction
    }
  }

  object Part1 {
    lazy val startPosition = Position(0, 0)
    lazy val finalPosition: Position = commands.foldLeft(startPosition)(updatePosition)
    lazy val result: String = s"Part 1: ${finalPosition.horizontal * finalPosition.depth}"

    def updatePosition(pos: Position, com: Command) = com match {
      case Command(Up, n)      => pos.copy(depth = pos.depth - n)
      case Command(Down, n)    => pos.copy(depth = pos.depth + n)
      case Command(Forward, n) => pos.copy(horizontal = pos.horizontal + n)
    }

    case class Position(depth: Int, horizontal: Int)
  }

  object Part2 {
    lazy val startPosition = Position(0, 0, 0)
    lazy val finalPosition: Position = commands.foldLeft(startPosition)(updatePosition)
    lazy val result = s"Part 2: ${finalPosition.horizontal * finalPosition.depth}"

    def updatePosition(pos: Position, com: Command) = com match {
      case Command(Up, n)   => pos.copy(aim = pos.aim - n)
      case Command(Down, n) => pos.copy(aim = pos.aim + n)
      case Command(Forward, n) =>
        pos.copy(
          horizontal = pos.horizontal + n,
          depth = pos.depth + (n * pos.aim)
        )
    }

    case class Position(depth: Int, horizontal: Int, aim: Int)
  }

  object TerseSolution {
    val commands = input()
      .map(_.split(' ').toSeq)
      .map(s => (s(0)(0), s(1).toInt))
      .map {
        case ('f', n) => (n, 0)
        case ('d', n) => (0, n)
        case ('u', n) => (0, -n)
      }
    val partOnePosition = commands.reduce((a, b) => (a._1 + b._1, a._2 + b._2))
    val partOneResult = partOnePosition._1 * partOnePosition._2
    val partTwoPosition =
      commands.foldLeft(0, 0, 0)((a, b) => (a._1 + b._1, a._2 + (b._1 * a._3), a._3 + b._2))
    val partTwoResult = partTwoPosition._1 * partTwoPosition._2
  }
}
