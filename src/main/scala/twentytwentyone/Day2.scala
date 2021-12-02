package twentytwentyone

import scala.io.Source

object Day2 {
  import commanding.Direction._
  import commanding._

  lazy val commands: Seq[Command] = parseInput(input())

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
    val startPosition = Position(0, 0)

    def main(args: Array[String]): Unit = {
      val finalPosition: Position = commands.foldLeft(startPosition)(updatePosition)
      println(s"Part 1: ${finalPosition.horizontal * finalPosition.depth}")
    }

    def updatePosition(pos: Position, com: Command) = com match {
      case Command(Up, n)      => pos.copy(depth = pos.depth - n)
      case Command(Down, n)    => pos.copy(depth = pos.depth + n)
      case Command(Forward, n) => pos.copy(horizontal = pos.horizontal + n)
    }

    case class Position(depth: Int, horizontal: Int)
  }

  object Part2 {
    val startPosition = Position(0, 0, 0)

    def main(args: Array[String]): Unit = {
      val finalPosition: Position = commands.foldLeft(startPosition)(updatePosition)
      println(s"Part 2: ${finalPosition.horizontal * finalPosition.depth}")
    }

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
}
