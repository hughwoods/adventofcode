package twentytwentyone

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import twentytwentyone.Day2._
import twentytwentyone.Day2.commanding.Direction._
import twentytwentyone.Day2.commanding._
import twentytwentyone.Day2Tests.sampleInput

class Day2Tests extends AnyFunSpec with Matchers {
  describe("commanding") {
    describe("Direction") {
      describe("parse") {
        it("should parse up")(Direction.parse("up") should be(Up))
        it("should parse down")(Direction.parse("down") should be(Down))
        it("should parse forward")(Direction.parse("forward") should be(Forward))
      }
    }
  }

  describe("part one") {
    import twentytwentyone.Day2.Part1._
    describe("update position") {
      it("should move up")(
        updatePosition(Position(0, 0), Command(Up, 1)) should be(Position(-1, 0))
      )
      it("should move down")(
        updatePosition(Position(0, 0), Command(Down, 1)) should be(Position(1, 0))
      )
      it("should move foward")(
        updatePosition(Position(0, 0), Command(Forward, 1)) should be(Position(0, 1))
      )
      it("should calculate final position from sample data") {
        val commands = parseInput(sampleInput)
        val result = commands.foldLeft(startPosition)(updatePosition)
        result should be(Position(depth = 10, horizontal = 15))
      }
    }
  }

  describe("part two") {
    import twentytwentyone.Day2.Part2._
    describe("update position") {
      it("should move up")(
        updatePosition(Position(0, 0, 0), Command(Up, 1)) should be(Position(0, 0, -1))
      )
      it("should move down")(
        updatePosition(Position(0, 0, 0), Command(Down, 1)) should be(Position(0, 0, 1))
      )
      it("should move foward")(
        updatePosition(Position(0, 0, 2), Command(Forward, 10)) should be(Position(20, 10, 2))
      )
      it("should calculate final position from sample data") {
        val commands = parseInput(sampleInput)
        val result = commands.foldLeft(startPosition)(updatePosition)
        result should be(Position(depth = 60, horizontal = 15, aim = 10))
      }
    }
  }
}

object Day2Tests {
  val sampleInput = Seq(
    "forward 5",
    "down 5",
    "forward 8",
    "up 3",
    "down 8",
    "forward 2"
  )
}
