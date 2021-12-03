package twentytwentyone

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import twentytwentyone.Day3Tests.testInput
import twentytwentyone.Day3._

class Day3Tests extends AnyFunSpec with Matchers {
  describe("part one") {
    describe("gamma") {
      it("should match the sample result")(Part1.gamma(testInput) should be(22))
    }
    describe("epsilon") {
      it("should match the sample result")(Part1.epsilon(testInput) should be(9))
    }
  }
  describe("part two") {
    describe("oxygen generator") {
      it("should match the sample result")(Part2.o2(testInput) should be(23))
    }
    describe("co2 scrubber") {
      it("should match the sample result")(Part2.co2(testInput) should be(10))
    }
  }
}

object Day3Tests {
  val testInput = Seq(
    "00100",
    "11110",
    "10110",
    "10111",
    "10101",
    "01111",
    "00111",
    "11100",
    "10000",
    "11001",
    "00010",
    "01010"
  )
}
