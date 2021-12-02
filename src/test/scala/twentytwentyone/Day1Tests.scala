package twentytwentyone

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should._
import twentytwentyone.Day1.{countOfIncreasingReadings, increases}
import twentytwentyone.Day1Tests.sampleInput

class Day1Tests extends AnyFunSpec with Matchers {

  describe("Increases") {

    it("should return 7 for sample input") {
      increases(sampleInput) should be(7)
    }

    it("should return 8 for sample input with larger value appended") {
      increases(sampleInput :+ 1000) should be(8)
    }

    it("should return 7 for sample input with smaller value appended") {
      increases(sampleInput :+ 1) should be(7)
    }
  }

  describe("Windowed Increases") {
    it("should return 5 for sample input") {
      countOfIncreasingReadings(3)(sampleInput) should be(5)
    }
  }

}

object Day1Tests {

  val sampleInput = Seq(199, 200, 208, 210, 200, 207, 240, 269, 260, 263)

}
