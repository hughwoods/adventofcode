package twentytwentyone

import cats.data.NonEmptyList
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

class Day7Tests extends AnyFunSpec with Matchers {
  describe("parser") {
    it("should parse simple input") {
      Day7.parsers.positions.parse("1057,542,226,868,89,160,1267,346,209") should be(
        Right("", NonEmptyList.of(1057, 542, 226, 868, 89, 160, 1267, 346, 209))
      )
    }
  }

  describe("simulation result") {
    for (testCase <- Map(18 -> 26L, 80 -> 5934L, 256 -> 26984457539L)) yield {
      it(s"should simulate ${testCase._1} cycles on the sample ") {
        Day6.result(Seq(3, 4, 3, 1, 2), testCase._1) should be(testCase._2)
      }
    }
  }
}
