package twentytwentyone

import cats.data.NonEmptyList
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

class Day6Tests extends AnyFunSpec with Matchers {
  describe("parser") {
    it("should parse simple input") {
      Day6.parsers.ages.parse("1,2,3") should be(Right("", NonEmptyList.of(1, 2, 3)))
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
