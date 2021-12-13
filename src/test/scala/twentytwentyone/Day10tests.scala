package twentytwentyone

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import twentytwentyone.Day10.{Chunk, partOne, partTwo}

class Day10Tests extends AnyFunSpec with Matchers {
  val testInput = Seq(
    "[({(<(())[]>[[{[]{<()<>>",
    "[(()[<>])]({[<{<<[]>>(",
    "{([(<{}[<>[]}>{[]{[(<()>",
    "(((({<>}<{<{<>}{[]{[]{}",
    "[[<[([]))<([[{}[[()]]]",
    "[{[{({}]{}}([{[{{{}}([]",
    "{<[[]]>}<{[{[{[]{()[[[]",
    "[<(<(<(<{}))><([]([]()",
    "<{([([[(<>()){}]>(<<{{",
    "<{([{{}}[<[[[<>{}]]]>[]]"
  )

  describe("chunk parser") {
    it("should parse simple input") {
      val result = Day10.parsers.chunk.parse("<>")
      result should be(Right("", Chunk('<', '>', Nil)))
    }

    it("should parse complex input") {
      Day10.parsers.chunk.parse("<<>([])>") should be(
        Right(
          "",
          Chunk('<', '>', List(Chunk('<', '>', Nil), Chunk('(', ')', List(Chunk('[', ']', Nil)))))
        )
      )
    }
  }
  describe("chunks parser") {

    it("should parse open input") {
      val res = Day10.parsers.chunks.parse("[](")

      info(res.toString)
      res.isInstanceOf[Right.type]

    }

    it("should get results for test input") {
      partOne(testInput) should be(26397)
    }

    it("should solve part 2 for test input") {
      partTwo(testInput) should be(288957)
    }
  }
}
