package twentytwentyone

import cats.parse.Parser.Error
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import twentytwentyone.Day10.{
  AngleChunk,
  ParenChunk,
  SquareChunk,
  complete,
  result2,
  scoreCompletion
}

class Day10Tests extends AnyFunSpec with Matchers {
  describe("chunk parser") {
    it("should parse simple input") {
      val result = Day10.parsers.chunk.parse("<>")
      result should be(Right("", AngleChunk(Nil)))
    }

    it("should parse complex input") {
      Day10.parsers.chunk.parse("<<>([])>") should be(
        Right("", AngleChunk(List(AngleChunk(Nil), ParenChunk(List(SquareChunk(Nil))))))
      )
    }

    it("should parse open input") {
      val res = Day10.parsers.chunks.parse("[](")

      info(res.toString)
      res.isInstanceOf[Right.type]

    }

    it("should solve part 2 for test input") {
      val r1 = Seq(
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
      result2(r1) should be(288957)
    }

    it("should get results for test input") {
      Seq(
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
      ).map(s => (s, Day10.parsers.chunks.parse(s)))
        .collect { case (s, Left(Error(offset, _))) =>
          s(offset)
        }
        .map {
          case ')' => 3
          case ']' => 57
          case '}' => 1197
          case '>' => 25137
        }
        .sum should be(26397)
    }

  }

}
