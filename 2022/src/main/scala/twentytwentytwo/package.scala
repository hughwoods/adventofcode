import cats.effect.IO

package object twentytwentytwo {
  type PuzzleInput = fs2.Stream[IO, String]
}
