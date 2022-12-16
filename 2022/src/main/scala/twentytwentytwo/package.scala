import cats.data.EitherT
import cats.effect.IO

package object twentytwentytwo {
  type PuzzleInput = fs2.Stream[MyEffect, String]
  type MyEffect[A] = EitherT[IO, AOCError, A]
}
