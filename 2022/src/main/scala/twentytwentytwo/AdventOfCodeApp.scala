package twentytwentytwo

import cats.data._
import cats.effect.kernel.Resource
import cats.effect.{IO, IOApp}
import cats.implicits._
import pureconfig._
import pureconfig.generic.auto._

abstract class AdventOfCodeApp(year: Int, day: Int) extends IOApp.Simple {
  def solve(input: PuzzleInput): IO[Either[AOCError, String]]

  private def getConfiguration(
      source: ConfigSource
  ): IO[Either[AOCError, ApplicationConfiguration]] =
    IO(
      source
        .load[ApplicationConfiguration]
        .left
        .map(e =>
          new AOCError {
            val message = e.prettyPrint(0)
          }
        )
    )

  private def createClient(
      conf: ApplicationConfiguration
  ): EitherT[IO, AOCError, PuzzleInputClient] =
    EitherT.pure(PuzzleInputClient(conf.sessionToken))

  private def getInput(client: PuzzleInputClient): Resource[IO, Either[AOCError, PuzzleInput]] =
    client.fetchResource(year, day)

  def run: IO[Unit] = {
    val defaultConfiguration: EitherT[IO, AOCError, ApplicationConfiguration] = EitherT(
      getConfiguration(ConfigSource.default)
    )

    def solver(in: Either[AOCError, PuzzleInput]): IO[Either[AOCError, String]] =
      (for {
        input <- EitherT(IO.pure(in))
        solution <- EitherT(solve(input))
      } yield solution).value

    val solution: EitherT[IO, AOCError, String] = for {
      conf <- defaultConfiguration
      client <- createClient(conf)
      result <- EitherT(getInput(client).use(solver))
    } yield result

    solution.value.flatMap {
      case Right(solution) => IO.println(solution)
      case Left(error)     => IO.println(error.message)
    }
  }
}
