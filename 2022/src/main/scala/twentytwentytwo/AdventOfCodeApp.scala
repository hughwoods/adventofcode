package twentytwentytwo

import cats.data._
import cats.effect.kernel.Resource
import cats.effect.{IO, IOApp}
import cats.implicits._
import pureconfig._
import pureconfig.generic.auto._

import scala.util.chaining.scalaUtilChainingOps

abstract class AdventOfCodeApp(year: Int, day: Int) extends IOApp.Simple {
  def solve(input: PuzzleInput): MyEffect[String]

  private def getConfiguration(
      source: ConfigSource
  ): MyEffect[ApplicationConfiguration] = {
    val eith: Either[AOCError, ApplicationConfiguration] =
      source
        .load[ApplicationConfiguration]
        .left.map(e =>
          new AOCError {
            val message = e.prettyPrint(0)
          }
        )

    EitherT[IO, AOCError, ApplicationConfiguration](IO.pure(eith))
  }

  private def createClient(
      conf: ApplicationConfiguration
  ): MyEffect[PuzzleInputClient] =
    EitherT.pure(PuzzleInputClient(conf.sessionToken))

  private def getInput(client: PuzzleInputClient): Resource[MyEffect, PuzzleInput] =
    client.fetchResource(year, day)

  def run: IO[Unit] = {
    val defaultConfiguration: MyEffect[ApplicationConfiguration] =
      getConfiguration(ConfigSource.default)

    val solution: MyEffect[String] = for {
      conf <- defaultConfiguration
      client <- createClient(conf)
      result <- getInput(client).use(solve)
    } yield result

    solution.value.flatMap {
      case Right(solution) => IO.println(solution)
      case Left(error)     => IO.println(error.message)
    }
  }
}
