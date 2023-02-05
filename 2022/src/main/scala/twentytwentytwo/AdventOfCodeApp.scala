package twentytwentytwo

import cats.effect.{IO, IOApp}
import cats.instances.all._
import fs2.Stream
import org.http4s.Status
import pureconfig._
import pureconfig.generic.auto._

import scala.util.chaining.scalaUtilChainingOps

abstract class AdventOfCodeApp(year: Int, day: Int) extends IOApp.Simple {
  def solve(input: Stream[IO, String]): IO[String]
  def errorMessage(status: Status, body: Stream[IO, String]): IO[String] =
    for(msg <- body.compile.string) yield
      s"Unexpected response [${status.code}] ${status.sanitizedReason}: ${System.lineSeparator} $msg"
  private def getConfiguration(source: ConfigSource): IO[ApplicationConfiguration] =
    source
      .load[ApplicationConfiguration]
      .left
      .map(probs => new Exception(probs.prettyPrint(0)))
      .pipe(IO.fromEither)

  def run: IO[Unit] = for {
    conf <- getConfiguration(ConfigSource.default)
    client <- IO.pure(PuzzleInputClient[IO](conf.sessionToken))
    result <- client.fetchResource(year, day).use{
      case (Status.Ok, content) => solve(content)
      case (code, content) => errorMessage(code, content)
    }
    _ <- IO.println(result)
  } yield ()

}
