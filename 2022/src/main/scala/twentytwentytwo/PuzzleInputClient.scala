package twentytwentytwo

import cats.effect.IO
import cats.effect.kernel.Resource
import fs2.text.{lines, utf8}
import org.http4s._
import org.http4s.client.dsl.io._
import org.http4s.dsl.io._
import org.http4s.ember.client._
import org.http4s.headers._

case class InputFetchError(message: String) extends AOCError

case class PuzzleInputClient(sessionToken: String) {
  private val sessionCookie = Cookie(RequestCookie(name = "session", content = sessionToken))

  private def bufferResponseAsLines(res: Response[IO]): PuzzleInput =
    res.body.through(utf8.decode).through(lines)

  private def request(year: Int, day: Int) =
    GET(
      Uri.unsafeFromString(s"https://adventofcode.com/$year/day/$day/input"),
      sessionCookie
    )

  def fetchResource(
      year: Int,
      day: Int
  ): Resource[IO, Either[AOCError, PuzzleInput]] =
    for {
      client <- EmberClientBuilder.default[IO].build
      response <- client.run(request(year, day))
    } yield
      if (response.status.isSuccess)
        Right(bufferResponseAsLines(response))
      else
        Left(InputFetchError(s"Request failed with status ${response.status.code}"))
}
