package twentytwentytwo

import cats.data.EitherT
import cats.effect.IO
import cats.effect.kernel.Resource
import fs2.text.{lines, utf8}
import org.http4s._
import org.http4s.dsl.io._
import org.http4s.ember.client._
import org.http4s.headers._

case class InputFetchError(message: String) extends AOCError

case class PuzzleInputClient(sessionToken: String) {
  private def bufferResponseAsLines(res: Response[MyEffect]): PuzzleInput =
    res.body.through(utf8.decode).through(lines)

  private def request(year: Int, day: Int): Request[MyEffect] = {
    Request[MyEffect](
      method = GET,
      uri = Uri.unsafeFromString(s"https://adventofcode.com/$year/day/$day/input"),
      httpVersion = HttpVersion.`HTTP/1.1`
    ).addCookie(name = "session", content = sessionToken)
  }

  def fetchResource(
      year: Int,
      day: Int
  ): Resource[MyEffect, PuzzleInput] =
    for {
      client <- EmberClientBuilder.default[MyEffect].build
      response <- client.run(request(year, day))
    } yield {
        bufferResponseAsLines(response)
        // ToDo: status code handling
    }
}
