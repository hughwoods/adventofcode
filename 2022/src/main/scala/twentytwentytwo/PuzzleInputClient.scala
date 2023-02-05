package twentytwentytwo

import cats.effect.Async
import cats.effect.kernel.Resource
import fs2.Stream
import fs2.io.net.Network
import fs2.text.{lines, utf8}
import org.http4s._
import org.http4s.dsl.io._
import org.http4s.ember.client._
import cats.implicits.{catsSyntaxEitherId, catsSyntaxMonadError}

case class PuzzleInputClient[F[_]](sessionToken: String)(implicit
    network: Network[F],
    F: Async[F]
) {
  private def request(year: Int, day: Int): Request[F] =
    Request[F](
      method = GET,
      uri = Uri.unsafeFromString(s"https://adventofcode.com/$year/day/$day/input"),
      httpVersion = HttpVersion.`HTTP/1.1`
    ).addCookie(name = "session", content = sessionToken)

  def fetchResource(year: Int, day: Int): Resource[F, (Status, Stream[F, String])] =
    for {
      client <- EmberClientBuilder.default[F].build
      response <- client.run(request(year, day))
    } yield (response.status, response
        .body
        .through(utf8.decode)
        .through(lines))
}
