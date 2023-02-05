package twentytwentyone

import cats.effect.{IO, IOApp}

import scala.io.Source

trait DayApp extends IOApp.Simple {
  type ErrorType = String
  val resourcePath: String
  val partOne: Seq[String] => Either[String, Long]
  val partTwo: Seq[String] => Either[String, Long]

  val run: IO[Unit] =
    for (
      lines <- input();
      _ <- printResults(lines)
    ) yield ()
  val printResults: Seq[String] => IO[Unit] = printer _ compose results

  def input(): IO[Seq[String]] =
    IO.blocking(Source.fromResource(resourcePath).getLines.toSeq)

  def results(in: Seq[String]): Either[ErrorType, (Long, Long)] = for (
    l1 <- partOne(in); l2 <- partTwo(in)
  ) yield (l1, l2)

  def printer(results: Either[ErrorType, (Long, Long)]): IO[Unit] =
    results match {
      case Left(err)       => IO { println(s"Failed: $err") }
      case Right((p1, p2)) => IO(println(s"Part one: $p1")) >> IO(println(s"Part two: $p2"))
    }
}
