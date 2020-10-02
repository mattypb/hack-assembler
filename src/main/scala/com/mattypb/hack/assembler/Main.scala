package com.mattypb.hack.assembler

import java.nio.file.{Files, Paths}

import cats.effect.{Blocker, ExitCode, IO, IOApp}
import fs2.{Stream, io, text}

object Main extends IOApp {
  override def run(args: List[String]): IO[ExitCode] =
    converter.compile.drain.as(ExitCode.Success)

  val converter: Stream[IO, Unit] = Stream.resource(Blocker[IO]).flatMap { blocker =>

    def fahrenheitToCelsius(f: Double): Double =
      (f - 32.0) * (5.0/9.0)

    io.file.readAll[IO](Paths.get("fahrenheit.txt"), blocker, 4096)
      .through(text.utf8Decode)
      .through(text.lines)
      .filter(s => !s.trim.isEmpty && !s.startsWith("//"))
      .map(line => fahrenheitToCelsius(line.toDouble).toString)
      .intersperse("\n")
      .through(text.utf8Encode)
      .through(io.file.writeAll(Files.createFile(Paths.get("celsius.txt")), blocker))
  }
}
