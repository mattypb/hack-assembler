package com.mattypb.hack.assembler

import java.nio.file.Files
import java.nio.file.Paths

import cats.effect.Blocker
import cats.effect.ExitCode
import cats.effect.IO
import cats.effect.IOApp
import fs2.Stream
import fs2.io
import fs2.text

object Main extends IOApp {
  override def run(args: List[String]): IO[ExitCode] =
    for {
      _ <- validateArgs(args)
      fileName = args.head.substring(0, args.head.length - 4)
      _ <- assembler(fileName).compile.drain
    } yield ExitCode.Success

  def validateArgs(args: List[String]): IO[Unit] =
    if (args.length != 1 || !args.head.endsWith(".asm"))
      IO.raiseError(new IllegalArgumentException("The only argument should be a .asm file"))
    else IO.unit

  def assembler(fileName: String): Stream[IO, Unit] =
    Stream.resource(Blocker[IO]).flatMap { blocker =>
      io.file
        .readAll[IO](Paths.get(s"$fileName.asm"), blocker, 4096)
        .through(text.utf8Decode)
        .through(text.lines)
        .filter(s => !s.trim.isEmpty && !s.trim.startsWith("//"))
        .map(Parser.parse)
        .intersperse("\n")
        .through(text.utf8Encode)
        .through(io.file.writeAll(Files.createFile(Paths.get(s"$fileName.hack")), blocker))
    }
}
