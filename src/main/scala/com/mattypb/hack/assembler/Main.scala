package com.mattypb.hack.assembler

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
      destinationFileName = args.head.replace(".asm", ".hack")
      _ <- assembler(args.head, destinationFileName).compile.drain
    } yield ExitCode.Success

  def validateArgs(args: List[String]): IO[Unit] =
    if (args.length != 1 || !args.head.endsWith(".asm") || args.head.length < 5)
      IO.raiseError(new IllegalArgumentException("The only argument should be a .asm file"))
    else IO.unit

  def assembler(originFileName: String, destinationFileName: String): Stream[IO, Unit] =
    Stream.resource(Blocker[IO]).flatMap { blocker =>
      io.file
        .readAll[IO](Paths.get(originFileName), blocker, 4096)
        .through(text.utf8Decode)
        .through(text.lines)
        .map(removeCommentsAndWhitespace)
        .filter(line => !line.isEmpty)
        .map(Parser.parseInstruction(_).toBinary.value)
        .intersperse("\n")
        .through(text.utf8Encode)
        .through(io.file.writeAll(Paths.get(destinationFileName), blocker))
    }

  def removeCommentsAndWhitespace(line: String): String =
    line.indexOf("//") match {
      case -1 => line.trim
      case i  => line.substring(0, i).trim
    }
}
