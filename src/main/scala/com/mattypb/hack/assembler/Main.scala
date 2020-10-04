package com.mattypb.hack.assembler

import java.nio.file.Paths

import cats.effect.Blocker
import cats.effect.ExitCode
import cats.effect.IO
import cats.effect.IOApp
import cats.effect.concurrent.Ref
import fs2.Stream
import fs2.io
import fs2.text

object Main extends IOApp {
  override def run(args: List[String]): IO[ExitCode] =
    for {
      _ <- validateArgs(args)
      destinationFileName = args.head.replace(".asm", ".hack")
      labels <- firstPass(args.head)
      symbols <- Ref[IO].of(Symbols.predefined ++ labels)
      lastUsedAddress <- Ref[IO].of(15.toLong)
      _ <- secondPass(args.head, destinationFileName, symbols, lastUsedAddress)
    } yield ExitCode.Success

  def validateArgs(args: List[String]): IO[Unit] =
    if (args.length != 1 || !args.head.endsWith(".asm") || args.head.length < 5)
      IO.raiseError(new IllegalArgumentException("The only argument should be a .asm file"))
    else IO.unit

  // TODO: labels line numbers are wrong
  def firstPass(originFileName: String): IO[Map[String, Long]] =
    Stream
      .resource(Blocker[IO])
      .flatMap { blocker =>
        io.file
          .readAll[IO](Paths.get(originFileName), blocker, 4096)
          .through(text.utf8Decode)
          .through(text.lines)
          .map(removeCommentsAndWhitespace)
          .filter(line => !line.isEmpty)
          .zipWithIndex
          .filter { case (line, _) => line.startsWith("(") }
          .map { case (line, index) => (removeBrackets(line), index) }
      }
      .compile
      .to(Map)

  def secondPass(
    originFileName: String,
    destinationFileName: String,
    symbols: Ref[IO, Map[String, Long]],
    lastUsedAddress: Ref[IO, Long]
  ): IO[Unit] =
    Stream
      .resource(Blocker[IO])
      .flatMap { blocker =>
        io.file
          .readAll[IO](Paths.get(originFileName), blocker, 4096)
          .through(text.utf8Decode)
          .through(text.lines)
          .map(removeCommentsAndWhitespace)
          .filter(line => !line.isEmpty)
          .filter(line => !line.startsWith("("))
          .map(line => Parser.parseInstruction(line, symbols, lastUsedAddress))
          .map(_.toBinary.map(_.value).unsafeRunSync()) // how do I not do this unsafeRunSync?
          .intersperse("\n")
          .through(text.utf8Encode)
          .through(io.file.writeAll(Paths.get(destinationFileName), blocker))
      }
      .compile
      .drain

  def removeCommentsAndWhitespace(line: String): String =
    line.indexOf("//") match {
      case -1 => line.trim
      case i  => line.substring(0, i).trim
    }

  def removeBrackets(line: String): String = line.replaceAll("[()]", "")
}
