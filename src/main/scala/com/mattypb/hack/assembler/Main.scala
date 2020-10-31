package com.mattypb.hack.assembler

import java.nio.file.Paths

import cats.effect.Blocker
import cats.effect.ExitCode
import cats.effect.IO
import cats.effect.IOApp
import cats.effect.concurrent.Ref
import com.mattypb.hack.assembler.Parser.toInstruction
import fs2.io.file.writeAll
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
          .zipWithIndex
          .map { case ((line, index), labelCount) => (removeBrackets(line), index - labelCount) }
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
          .map(line => toInstruction(line, symbols, lastUsedAddress))
          .evalMap(_.toBinary.map(_.value))
          .intersperse("\n")
          .through(text.utf8Encode)
          .through(writeAll(Paths.get(destinationFileName), blocker))
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
