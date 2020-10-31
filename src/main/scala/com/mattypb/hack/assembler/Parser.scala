package com.mattypb.hack.assembler

import cats.effect.IO
import cats.effect.concurrent.Ref

object Parser {

  def toInstruction(
    line: String,
    symbols: Ref[IO, Map[String, Long]],
    lastUsedAddress: Ref[IO, Long]
  ): Instruction =
    if (isAInstruction(line)) AInstruction(line, symbols, lastUsedAddress)
    else CInstruction(line)

  private def isAInstruction(line: String): Boolean = line.startsWith("@")
}

case class Binary(value: String)

sealed trait Instruction {
  def toBinary: IO[Binary]
}

case class AInstruction(
  line: String,
  symbols: Ref[IO, Map[String, Long]],
  lastUsedAddress: Ref[IO, Long]
) extends Instruction {
  def address: IO[Long] = {
    val text = line.substring(1)

    text.toLongOption match {
      case Some(value: Long) => IO(value)
      case None              => getOrStoreSymbol(text)
    }
  }

  private def getOrStoreSymbol(text: String): IO[Long] =
    symbols.get.flatMap {
      _.get(text) match {
        case Some(v: Long) => IO(v)
        case None =>
          for {
            a <- lastUsedAddress.updateAndGet(_ + 1)
            _ <- symbols.update(_ ++ Map(text -> a))
          } yield a
      }
    }

  override def toBinary: IO[Binary] =
    address.map { a =>
      val binaryString = s"000000000000000${a.toBinaryString}".takeRight(16)
      Binary(binaryString)
    }

}

case class CInstruction(line: String) extends Instruction {

  val dest: Option[String] =
    if (line.contains("=")) line.split("=").headOption
    else None

  val jump: Option[String] =
    if (line.contains(";")) line.split(";").lastOption
    else None

  val comp: String = dest match {
    case Some(_) => line.split("=").last
    case None    => line.split(";").head
  }

  override def toBinary: IO[Binary] = IO(Binary(s"111$compBinary$destBinary$jumpBinary"))

  private def destBinary: String =
    dest match {
      case Some(value) =>
        value match {
          case "M"   => "001"
          case "D"   => "010"
          case "MD"  => "011"
          case "A"   => "100"
          case "AM"  => "101"
          case "AD"  => "110"
          case "AMD" => "111"
        }
      case None => "000"
    }

  private def jumpBinary: String =
    jump match {
      case Some(value) =>
        value match {
          case "JGT" => "001"
          case "JEQ" => "010"
          case "JGE" => "011"
          case "JLT" => "100"
          case "JNE" => "101"
          case "JLE" => "110"
          case "JMP" => "111"
        }
      case None => "000"
    }

  private def compBinary: String =
    comp match {
      case "0"   => "0101010"
      case "1"   => "0111111"
      case "-1"  => "0111010"
      case "D"   => "0001100"
      case "A"   => "0110000"
      case "!D"  => "0001101"
      case "!A"  => "0110001"
      case "-D"  => "0001111"
      case "-A"  => "0110011"
      case "D+1" => "0011111"
      case "A+1" => "0110111"
      case "D-1" => "0001110"
      case "A-1" => "0110010"
      case "D+A" => "0000010"
      case "D-A" => "0010011"
      case "A-D" => "0000111"
      case "D&A" => "0000000"
      case "D|A" => "0010101"
      case "M"   => "1110000"
      case "!M"  => "1110001"
      case "-M"  => "1110011"
      case "M+1" => "1110111"
      case "M-1" => "1110010"
      case "D+M" => "1000010"
      case "D-M" => "1010011"
      case "M-D" => "1000111"
      case "D&M" => "1000000"
      case "D|M" => "1010101"
    }
}
