package com.mattypb.hack.assembler

object Parser {

  def parse(line: String): String = toInstruction(line).toString

  def toInstruction(line: String): Instruction =
    if (isAInstruction(line)) AInstruction(line)
    else CInstruction(line)

  private def isAInstruction(line: String): Boolean = line.startsWith("@")
}

case class Binary(value: String)

sealed trait Instruction {
  def toBinary: Binary
}

case class AInstruction(line: String) extends Instruction {
  val address: Int = line.substring(1).toInt

  override def toBinary: Binary = {
    val binaryString = s"000000000000000${address.toBinaryString}".takeRight(16)
    Binary(binaryString)
  }
}

case class CInstruction(line: String) extends Instruction {

  val dest: Option[String] = line.split("=").headOption
  val jump: Option[String] = line.split(";").lastOption

  val comp: String = dest match {
    case Some(_) => line.split("=").last
    case None    => line.split(";").head
  }

  override def toBinary: Binary = ???

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
      case "0" => ""
      case "1" => ""
      case "-1" => ""
      case "D" => ""
      case "A" => ""
      case "!D" => ""
      case "!A" => ""
      case "-D" => ""
      case "-A" => ""
      case "D+1" => ""
      case "A+1" => ""
      case "D-1" => ""
      case "A-1" => ""
      case "D+A" => ""
      case "D-A" => ""
      case "A-D" => ""
      case "D&A" => ""
      case "D|A" => ""
      case "0" => ""
      case "0" => ""
      case "0" => ""
      case "0" => ""
    }
}
