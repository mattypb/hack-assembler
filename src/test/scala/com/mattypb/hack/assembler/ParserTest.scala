package com.mattypb.hack.assembler

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class ParserTest extends AnyFunSuite with Matchers {

  test("correctly identifies A and C instructions") {
    Parser.parseInstruction("@2001") shouldBe AInstruction("@2001")
    Parser.parseInstruction("D=A") shouldBe CInstruction("D=A")
  }

  test("correctly converts A instructions to binary") {
    AInstruction("@0").toBinary shouldEqual Binary("0000000000000000")
    AInstruction("@2001").toBinary shouldEqual Binary("0000011111010001")
  }

  test("correctly converts C instructions to binary") {
    CInstruction("D=A").toBinary shouldEqual Binary("1110110000010000")
    CInstruction("M=M+1").toBinary shouldEqual Binary("1111110111001000")
    CInstruction("D;JGT").toBinary shouldEqual Binary("1110001100000001")
  }

}
