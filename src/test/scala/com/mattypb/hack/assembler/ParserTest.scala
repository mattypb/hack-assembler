package com.mattypb.hack.assembler

import cats.effect.IO
import cats.effect.concurrent.Ref
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class ParserTest extends AnyFunSuite with Matchers {

  test("identifies A instruction") {
    Ref[IO]
      .of(Map[String, Long]())
      .map { ref =>
        val index = 0
        Parser.parseInstruction("@2001", index, ref) shouldBe AInstruction("@2001", index, ref)
      }
      .unsafeRunSync()
  }

  test("identifies C instructions") {
    Ref[IO]
      .of(Map[String, Long]())
      .map { ref =>
        Parser.parseInstruction("D=A", 0, ref) shouldBe CInstruction("D=A")
      }
      .unsafeRunSync()
  }

  test("converts standard A instruction to binary") {
    {
      for {
        ref <- Ref[IO].of(Map[String, Long]())
        index = 0
        binary <- AInstruction("@2001", index, ref).toBinary
      } yield {
        binary.value shouldEqual "0000011111010001"
        ref.get.unsafeRunSync().size shouldEqual 0
      }
    }.unsafeRunSync()
  }

  test("converts A instruction to binary, with symbol not yet stored") {
    {
      for {
        ref <- Ref[IO].of(Map[String, Long]())
        index = 16
        binary <- AInstruction("@var", index, ref).toBinary
      } yield {
        binary.value shouldEqual "0000011111010001"
        ref.get.unsafeRunSync().size shouldEqual 1
      }
    }.unsafeRunSync()
  }

  test("converts A instruction to binary, with symbol already stored") {}

  test("converts C instructions to binary") {
    CInstruction("D=A").toBinary.unsafeRunSync() shouldEqual Binary("1110110000010000")
    CInstruction("M=M+1").toBinary.unsafeRunSync() shouldEqual Binary("1111110111001000")
    CInstruction("D;JGT").toBinary.unsafeRunSync() shouldEqual Binary("1110001100000001")
  }

}
