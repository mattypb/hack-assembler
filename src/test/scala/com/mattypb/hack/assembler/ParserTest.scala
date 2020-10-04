package com.mattypb.hack.assembler

import cats.effect.IO
import cats.effect.concurrent.Ref
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class ParserTest extends AnyFunSuite with Matchers {

  test("identifies A instruction") {
    {
      for {
        symbols <- Ref[IO].of(Map[String, Long]())
        lastUsedAddress <- Ref[IO].of(15.toLong)
        line = "@2001"
      } yield {
        Parser.parseInstruction(line, symbols, lastUsedAddress) shouldBe AInstruction(
          line,
          symbols,
          lastUsedAddress
        )
      }
    }.unsafeRunSync()
  }

  test("identifies C instructions") {
    {
      for {
        symbols <- Ref[IO].of(Map[String, Long]())
        lastUsedAddress <- Ref[IO].of(15.toLong)
        line = "D=A"
      } yield {
        Parser.parseInstruction(line, symbols, lastUsedAddress) shouldBe CInstruction(line)
      }
    }.unsafeRunSync()
  }

  test("converts standard A instruction to binary") {
    {
      for {
        symbols <- Ref[IO].of(Map[String, Long]())
        address = 15.toLong
        lastUsedAddress <- Ref[IO].of(address)
        binary <- AInstruction("@2001", symbols, lastUsedAddress).toBinary
      } yield {
        binary.value shouldEqual "0000011111010001" // 2001 in binary
        symbols.get.unsafeRunSync().size shouldEqual 0
        lastUsedAddress.get.unsafeRunSync() shouldEqual address
      }
    }.unsafeRunSync()
  }

  test("converts A instruction to binary, with symbol not yet stored") {
    {
      for {
        symbols <- Ref[IO].of(Map[String, Long]())
        address = 15.toLong
        lastUsedAddress <- Ref[IO].of(address)
        symbol = "var"
        binary <- AInstruction(s"@$symbol", symbols, lastUsedAddress).toBinary
      } yield {
        binary.value shouldEqual "0000000000010000" // 16 in binary
        symbols.get.unsafeRunSync().size shouldEqual 1
        symbols.get.unsafeRunSync()(symbol) shouldEqual address + 1
        lastUsedAddress.get.unsafeRunSync() shouldEqual address + 1
      }
    }.unsafeRunSync()
  }

  test("converts A instruction to binary, with symbol already stored") {
    {
      for {
        symbols <- Ref[IO].of(Map[String, Long]("R3" -> 3))
        address = 15.toLong
        lastUsedAddress <- Ref[IO].of(address)
        binary <- AInstruction("@R3", symbols, lastUsedAddress).toBinary
      } yield {
        binary.value shouldEqual "0000000000000011" // 3 in binary
        symbols.get.unsafeRunSync().size shouldEqual 1
        lastUsedAddress.get.unsafeRunSync() shouldEqual address
      }
    }.unsafeRunSync()
  }

  test("converts C instructions to binary") {
    CInstruction("D=A").toBinary.unsafeRunSync() shouldEqual Binary("1110110000010000")
    CInstruction("M=M+1").toBinary.unsafeRunSync() shouldEqual Binary("1111110111001000")
    CInstruction("D;JGT").toBinary.unsafeRunSync() shouldEqual Binary("1110001100000001")
  }

}
