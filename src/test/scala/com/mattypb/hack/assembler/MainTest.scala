package com.mattypb.hack.assembler

import java.nio.file.Files
import java.nio.file.Paths

import cats.effect.IO
import cats.effect.concurrent.Ref
import com.mattypb.hack.assembler.Main._
import org.scalatest.Assertion
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

import scala.io.Source

class MainTest extends AnyFunSuite with Matchers {

  private val testData: String = "src/test/resources/testdata"
  private val generated: String = s"$testData/generated"

  test("validates command line arguments") {
    intercept[IllegalArgumentException](validateArgs(List()).unsafeRunSync())
    intercept[IllegalArgumentException](validateArgs(List("")).unsafeRunSync())
    intercept[IllegalArgumentException](validateArgs(List("a1")).unsafeRunSync())
    intercept[IllegalArgumentException](validateArgs(List("b2.")).unsafeRunSync())
    intercept[IllegalArgumentException](validateArgs(List("3c.btn")).unsafeRunSync())
    intercept[IllegalArgumentException](validateArgs(List(".asm")).unsafeRunSync())
    intercept[IllegalArgumentException](validateArgs(List("a1.asm", "b2.asm")).unsafeRunSync())
    validateArgs(List("works.asm")).unsafeRunSync() shouldEqual ()
  }

  test("removes comments") {
    removeCommentsAndWhitespace("") shouldEqual ""
    removeCommentsAndWhitespace(" ") shouldEqual ""
    removeCommentsAndWhitespace(" a ") shouldEqual "a"
    removeCommentsAndWhitespace("string") shouldEqual "string"
    removeCommentsAndWhitespace("//") shouldEqual ""
    removeCommentsAndWhitespace("// asd") shouldEqual ""
    removeCommentsAndWhitespace("  //  ") shouldEqual ""
    removeCommentsAndWhitespace("  // asd  ") shouldEqual ""
    removeCommentsAndWhitespace("  @200 // asd  ") shouldEqual "@200"
  }

  test("removes brackets from labels") {
    removeBrackets("()") shouldEqual ""
    removeBrackets("(LOOP)") shouldEqual "LOOP"
  }

  test("first pass generates correct map") {
    val file = s"$testData/Max.asm"
    val expected: Map[String, Long] = Map(
      "OUTPUT_FIRST" -> 10,
      "OUTPUT_D" -> 12,
      "INFINITE_LOOP" -> 14
    )
    val actual: Map[String, Long] = firstPass(file).unsafeRunSync()

    actual shouldEqual expected
  }

  test("second pass assembles Add.asm") {
    val file = "Add"
    assembleAndCompare(file)
  }

  test("second pass assembles MaxL.asm") {
    val file = "MaxL"
    assembleAndCompare(file)
  }

  test("second pass assembles RectL.asm") {
    val file = "RectL"
    assembleAndCompare(file)
  }

  test("second pass assembles PongL.asm") {
    val file = "PongL"
    assembleAndCompare(file)
  }

  test("assembles Max.asm") {
    val file = "Max"
    assembleAndCompare(file, firstPass)
  }

  test("assembles Rect.asm") {
    val file = "Rect"
    assembleAndCompare(file, firstPass)
  }

  test("assembles Pong.asm") {
    val file = "Pong"
    assembleAndCompare(file, firstPass)
  }

  private def assembleAndCompare(
    file: String,
    firstPass: String => IO[Map[String, Long]] = str => IO(Map[String, Long]())
  ): Unit = {
    val origin = s"$testData/$file.asm"
    val destination = s"$generated/$file.hack"
    val expectedFile = s"$testData/$file.hack"

    {
      for {
        _ <- IO(Files.createDirectories(Paths.get(generated)))
        labels <- firstPass(origin)
        symbols <- Ref[IO].of(Symbols.predefined ++ labels)
        lastUsedAddress <- Ref[IO].of(15.toLong)
        _ <- secondPass(origin, destination, symbols, lastUsedAddress)
      } yield compareHackFiles(expectedFile, destination)
    }.unsafeRunSync()
  }

  private def compareHackFiles(expectedFileName: String, actualFileName: String): Assertion = {
    val expectedFile = Source.fromURI(Paths.get(expectedFileName).toUri)
    val expected = expectedFile.getLines.mkString("\n")

    val actualFile = Source.fromURI(Paths.get(actualFileName).toUri)
    val actual = actualFile.getLines.mkString("\n")

    expectedFile.close()
    actualFile.close()

    actual shouldEqual expected
  }

}
