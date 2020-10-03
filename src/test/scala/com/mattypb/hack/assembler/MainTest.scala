package com.mattypb.hack.assembler

import org.scalatest.Assertion
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

import scala.io.Source

class MainTest extends AnyFunSuite with Matchers {

  test("correctly validates command line arguments") {
    intercept[IllegalArgumentException](Main.validateArgs(List()).unsafeRunSync())
    intercept[IllegalArgumentException](Main.validateArgs(List("")).unsafeRunSync())
    intercept[IllegalArgumentException](Main.validateArgs(List("a1")).unsafeRunSync())
    intercept[IllegalArgumentException](Main.validateArgs(List("b2.")).unsafeRunSync())
    intercept[IllegalArgumentException](Main.validateArgs(List("3c.btn")).unsafeRunSync())
    intercept[IllegalArgumentException](Main.validateArgs(List(".asm")).unsafeRunSync())
    intercept[IllegalArgumentException](Main.validateArgs(List("a1.asm", "b2.asm")).unsafeRunSync())
    Main.validateArgs(List("works.asm")).unsafeRunSync() shouldEqual ()
  }

  test("correctly removes comments") {
    Main.removeCommentsAndWhitespace("") shouldEqual ""
    Main.removeCommentsAndWhitespace(" ") shouldEqual ""
    Main.removeCommentsAndWhitespace(" a ") shouldEqual "a"
    Main.removeCommentsAndWhitespace("string") shouldEqual "string"
    Main.removeCommentsAndWhitespace("//") shouldEqual ""
    Main.removeCommentsAndWhitespace("// asd") shouldEqual ""
    Main.removeCommentsAndWhitespace("  //  ") shouldEqual ""
    Main.removeCommentsAndWhitespace("  // asd  ") shouldEqual ""
    Main.removeCommentsAndWhitespace("  @200 // asd  ") shouldEqual "@200"
  }

  private val resources: String = "src/test/resources"

  test("correctly assembles Add.asm") {
    val origin = s"$resources/testdata/Add.asm"
    val destinationFileName = "/generated/Add.hack"
    val destination = s"$resources$destinationFileName"
    val expectedFileName = "/testdata/Add.hack"

    Main.assembler(origin, destination).compile.drain.unsafeRunSync()

    compareHackFile(destinationFileName, expectedFileName)
  }

  def compareHackFile(actualFileName: String, expectedFileName: String): Assertion = {
    val actualFile = Source.fromURL(getClass.getResource(actualFileName))
    val actual = actualFile.getLines.mkString("\n")

    val compareFile = Source.fromURL(getClass.getResource(expectedFileName))
    val expected = compareFile.getLines.mkString("\n")

    actualFile.close()
    compareFile.close()

    actual shouldEqual expected
  }

}
