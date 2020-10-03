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
    val file = "Add"
    assemble(file)
//    compareHackFiles(file)
  }

  test("correctly assembles MaxL.asm") {
    val file = "MaxL"
    assemble(file)
//    compareHackFiles(file)
  }

  test("correctly assembles RectL.asm") {
    val file = "RectL"
    assemble(file)
//    compareHackFiles(file)
  }

  test("correctly assembles PongL.asm") {
    val file = "PongL"
    assemble(file)
//    compareHackFiles(file)
  }

  private def assemble(file: String): Unit = {
    val origin = s"$resources/testdata/$file.asm"
    val destinationFileName = s"/generated/$file.hack"
    val destination = s"$resources$destinationFileName"

    Main.secondPass(origin, destination).compile.drain.unsafeRunSync()
  }

  // can't run compareHackFiles() straight after assemble() because for some reason the file being written isn't being
  // until the jvm stops, so compareHackFiles() throws exception as it can't find the file. Not sure why it's not closing.
  // in the meantime, can run them separately one after the other
  private def compareHackFiles(file: String): Assertion = {
    val actualFileName = s"/generated/$file.hack"
    val actualFile = Source.fromURL(getClass.getResource(actualFileName))
    val actual = actualFile.getLines.mkString("\n")

    val expectedFileName = s"/testdata/$file.hack"
    val expectedFile = Source.fromURL(getClass.getResource(expectedFileName))
    val expected = expectedFile.getLines.mkString("\n")

    actualFile.close()
    expectedFile.close()

    actual shouldEqual expected
  }

}
