package com.mattypb.hack.assembler

import cats.effect.IO
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

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

}
