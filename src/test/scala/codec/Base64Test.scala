package codec

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import java.nio.charset.Charset;
import org.scalacheck.Gen
import org.scalacheck.Prop.forAll

// code that interfaces with Base64Wrapper module and tests functionality
class WrapperInterface {

  def Base64WrapperDecode(
      dut: Base64Wrapper,
      inputStr: String,
      bytesPerCycle: Int
  ): Unit = {
    val params =
      Base64WrapperParams(bytesPerCycle, Base64Params.DEFAULT_BASE_CHARS)
    var allInputs =
      java.util.Base64.getEncoder.encode(inputStr.toString.getBytes())
    val numInputs =
      if (allInputs.size / (params.numDecoders * 4) == 0)
        1
      else
        allInputs.size / (params.numDecoders * 4)

    var mutInputStr = inputStr // mutable copy of input string

    for (i <- 0 until numInputs.toInt) {

      var currentInputs = allInputs.take(4 * params.numDecoders)
      allInputs = allInputs.drop(4 * params.numDecoders)
      val origChunk = mutInputStr.take(3 * params.numDecoders)
      mutInputStr = mutInputStr.drop(3 * params.numDecoders)

      dut.io.encode.poke(false.B)
      for ((c, i) <- currentInputs.zipWithIndex) {
        dut.io.input.bits(i).poke(c)
      }
      dut.io.input.valid.poke(true.B)
      while ((dut.io.output.valid.peek().litValue == 0)) dut.clock.step()
      for ((c, i) <- origChunk.zipWithIndex) {
        dut.io.output.bits(i).expect(c.U(8.W))
      }
      dut.clock.step(3)
    }
  }

  def Base64WrapperEncode(
      dut: Base64Wrapper,
      inputStr: String,
      bytesPerCycle: Int
  ): Unit = {
    val params =
      Base64WrapperParams(bytesPerCycle, Base64Params.DEFAULT_BASE_CHARS)
    var allInputs = inputStr
    val numInputs =
      Math.ceil(allInputs.length / params.bytesPerCycleForEnc.toFloat)
    for (i <- 0 until numInputs.toInt) {
      var currentInputs = allInputs.take(params.bytesPerCycleForEnc)
      allInputs = allInputs.drop(params.bytesPerCycleForEnc)

      while (currentInputs.size != params.bytesPerCycleForEnc) { // pad input on last cycle
        currentInputs = currentInputs.concat("=")
      }

      val encoded =
        java.util.Base64.getEncoder.encode(currentInputs.toString.getBytes())

      dut.io.encode.poke(true.B)
      for ((c, i) <- currentInputs.zipWithIndex) {
        dut.io.input.bits(i).poke(c)
      }
      dut.io.input.valid.poke(true.B)
      while ((dut.io.output.valid.peek().litValue == 0)) dut.clock.step()
      for ((c, i) <- encoded.zipWithIndex) {
        dut.io.output.bits(i).expect(c.U(8.W))
      }
    }
  }
}

class Base64WrapperTest extends AnyFlatSpec with ChiselScalatestTester {
  behavior of "Base64Wrapper"

  // input strings
  val lol = "lol"
  val ucsc = "University of California Santa Cruz"
  val marquez =
    "Many years later as he faced the firing squad colonel Aureliano Buendia " +
      "was to remember that distant afternoon when his father took him to discover ice"
  val herbert =
    "He felt the bubble lift him, felt it break and the dust whirlpool engulf him, " +
      "dragging him down into cool darkness. For a moment, the sensation of coolness " +
      "and the moisture were blessed relief. Then, as his planet killed him, it occurred " +
      "to Kynes that his father and all the other scientists were wrong, that the most " +
      "persistent principles of the universe were accident and error. " +
      "Even the hawks could appreciate these facts."

  // test the Base64Wrapper state machine one different input strings with different input sizes
  val t = new WrapperInterface

  // encoding tests
  for (bpc <- 3 to 12 by 3) {
    it should s"stream in ${bpc} bytes at  a time for Encoding" in {
      val params = Base64WrapperParams(bpc, Base64Params.DEFAULT_BASE_CHARS)
      test(new Base64Wrapper(params)) { dut =>
        t.Base64WrapperEncode(dut, lol, params.bytesPerCycleForEnc)
        t.Base64WrapperEncode(dut, ucsc, params.bytesPerCycleForEnc)
        t.Base64WrapperEncode(dut, marquez, params.bytesPerCycleForEnc)
        t.Base64WrapperEncode(dut, herbert, params.bytesPerCycleForEnc)
      }
    }
  }

  // decoding tests
  for (bpc <- 3 to 12 by 3) {
    it should s"stream in ${bpc} bytes at a time for Decoding" in {
      val params = Base64WrapperParams(bpc, Base64Params.DEFAULT_BASE_CHARS)
      test(new Base64Wrapper(params)) { dut =>
        t.Base64WrapperDecode(dut, lol, params.bytesPerCycleForEnc)
        t.Base64WrapperDecode(dut, ucsc, params.bytesPerCycleForEnc)
        t.Base64WrapperDecode(dut, marquez, params.bytesPerCycleForEnc)
        t.Base64WrapperDecode(dut, herbert, params.bytesPerCycleForEnc)
      }
    }
  }
}

class Base64Test extends AnyFlatSpec with ChiselScalatestTester {
  behavior of "Base64"

  // Check if a given string contains only ASCII characters
  // ref: https://stackoverflow.com/a/3585247/143733
  def isAscii(str: String): Boolean = {
    return Charset.forName("US-ASCII").newEncoder().canEncode(str);
  }

  it should "do a simple encode of exactly 3 bytes" in {
    val str = "lol"
    val params =
      Base64EncodingParams(str.length, Base64Params.DEFAULT_BASE_CHARS)
    val encoded = java.util.Base64.getEncoder.encode(str.getBytes())
    // Note: This technically just maps a single character to a byte
    // I don't think this is technically correct since it can be a base16 ?
    test(new Base64Encoder(params)) { dut =>
      for ((c, i) <- str.zipWithIndex) {
        dut.io.input(i).poke(c.toByte.U(8.W))
      }

      dut.io.output.valid.expect(true.B)
      for ((c, i) <- encoded.zipWithIndex) {
        dut.io.output.bits(i).expect(c.U(8.W))
      }
    }
  }

  it should "do a simple encode of exactly 1 bytes (2-padding)" in {
    val str = "M"
    val params =
      Base64EncodingParams(str.length, Base64Params.DEFAULT_BASE_CHARS)
    val encoded = java.util.Base64.getEncoder.encode(str.getBytes())
    // Note: This technically just maps a single character to a byte
    // I don't think this is technically correct since it can be a base16 ?
    test(new Base64Encoder(params)) { dut =>
      for ((c, i) <- str.zipWithIndex) {
        dut.io.input(i).poke(c.toByte.U(8.W))
      }

      dut.io.output.valid.expect(true.B)
      for ((c, i) <- encoded.zipWithIndex) {
        dut.io.output.bits(i).expect(c.U(8.W))
      }
    }
  }

  it should "do a simple encode of exactly 2 bytes (1-padding)" in {
    val str = "Ma"
    val params =
      Base64EncodingParams(str.length, Base64Params.DEFAULT_BASE_CHARS)
    val encoded = java.util.Base64.getEncoder.encode(str.getBytes())
    // Note: This technically just maps a single character to a byte
    // I don't think this is technically correct since it can be a base16 ?
    test(new Base64Encoder(params)) { dut =>
      for ((c, i) <- str.zipWithIndex) {
        dut.io.input(i).poke(c.toByte.U(8.W))
      }

      dut.io.output.valid.expect(true.B)
      for ((c, i) <- encoded.zipWithIndex) {
        dut.io.output.bits(i).expect(c.U(8.W))
      }
    }
  }

  it should "do a simple encode of exactly 5 bytes (1-padding)" in {
    val str = "Many "
    val params =
      Base64EncodingParams(str.length, Base64Params.DEFAULT_BASE_CHARS)
    val encoded = java.util.Base64.getEncoder.encode(str.getBytes())
    // Note: This technically just maps a single character to a byte
    // I don't think this is technically correct since it can be a base16 ?
    test(new Base64Encoder(params)).withAnnotations(Seq(WriteVcdAnnotation)) {
      dut =>
        for ((c, i) <- str.zipWithIndex) {
          dut.io.input(i).poke(c.toByte.U(8.W))
        }

        dut.io.output.valid.expect(true.B)
        for (i <- 0 until encoded.length by 4) {
          dut.io.output.bits(i).expect(encoded(i).U(8.W))
          dut.io.output.bits(i + 1).expect(encoded(i + 1).U(8.W))
          dut.io.output.bits(i + 2).expect(encoded(i + 2).U(8.W))
          dut.io.output.bits(i + 3).expect(encoded(i + 3).U(8.W))

          dut.clock.step()
        }
    }
  }

  it should "do a simple encode of larger than 3 bytes divisible by 3" in {
    val str = "Many hands make light work."
    val params =
      Base64EncodingParams(str.length, Base64Params.DEFAULT_BASE_CHARS)
    val encoded = java.util.Base64.getEncoder.encode(str.getBytes())
    // Note: This technically just maps a single character to a byte
    // I don't think this is technically correct since it can be a base16 ?
    test(new Base64Encoder(params)) { dut =>
      for ((c, i) <- str.zipWithIndex) {
        dut.io.input(i).poke(c.toByte.U(8.W))
      }

      dut.io.output.valid.expect(true.B)
      for (i <- 0 until encoded.length by 4) {
        dut.io.output.bits(i).expect(encoded(i).U(8.W))
        dut.io.output.bits(i + 1).expect(encoded(i + 1).U(8.W))
        dut.io.output.bits(i + 2).expect(encoded(i + 2).U(8.W))
        dut.io.output.bits(i + 3).expect(encoded(i + 3).U(8.W))

        dut.clock.step()
      }
    }
  }

  it should "do a simple decode of 4 characters" in {
    val str = "bG9s"
    val decoded = java.util.Base64.getDecoder.decode(str)
    // println( (decoded.map(_.toChar)).mkString )
    val params =
      Base64DecodingParams(str.length, Base64Params.DEFAULT_BASE_CHARS)
    test(new Base64Decoder(params)) { dut =>
      for ((c, i) <- str.zipWithIndex) {
        dut.io.input(i).poke(c.toByte.U(8.W))
      }

      dut.io.output.valid.expect(true.B)
      for ((c, i) <- decoded.zipWithIndex) {
        dut.io.output.bits(i).expect(c.U(8.W))
      }
    }
  }

  it should "do a simple decode of 8 characters" in {
    val str = "YWJjZGVmZ2hp"
    val decoded = java.util.Base64.getDecoder.decode(str)
    // println( (decoded.map(_.toChar)).mkString )
    val params =
      Base64DecodingParams(str.length, Base64Params.DEFAULT_BASE_CHARS)
    test(new Base64Decoder(params)) { dut =>
      for ((c, i) <- str.zipWithIndex) {
        dut.io.input(i).poke(c.toByte.U(8.W))
      }

      dut.io.output.valid.expect(true.B)

      for (i <- 0 until decoded.length by 3) {
        dut.io.output.bits(i).expect(decoded(i).U(8.W))
        dut.io.output.bits(i + 1).expect(decoded(i + 1).U(8.W))
        dut.io.output.bits(i + 2).expect(decoded(i + 2).U(8.W))

        dut.clock.step()
      }
    }
  }
}
