package codec

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import java.nio.charset.Charset;
import org.scalacheck.Gen
import org.scalacheck.Prop.forAll

class WrapperInterface {
  def Base64WrapperTest(dut: Base64Wrapper, inputStr: String, bytesPerCycle: Int): Unit = {
    val params = Base64WrapperParams(bytesPerCycle, Base64Params.DEFAULT_BASE_CHARS)
    var allInputs = inputStr
    val numInputs = Math.ceil(allInputs.length / params.bytesPerCycle.toFloat)

    for (i <- 0 until numInputs.toInt) {
      var currentInputs = allInputs.take(params.bytesPerCycle)
      allInputs = allInputs.drop(params.bytesPerCycle)
      while(currentInputs.size != params.bytesPerCycle) {
        currentInputs = currentInputs.concat("=")
      }

      val encoded = java.util.Base64.getEncoder.encode(currentInputs.toString.getBytes())

      for ((c, i) <- currentInputs.zipWithIndex) {
        dut.io.input.bits(i).poke(c)
      }
      dut.io.input.valid.poke(true.B)
      while ((dut.io.output.valid.peek().litValue() == 0)) dut.clock.step()
      for ((c, i) <- encoded.zipWithIndex) {
        dut.io.output.bits(i).expect(c.U(8.W))
      }
    }
  }
}

class Base64WrapperTest extends AnyFlatSpec with ChiselScalatestTester {
  behavior of "Base64Wrapper"

  it should "stream in bytesPerCycle bytes at a time to a Base64 module" in {

    // some sample input strings
    val lol = "lol"
    val school = "University of California Santa Cruz"
    val marquez = "Many years later as he faced the firing squad colonel Aureliano Buendia " +
                  "was to remember that distant afternoon when his father took him to discover ice"
    val herbert = "He felt the bubble lift him, felt it break and the dust whirlpool engulf him, " +
                  "dragging him down into cool darkness. For a moment, the sensation of coolness " +
                  "and the moisture were blessed relief. Then, as his planet killed him, it occurred " +
                  "to Kynes that his father and all the other scientists were wrong, that the most " +
                  "persistent principles of the universe were accident and error. " +
                  "Even the hawks could appreciate these facts."

    val params = Base64WrapperParams(3, Base64Params.DEFAULT_BASE_CHARS)

    val t = new WrapperInterface

    test(new Base64Wrapper(params)) { dut =>
      t.Base64WrapperTest(dut, lol, 3)
      t.Base64WrapperTest(dut, school, 3)
      t.Base64WrapperTest(dut, marquez, 3)
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
    val params = Base64Params(str.length, Base64Params.DEFAULT_BASE_CHARS)
    val encoded = java.util.Base64.getEncoder.encode(str.getBytes())
    // Note: This technically just maps a single character to a byte
    // I don't think this is technically correct since it can be a base16 ?
    test(new Base64(params)) { dut =>
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
    val params = Base64Params(str.length, Base64Params.DEFAULT_BASE_CHARS)
    val encoded = java.util.Base64.getEncoder.encode(str.getBytes())
    // Note: This technically just maps a single character to a byte
    // I don't think this is technically correct since it can be a base16 ?
    test(new Base64(params)) { dut =>
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
    val params = Base64Params(str.length, Base64Params.DEFAULT_BASE_CHARS)
    val encoded = java.util.Base64.getEncoder.encode(str.getBytes())
    // Note: This technically just maps a single character to a byte
    // I don't think this is technically correct since it can be a base16 ?
    test(new Base64(params)) { dut =>
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
    val params = Base64Params(str.length, Base64Params.DEFAULT_BASE_CHARS)
    val encoded = java.util.Base64.getEncoder.encode(str.getBytes())
    // Note: This technically just maps a single character to a byte
    // I don't think this is technically correct since it can be a base16 ?
    test(new Base64(params)).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
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
    val params = Base64Params(str.length, Base64Params.DEFAULT_BASE_CHARS)
    val encoded = java.util.Base64.getEncoder.encode(str.getBytes())
    // Note: This technically just maps a single character to a byte
    // I don't think this is technically correct since it can be a base16 ?
    test(new Base64(params)) { dut =>
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
}
