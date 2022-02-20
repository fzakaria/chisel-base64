package codec

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import java.nio.charset.Charset;
import org.scalacheck.Gen
import org.scalacheck.Prop.forAll

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
