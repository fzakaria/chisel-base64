package codec

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import java.nio.charset.Charset;

class Base64Test extends AnyFlatSpec with ChiselScalatestTester {
  behavior of "Base64"

  // Check if a given string contains only ASCII characters
  // ref: https://stackoverflow.com/a/3585247/143733
  def isAscii(str: String): Boolean = {
    return Charset.forName("US-ASCII").newEncoder().canEncode(str);
  }

  it should "do a simple encode" in {
    val str = "Many hands make light work."
    val params = Base64Params(str.length)

    // Note: This technically just maps a single character to a byte
    // I don't think this is technically correct since it can be a base16 ?
    test(new Base64(params)) { dut =>
        for ( (c, i) <- str.zipWithIndex) {
            dut.io.input(i).poke(c.toByte.U(8.W))
        }
        
    }
  }
}