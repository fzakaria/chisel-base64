package codec

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

class Base64Test extends AnyFlatSpec with ChiselScalatestTester {
  behavior of "Base64"


  it should "do nothing" in {
    val params = Base64Params()
    test(new Base64(params)) { dut =>
        // do nothing
    }
  }
}