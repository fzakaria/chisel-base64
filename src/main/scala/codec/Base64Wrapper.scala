package codec

import chisel3._
import chisel3.util._

object Base64Wrapper {
  val idle :: working :: Nil = Enum(2)
}

case class Base64WrapperParams(val bytesPerCycle: Int = 3,
                               val base64Chars: String = Base64Params.DEFAULT_BASE_CHARS) {
  require(bytesPerCycle > 0)
  require(base64Chars.length == 64)
  val paddedLength: Int = 4 * Math.ceil(bytesPerCycle / 3.0).toInt
}

// This is a state machine that utilizes the Base64 module. It takes in
// bytesPerCycle chars at a time and streams them into the instantiated Base64 module.
class Base64Wrapper(p: Base64WrapperParams) extends Module {
  def Byte() = UInt(8.W)

  // import state and state reg
  import Base64Wrapper._
  val state = RegInit(idle)

  val io = IO(new Bundle {
    val input = Flipped(Valid(Vec(p.bytesPerCycle, Byte())))
    val output = Valid(Vec(p.paddedLength, Byte()))
  })


  // Base64 instantiation
  val params = Base64Params(p.bytesPerCycle, p.base64Chars)
  val b64 = Module(new Base64(params))
  io.input.bits.zipWithIndex foreach { case(b, i) => b64.io.input(i) := b }

  // default values
  io.output.valid := false.B
  io.output.bits := DontCare
  val equalsChar = '='.toByte.U(8.W)


  // main switch statement
  switch (state) {
    is (idle) {
      io.output.valid := false.B
      when (io.input.valid) {
        state := working
      }
    }
    is (working) {
      when (b64.io.output.valid) {
        state := idle
        io.output.valid := true.B
        io.output.bits := b64.io.output.bits
      }
    }
  }

  // debugging
  //printf(
  //  p"state(${state}) io.input.bits(${io.input.bits}) io.output.bits(${io.output.bits}) paddedlength(${p.paddedLength})\n"
  //)

}
