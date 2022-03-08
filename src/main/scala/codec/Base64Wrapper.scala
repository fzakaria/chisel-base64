package codec

import chisel3._
import chisel3.util._

object Base64Wrapper {
  val idle :: working :: Nil = Enum(2)
}

case class Base64WrapperParams(
    val bytesPerCycle: Int = 3,
    val base64Chars: String = Base64Params.DEFAULT_BASE_CHARS
) {
  require(bytesPerCycle > 0)
  require(bytesPerCycle % 3 == 0)
  require(base64Chars.length == 64)
  val paddedLength: Int = 4 * Math.ceil(bytesPerCycle / 3.0).toInt
  val numBase64Mods: Int = bytesPerCycle / 3
}

// This is a state machine that utilizes the Base64 module. It takes in
// bytesPerCycle chars at a time and streams them into the instantiated Base63 module.
class Base64Wrapper(p: Base64WrapperParams) extends Module {
  def Byte() = UInt(8.W)

  // import state and state reg
  import Base64Wrapper._
  val state = RegInit(idle)

  val io = IO(new Bundle {
    val input = Flipped(Valid(Vec(p.bytesPerCycle, Byte())))
    // false => encode
    // true => decode
    val mode = Input(Bool())
    val output = Valid(Vec(p.paddedLength, Byte()))
  })

  // instantiate Seq of Base64 Modules
  val b64params = Base64EncodingParams(3, p.base64Chars) // always take in 3 bytes
  val b64mods = Seq.fill(p.numBase64Mods)(Module(new Base64Encoder(b64params)))
  val equalsChar = '='.toByte.U(8.W)
  b64mods.zipWithIndex foreach {
    case (b, i) => {
      for (j <- 0 until 3) {
        b.io.input(j) := io.input.bits((3 * i) + j)
      }
    }
  }

  // default values
  io.output.valid := false.B
  io.output.bits := DontCare

  // main switch statement
  switch(state) {
    is(idle) {
      io.output.valid := false.B
      when(io.input.valid) {
        state := working
      }
    }
    is(working) {
      when(b64mods(0).io.output.valid) {
        state := idle
        io.output.valid := true.B
        b64mods.zipWithIndex foreach {
          case (b, i) => {
            for (j <- 0 until 4) {
              io.output.bits((4 * i) + j) := b.io.output.bits(j)
            }
          }
        }

      }
    }
  }

  // debugging
  // printf(
  //  p"state(${state}) io.input.bits(${io.input.bits}) io.output.bits(${io.output.bits}) paddedlength(${p.paddedLength})\n"
  // )

}
