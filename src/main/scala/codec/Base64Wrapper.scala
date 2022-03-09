package codec

import chisel3._
import chisel3.util._

object Base64Wrapper {
  val idle :: encode :: decode :: Nil = Enum(3)
}

case class Base64WrapperParams(
    val bytesPerCycleForEnc: Int = 3,
    val base64Chars: String = Base64Params.DEFAULT_BASE_CHARS
) {
  require(bytesPerCycleForEnc > 0)
  require(bytesPerCycleForEnc % 3 == 0)
  require(base64Chars.length == 64)
  val paddedLength: Int = 4 * Math.ceil(bytesPerCycleForEnc / 3.0).toInt
  val numEncoders: Int = bytesPerCycleForEnc / 3
  val numDecoders: Int = paddedLength / 4
}

// This is a state machine that utilizes the Base64 module. It takes in
// bytesPerCycle chars at a time and streams them into the instantiated Base63 module.
class Base64Wrapper(p: Base64WrapperParams) extends Module {
  def Byte() = UInt(8.W)

  // import state and state reg
  import Base64Wrapper._
  val state = RegInit(idle)

  val io = IO(new Bundle {
    val input = Flipped(Valid(Vec(p.paddedLength, Byte())))
    val encode = Input(Bool()) // true -> encoding, false -> decoding
    val output = Valid(Vec(p.paddedLength, Byte()))
  })

  // instantiate Seq of Base64Encoder Modules
  val b64paramsenc =
    Base64EncodingParams(3, p.base64Chars) // always take in 3 bytes
  val encoders =
    Seq.fill(p.numEncoders)(Module(new Base64Encoder(b64paramsenc)))
  encoders.zipWithIndex foreach {
    case (enc, i) => {
      for (j <- 0 until 3) {
        enc.io.input(j) := io.input.bits((3 * i) + j)
      }
    }
  }

  // instantiate Seq of Base64Decoder Modules
  val b64paramsdec =
    Base64DecodingParams(4, p.base64Chars) // always take in 4 bytes
  val decoders =
    Seq.fill(p.numDecoders)(Module(new Base64Decoder(b64paramsdec)))
  val equalsChar = '='.toByte.U(8.W)
  decoders.zipWithIndex foreach {
    case (dec, i) => {
      for (j <- 0 until 4) {
        dec.io.input(j) := io.input.bits((4 * i) + j)
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
        when(io.encode) {
          state := encode
        }.otherwise {
          state := decode
        }
      }
    }
    is(encode) {
      when(encoders(0).io.output.valid) {
        state := idle
        io.output.valid := true.B
        encoders.zipWithIndex foreach {
          case (enc, i) => {
            for (j <- 0 until 4) {
              io.output.bits((4 * i) + j) := enc.io.output.bits(j)
            }
          }
        }
      }
    }
    is(decode) {
      when(decoders(0).io.output.valid) {
        state := idle
        io.output.valid := true.B
        decoders.zipWithIndex foreach {
          case (dec, i) => {
            for (j <- 0 until 3) {
              io.output.bits((3 * i) + j) := dec.io.output.bits(j)
            }
          }
        }
      }
    }
  }

  // debugging
  /*printf(
    p"state(${state}) io.input.bits(${io.input.bits}) io.output.bits(${io.output.bits}) paddedlength(${p.paddedLength})\n"
   )*/
}
