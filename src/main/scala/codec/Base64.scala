package codec

import chisel3._
import chisel3.util._

object Base64Params {
  val DEFAULT_BASE_CHARS =
    "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"
}

abstract class Base64Params {
  def sizeInBytes: Int
  def base64Chars : String
  val paddingChar = '='
}

case class Base64EncodingParams(val sizeInBytes: Int, val base64Chars: String) extends Base64Params {
  require(sizeInBytes > 0)
  require(base64Chars.length == 64)
  // We need to divide by 3 since 4 digits can represent 3 bytes
  // we do ceiling to account for the padding we need to generate
  // ref: https://stackoverflow.com/a/49633849/143733
  val paddedLength: Int = 4 * Math.ceil(sizeInBytes / 3.0).toInt
}

case class Base64DecodingParams(val sizeInBytes: Int, val base64Chars: String) extends Base64Params {
  require(sizeInBytes > 0)
  require(base64Chars.length == 64)

  // the maximum length will be to divide into 4 characters which represent 3 octets.
  // this is the maximum because there can be padding
  val maxDecodedLength : Int = 3 * (sizeInBytes / 4.0).toInt
}

class Base64Decoder(val p: Base64DecodingParams) extends Module {
  def Byte() = UInt(8.W)

  val io = IO(new Bundle {
    val input = Input(Vec(p.sizeInBytes, Byte()))
    val output = Valid(Vec(p.maxDecodedLength, Byte()))
  })

  val mapping = VecInit(p.base64Chars.map(_.toByte.U(8.W)))

  // iterate every 4 which should be complete
  // since we assume this handles padded
  val (index, wrapped) = Counter(0 until p.sizeInBytes by 4, true.B, false.B)
  val (outputIndex, outputIndexWrapped) =
    Counter(0 until p.maxDecodedLength by 3, true.B, false.B)
    
  // Note: these can be limited to 6 width bits
  val char1 = mapping.indexWhere(_ === io.input(index))
  val char2 = mapping.indexWhere(_ === io.input(index +& 1.U))
  val char3 = mapping.indexWhere(_ === io.input(index +& 2.U))
  val char4 = mapping.indexWhere(_ === io.input(index +& 3.U))

  val byte1 = Cat(char1(5, 0), char2(5, 4))
  val byte2 = Cat(char2(3, 0), char3(5, 2))
  val byte3 = Cat(char3(1, 0), char4(5, 0))

  io.output.valid := true.B
  io.output.bits := DontCare
  io.output.bits(outputIndex) := byte1
  io.output.bits(outputIndex +& 1.U) := byte2
  io.output.bits(outputIndex +& 2.U) := byte3
}

class Base64Encoder(val p: Base64EncodingParams) extends Module {
  def Byte() = UInt(8.W)

  val io = IO(new Bundle {
    val input = Input(Vec(p.sizeInBytes, Byte()))
    val output = Valid(Vec(p.paddedLength, Byte()))
  })

  // Helpful method to read an index (byte) off a vector.
  // It simply checks if the index is passed the length and returns
  // a 0 (null byte).
  def read_byte(index: UInt, vec: Vec[UInt]): UInt = {
    val byte = Wire(Byte())
    // a bit more concise, perhaps harder to read?
    byte := Mux(index < vec.length.U, vec(index), 0.U(8.W))
    return byte
  }

  val mapping = VecInit(p.base64Chars.map(_.toByte.U(8.W)))

  // Algorithm:
  // Take 3 bytes from input, divide them into four 6bit value
  // each 6bit value maps index-0 to a base64 character
  val (index, wrapped) = Counter(0 until p.sizeInBytes by 3, true.B, false.B)
  val (outputIndex, outputIndexWrapped) =
    Counter(0 until p.paddedLength by 4, true.B, false.B)

  // Note: bit field access is inclusive (0, 7) since it's a byte
  val byte1 = mapping(read_byte(index, io.input)(7, 2))
  // Note: BE SUPER MINDFUL about when you increment the index since it will wrap.
  //       You have to use the addition that expands the width of the UInt
  //       So much frustration!
  val byte2 = mapping(
    Cat(
      read_byte(index, io.input)(1, 0),
      read_byte(index +& 1.U, io.input)(7, 4)
    )
  )
  val byte3 = mapping(
    Cat(
      read_byte(index +& 1.U, io.input)(3, 0),
      read_byte(index +& 2.U, io.input)(7, 6)
    )
  )
  val byte4 = mapping(read_byte(index +& 2.U, io.input)(5, 0))

  val equalsChar = p.paddingChar.toByte.U(8.W)

  io.output.valid := true.B
  io.output.bits := DontCare
  io.output.bits(outputIndex) := byte1
  io.output.bits(outputIndex + 1.U) := byte2
  io.output.bits(outputIndex + 2.U) := Mux(
    index +& 1.U >= io.input.length.U,
    equalsChar,
    byte3
  )
  io.output.bits(outputIndex + 3.U) := Mux(
    index +& 2.U >= io.input.length.U,
    equalsChar,
    byte4
  )

  // debugging
  // printf(
  //  p"input: ${io.input} output: ${io.output.bits}\n"
  // )
}
