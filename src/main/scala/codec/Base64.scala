package codec

import chisel3._
import chisel3.util._

case class Base64Params(val sizeInBytes: Int) {
    require(sizeInBytes > 0)

    // We need to divide by 3 since 4 digits can represent 3 bytes
    // we do ceiling to account for the padding we need to generate
    // ref: https://stackoverflow.com/a/49633849/143733
    val paddedLength : Int = 4 * Math.ceil(sizeInBytes / 3.0).toInt

    val base64Chars = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/";
}



class Base64(val p: Base64Params) extends Module {

    def Byte() = UInt(8.W)

    val io = IO(new Bundle {
        val input = Input(Vec(p.sizeInBytes, Byte()))
        val output = Valid(Vec(p.paddedLength, Byte()))
    })

    val mapping = VecInit(p.base64Chars.map(_.toByte.U(8.W)))

    // Algorithm:
    // Take 3 bytes from input, divide them into four 6bit value
    // each 6bit value mapps index-0 to a base64 character
    val (index, wrapped) = Counter(0 until p.sizeInBytes by 3, true.B, true.B)
    val (outputIndex, outputIndexWrapped) = Counter(0 until p.paddedLength by 4, true.B, true.B)

    // Note: bit field access is inclusive (0, 7) since it's a byte
    val byte1 = mapping(io.input(index)(7, 2))
    val byte2 = mapping(Cat(io.input(index)(1, 0), io.input(index + 1.U)(7, 4)))
    val byte3 = mapping(Cat(io.input(index + 1.U)(3, 0), io.input(index + 2.U)(7, 6)))
    val byte4 = mapping(io.input(index + 3.U)(5, 0))

    io.output.valid := true.B
    io.output.bits := DontCare
    io.output.bits(outputIndex) := byte1
    io.output.bits(outputIndex + 1.U) := byte2
    io.output.bits(outputIndex + 2.U) := byte3
    io.output.bits(outputIndex + 3.U) := byte4

    printf(p"output: ${io.output.bits}")
}