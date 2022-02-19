package codec

import chisel3._
import chisel3.util._

case class Base64Params(val sizeInBytes: Int) {
    require(sizeInBytes > 0)

    // We need to divide by 3 since 4 digits can represent 3 bytes
    // we do ceiling to account for the padding we need to generate
    // ref: https://stackoverflow.com/a/49633849/143733
    val paddedLength : Int = 4 * Math.ceil(sizeInBytes / 3.0).toInt

    // TODO: Make this a parameterized input
    // make sure to assert that the input is 64 characters long.
    val base64Chars = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/";
}



class Base64(val p: Base64Params) extends Module {

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
        when(index < vec.length.U) {
            byte := vec(index)
        }.otherwise {
            byte := 0.U(8.W)
        }
        return byte
    }

    val mapping = VecInit(p.base64Chars.map(_.toByte.U(8.W)))

    // Algorithm:
    // Take 3 bytes from input, divide them into four 6bit value
    // each 6bit value mapps index-0 to a base64 character
    val (index, wrapped) = Counter(0 until p.sizeInBytes by 3, true.B, false.B)
    val (outputIndex, outputIndexWrapped) = Counter(0 until p.paddedLength by 4, true.B, false.B)

    // Note: bit field access is inclusive (0, 7) since it's a byte
    val byte1 = mapping(read_byte(index, io.input)(7, 2))
    val byte2 = mapping(Cat(read_byte(index, io.input)(1, 0), read_byte(index + 1.U, io.input)(7, 4)))
    val byte3 = mapping(Cat(read_byte(index + 1.U, io.input)(3, 0), read_byte(index + 2.U, io.input)(7, 6)))
    val byte4 = mapping(read_byte(index + 2.U, io.input)(5, 0))

    val equalsChar = '='.toByte.U(8.W)

    io.output.valid := true.B
    io.output.bits := DontCare
    io.output.bits(outputIndex) := byte1
    io.output.bits(outputIndex + 1.U) := byte2
    io.output.bits(outputIndex + 2.U) := Mux(index + 1.U >= io.input.length.U, equalsChar, byte3)
    io.output.bits(outputIndex + 3.U) := Mux(index + 2.U >= io.input.length.U, equalsChar, byte4)

    printf(p"index: ${index} output: ${io.output.bits}\n")
}