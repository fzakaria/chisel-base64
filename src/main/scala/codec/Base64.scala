package codec

import chisel3._

case class Base64Params(val sizeInBytes: Int) {
    require(sizeInBytes > 0)

    // We need to divide by 3 since 4 digits can represent 3 bytes
    // we do ceiling to account for the padding we need to generate
    // ref: https://stackoverflow.com/a/49633849/143733
    val paddedLength : Int = 4 * Math.ceil(sizeInBytes / 3.0).toInt

    val base64chars = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/";
}



class Base64(val p: Base64Params) extends Module {

    def Byte() = UInt(8.W)

    val io = IO(new Bundle {
        val input = Input(Vec(p.sizeInBytes, Byte()))
        val output = Output(Vec(p.paddedLength, Byte()))
    })


    io.output := DontCare
}