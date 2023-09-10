//question: 他在scratchpad上面的交互是什么样的？
//block_col=>不应该是全部的col了嘛？ 为什么col 可以大于block_col
//max_col的问题 为啥不能大于64
package gemmini

import chisel3._
import chisel3.util._

import Util._

class ZeroWriterReq[Tag <: Data](laddr_t: LocalAddr, max_cols: Int, tag_t: Tag) extends Bundle {
  val laddr = laddr_t
  val cols = UInt(log2Up(max_cols+1).W)
  val block_stride = UInt(16.W) // TODO magic number
  val tag = tag_t

}

class ZeroWriterResp[Tag <: Data](laddr_t: LocalAddr, block_cols: Int, tag_t: Tag) extends Bundle {
  val laddr = laddr_t.cloneType
  val mask = Vec(block_cols, Bool())
  val last = Bool()
  val tag = tag_t

}

class ZeroWriter[T <: Data, U <: Data, V <: Data, Tag <: Data](config: GemminiArrayConfig[T, U, V], tag_t: Tag)
  extends Module {
  import config._
  

  val block_cols = meshColumns * tileColumns // 16 x 1 0x10
  val max_cols = (dma_maxbytes / (inputType.getWidth / 8)) max block_cols //dma_maxbyte: 64
  //max_cols : 64


  val io = IO(new Bundle {
    val req = Flipped(Decoupled(new ZeroWriterReq(local_addr_t, max_cols, tag_t)))
    val resp = Decoupled(new ZeroWriterResp(local_addr_t, block_cols, tag_t))
  })


  val req = Reg(UDValid(new ZeroWriterReq(local_addr_t, max_cols, tag_t)))

  val col_counter = Reg(UInt(log2Up(max_cols).W)) //col_counter: 4

  io.req.ready := !req.valid

  io.resp.valid := req.valid
  io.resp.bits.laddr := req.bits.laddr + req.bits.block_stride * {
    // This code block was originally just "col_counter / block_cols.U". We
    // changed it to satisfy Verilator's linter
    // laddr = laddr + block_stride * col / block_col
    //如果col大于block_col的数量，那么需要更新地址
    if (col_counter.getWidth >= log2Ceil(block_cols+1))
      (col_counter / block_cols.U(col_counter.getWidth.W))
    else
      0.U
  }
  io.resp.bits.mask.zipWithIndex.foreach { case (m, i) => m := col_counter + i.U < req.bits.cols }
  io.resp.bits.last := col_counter +& block_cols.U >= req.bits.cols
  io.resp.bits.tag := req.bits.tag

  when (io.resp.fire) {
    //req.bits.cols: 最大模数
    val next_col_counter = floorAdd(col_counter, block_cols.U, req.bits.cols)

    col_counter := next_col_counter

    when (next_col_counter === 0.U) {
      req.pop()
      io.req.ready := true.B
    }
  }

  when (io.req.fire) {
    req.push(io.req.bits)

    col_counter := 0.U
  }

  when (reset.asBool) {
    req.pop()
  }
}