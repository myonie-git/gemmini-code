
package gemmini

import chisel3._
import chisel3.util._

import Util._

class ZeroWriterReq1(laddr_t: LocalAddr, max_cols: Int) extends Bundle {
  val laddr = laddr_t
  val cols = UInt(log2Up(max_cols+1).W)
  val block_stride = UInt(16.W) // TODO magic number
  // val tag = tag_t

}

class ZeroWriterResp1(laddr_t: LocalAddr, block_cols: Int) extends Bundle {
  val laddr = laddr_t.cloneType
  val mask = Vec(block_cols, Bool())
  val last = Bool()
  // val tag = tag_t

}

class ZeroWriter1[T <: Data, U <: Data, V <: Data](config: GemminiArrayConfig[T, U, V])
  extends Module {
  import config._

  val block_cols = meshColumns * tileColumns
  val max_cols = (dma_maxbytes / (inputType.getWidth / 8)) max block_cols
  //0x40

  val io = IO(new Bundle {
    val req = Flipped(Decoupled(new ZeroWriterReq1(local_addr_t, max_cols)))
    val resp = Decoupled(new ZeroWriterResp1(local_addr_t, block_cols))
    val test_wire1 = Output((UInt(7.W)));
    val test_wire2 = Output((UInt(7.W)));
    val test_wire3 = Output((UInt(7.W)));
  })

  io.test_wire1 := max_cols.U;
  io.test_wire2 := tileColumns.U;
  io.test_wire3 := meshColumns.U;

  val req = Reg(UDValid(new ZeroWriterReq1(local_addr_t, max_cols)))

  val col_counter = Reg(UInt(log2Up(max_cols).W))

  io.req.ready := !req.valid

  io.resp.valid := req.valid
  io.resp.bits.laddr := req.bits.laddr + req.bits.block_stride * {
    // This code block was originally just "col_counter / block_cols.U". We
    // changed it to satisfy Verilator's linter
    if (col_counter.getWidth >= log2Ceil(block_cols+1))
      (col_counter / block_cols.U(col_counter.getWidth.W))
    else
      0.U
  }
  io.resp.bits.mask.zipWithIndex.foreach { case (m, i) => m := col_counter + i.U < req.bits.cols }
  io.resp.bits.last := col_counter +& block_cols.U >= req.bits.cols
  // io.resp.bits.tag := req.bits.tag

  when (io.resp.fire) {
    val next_col_counter = floorAdd(col_counter, block_cols.U, req.bits.cols)
    //输入的最大值
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
