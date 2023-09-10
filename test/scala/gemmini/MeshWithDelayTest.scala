package gemmini
import chisel3._
import chisel3.util._
import chisel3.iotesters.{ChiselFlatSpec, Driver, PeekPokeTester}
import chisel3.experimental.BundleLiterals._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import gemmini.PEControl
import gemmini._
import gemmini.Util._


class MeshWithDelayTest extends AnyFlatSpec with ChiselScalatestTester {
  behavior of "MeshWithDelayTest"


   it should "perform matrix multiplication using Gemmini tiles" in {
    val block_size = 2
    val rob_entries = 3
    val local_addr_t = new LocalAddr(4, 256, 4, 256)

    val mesh_tag = new Bundle with TagQueueTag {
        val rob_id = UDValid(UInt(log2Up(rob_entries).W))
        //输出地址
        val addr = local_addr_t.cloneType
        val rows = UInt(log2Up(block_size + 1).W)
        val cols = UInt(log2Up(block_size + 1).W)

        override def make_this_garbage(dummy: Int = 0): Unit = {
        rob_id.valid := false.B
        addr.make_this_garbage()
        }
    }
    // test(new MeshWithDelays(SInt(8.W), SInt(16.W), SInt(32.W), mesh_tag, Dataflow.WS, false, 0, 1, 1, 1, 2, 2, 1, 1)){
    //     c =>

    //     c.io.a.valid.poke(true.B)
    //     var in_a = Seq(Seq(1.S), Seq(1.S))
        
    //     c.io.a.bits.zip(in_a).foreach{ case(in_a_row_tile, a_row_tile) =>
    //         in_a_row_tile.zip(a_row_tile).foreach{ case(in_a_row, a_row) =>
    //             in_a_row.poke(a_row)
    //         }
    //     }


    //     c.io.b.valid.poke(true.B)
    //     var in_b = Seq(Seq(1.S), Seq(1.S))
    //     c.io.b.bits.zip(in_b).foreach{ case(in_b_row_tile, b_row_tile) =>
    //         in_b_row_tile.zip(b_row_tile).foreach{ case(in_b_row, b_row) =>
    //             in_b_row.poke(b_row)
    //         }
    //     }


    //     c.io.d.valid.poke(true.B)
    //     var in_d = Seq(Seq(2.S), Seq(1.S))
    //     c.io.d.bits.zip(in_d).foreach{ case(in_d_row_tile, d_row_tile) =>
    //         in_d_row_tile.zip(d_row_tile).foreach{ case(in_d_row, d_row) =>
    //             in_d_row.poke(d_row)
    //         }
    //     }

    //     c.io.req.valid.poke(true.B)
    //     var in_control = Seq(0.U, 1.U, 0.U)
    //     val a_transpose = false.B
    //     val bd_transpose = false.B
    //     val total_rows = 2.U
    //     val tag = Seq(true.B, 1.U, 0.U, 2.U, 2.U)
    //     val flush = 0.U // TODO magic number
    //     c.io.req.bits.pe_control.shift.poke(in_control(0))
    //     c.io.req.bits.pe_control.dataflow.poke(in_control(1))
    //     c.io.req.bits.pe_control.propagate.poke(in_control(2))
    //     c.io.req.bits.a_transpose.poke(a_transpose)
    //     c.io.req.bits.bd_transpose.poke(bd_transpose)
    //     c.io.req.bits.total_rows.poke(total_rows)
    //     c.io.req.bits.tag.rob_id.valid.poke(tag(0).asBool())
    //     c.io.req.bits.tag.rob_id.bits.poke(tag(1))

    //     c.io.req.bits.tag.addr.is_acc_addr.poke(0.B)
    //     c.io.req.bits.tag.addr.accumulate.poke(0.B)
    //     c.io.req.bits.tag.addr.read_full_acc_row.poke(0.B)
    //     c.io.req.bits.tag.addr.garbage.poke(0.U)
    //     c.io.req.bits.tag.addr.garbage_bit.poke(0.U)
    //     c.io.req.bits.tag.addr.data.poke(tag(2))
    //     c.io.req.bits.tag.rows.poke(tag(3))
    //     c.io.req.bits.tag.cols.poke(tag(4))
    //     c.io.req.bits.flush.poke(flush)

    //     c.clock.step(10)

    // }


    test(new MeshWithDelays(SInt(8.W), SInt(16.W), SInt(32.W), mesh_tag, Dataflow.WS, false, 0, 1, 1, 1, 2, 2, 1, 1)){
        c =>
        var in_a = Seq(Seq(Seq(1.S), Seq(1.S)), Seq(Seq(1.S), Seq(1.S)), Seq(Seq(3.S), Seq(0.S)),
            Seq(Seq(3.S), Seq(3.S)), Seq(Seq(0.S), Seq(3.S)), Seq(Seq(0.S), Seq(3.S))
        )
        var in_b = Seq(Seq(Seq(1.S), Seq(1.S)), Seq(Seq(1.S), Seq(1.S)), Seq(Seq(0.S), Seq(0.S)), 
        Seq(Seq(0.S), Seq(0.S)), Seq(Seq(0.S), Seq(0.S)), Seq(Seq(0.S), Seq(0.S))     
        )
        var in_d = Seq(Seq(Seq(2.S), Seq(1.S)), Seq(Seq(2.S), Seq(2.S)), Seq(Seq(0.S), Seq(2.S)), 
        Seq(Seq(0.S), Seq(0.S)), Seq(Seq(0.S), Seq(0.S)), Seq(Seq(0.S), Seq(0.S)))
        var in_control = Seq(Seq(Seq(Seq(0.U, 1.U, 0.U)), Seq(Seq(0.U, 1.U, 0.U))), Seq(Seq(Seq(0.U, 1.U, 1.U)), Seq(Seq(0.U, 1.U, 1.U))),
        Seq(Seq(Seq(0.U, 1.U, 0.U)), Seq(Seq(0.U, 1.U, 0.U))), Seq(Seq(Seq(0.U, 1.U, 0.U)), Seq(Seq(0.U, 1.U, 0.U))), 
        Seq(Seq(Seq(0.U, 1.U, 0.U)), Seq(Seq(0.U, 1.U, 0.U))), Seq(Seq(Seq(0.U, 1.U, 0.U)), Seq(Seq(0.U, 1.U, 0.U)))
        
        )
        val a_transpose = false.B
        val bd_transpose = false.B
        val total_rows = 2.U
        val tag = Seq(true.B, 1.U, 0.U, 2.U, 2.U)
        val flush = 0.U // TODO magic number

        for (i <- 0 to 5){
            c.io.a.valid.poke(true.B)
            c.io.a.bits.zip(in_a(i)).foreach{ case(in_a_row_tile, a_row_tile) =>
                in_a_row_tile.zip(a_row_tile).foreach{ case(in_a_row, a_row) =>
                    in_a_row.poke(a_row)
                }
            }
            c.io.b.valid.poke(true.B)
            c.io.b.bits.zip(in_b(i)).foreach{ case(in_b_row_tile, b_row_tile) =>
                in_b_row_tile.zip(b_row_tile).foreach{ case(in_b_row, b_row) =>
                    in_b_row.poke(b_row)
                }
            }
            c.io.d.valid.poke(true.B)
            c.io.d.bits.zip(in_d(i)).foreach{ case(in_d_row_tile, d_row_tile) =>
                in_d_row_tile.zip(d_row_tile).foreach{ case(in_d_row, d_row) =>
                    in_d_row.poke(d_row)
                }
            }
            c.io.req.valid.poke(true.B)
            c.io.req.bits.pe_control.shift.poke(in_control(i)(0)(0)(0))
            c.io.req.bits.pe_control.dataflow.poke(in_control(i)(0)(0)(1))
            c.io.req.bits.pe_control.propagate.poke(in_control(i)(0)(0)(2))
            c.io.req.bits.a_transpose.poke(a_transpose)
            c.io.req.bits.bd_transpose.poke(bd_transpose)
            c.io.req.bits.total_rows.poke(total_rows)
            c.io.req.bits.tag.rob_id.valid.poke(tag(0).asBool())
            c.io.req.bits.tag.rob_id.bits.poke(tag(1))

            c.io.req.bits.tag.addr.is_acc_addr.poke(0.B)
            c.io.req.bits.tag.addr.accumulate.poke(0.B)
            c.io.req.bits.tag.addr.read_full_acc_row.poke(0.B)
            c.io.req.bits.tag.addr.garbage.poke(0.U)
            c.io.req.bits.tag.addr.garbage_bit.poke(0.U)
            c.io.req.bits.tag.addr.data.poke(tag(2))
            c.io.req.bits.tag.rows.poke(tag(3))
            c.io.req.bits.tag.cols.poke(tag(4))
            c.io.req.bits.flush.poke(flush)
            c.clock.step(1)

        }


    }
   }

}