package gemmini
import chisel3._
import chisel3.util._
import chisel3.iotesters.{ChiselFlatSpec, Driver, PeekPokeTester}
import chisel3.experimental.BundleLiterals._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import LocalAddr._

//GemminiConfigs.defaultConfig
class ZeroWriterTester extends AnyFlatSpec with ChiselScalatestTester with Matchers {
  behavior of "ZeroWriter"
  it should "write zero" in {
    //test case body here
    test(new ZeroWriter1(GemminiConfigs.defaultConfig)) { c => 
      // Example req setup
      
      // val local_addr_t = new LocalAddr(4, 256, 4, 256)
      c.io.req.bits.laddr.data.poke(0.U)
      
      c.io.req.bits.cols.poke(32.U) // for instance

      c.io.req.bits.block_stride.poke(4.U)
      // c.io.req.bits.tag.poke(0.U.asTypeOf()) // or some other default value based on Tag type
      
      c.io.req.valid.poke(true.B)

      c.io.resp.ready.poke(true.B)
      // Make sure to tick the circuit to let it process
      c.clock.step()
      c.clock.step()
      c.clock.step()
      c.clock.step()
      c.clock.step()
      c.clock.step()
      c.clock.step()
      c.clock.step()
      c.clock.step()
      c.clock.step()
      c.clock.step()
      c.clock.step()
      c.clock.step()
      c.clock.step()
      c.clock.step()
      c.clock.step()
    }
  }

}