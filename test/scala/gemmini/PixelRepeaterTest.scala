package gemmini
import chisel3._
import chisel3.util._
import chisel3.iotesters.{ChiselFlatSpec, Driver, PeekPokeTester}
import chisel3.experimental.BundleLiterals._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import LocalAddr._

class PixelRepeaterTester1 extends AnyFlatSpec with ChiselScalatestTester with Matchers {
  behavior of "PixelRepeater"
  it should "repeat pixel" in {
    test(new PixelRepeater1(GemminiConfigs.defaultConfig.inputType,(new LocalAddr(4, 256, 4, 256)), 16, 1, false)) { c => 
        /*add your code here*/

        c.io.req.valid.poke(true.B)

        for(i <- 0 until 16){
            c.io.req.bits.in(i).poke((1 + i).S)
            c.io.req.bits.mask(i).poke(true.B)
        }

        // c.io.req.bits.in.poke(VecInit(Seq.fill(16)(123.S))) // Example data
        // c.io.req.bits.mask.poke(VecInit(Seq.fill(16)(true.B)))
        c.io.req.bits.laddr.data.poke(10.U)
        c.io.req.bits.len.poke(20.U) // Example length
        c.io.req.bits.pixel_repeats.poke(1.U)
        c.io.req.bits.last.poke(true.B)
        c.io.resp.ready.poke(true.B)
        c.clock.step()
        c.clock.step() 
        c.clock.step()
        c.clock.step()
        c.clock.step()
        c.clock.step()
        
    }
  }
}

class PixelRepeaterTester2 extends AnyFlatSpec with ChiselScalatestTester with Matchers {
  behavior of "PixelRepeater"
  it should "repeat pixel2" in {
    test(new PixelRepeater1(GemminiConfigs.defaultConfig.inputType,(new LocalAddr(4, 256, 4, 256)), 16, 1, false)) { c => 
        /*add your code here*/

        c.io.req.valid.poke(true.B)

        for(i <- 0 until 16){
            c.io.req.bits.in(i).poke((1 + i).S)
            c.io.req.bits.mask(i).poke(false.B)
        }
        c.io.req.bits.mask(3).poke(true.B)
        c.io.req.bits.mask(4).poke(true.B)

        // c.io.req.bits.in.poke(VecInit(Seq.fill(16)(123.S))) // Example data
        // c.io.req.bits.mask.poke(VecInit(Seq.fill(16)(true.B)))
        c.io.req.bits.laddr.data.poke(10.U)
        c.io.req.bits.len.poke(2.U) // Example length
        c.io.req.bits.pixel_repeats.poke(10.U)
        c.io.req.bits.last.poke(true.B)
        c.io.resp.ready.poke(true.B)
        c.clock.step()
        c.io.req.valid.poke(false.B)
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
        c.clock.step()
        

        c.io.req.bits.len.poke(18.U) // Example length
        c.io.req.valid.poke(true.B)
        for(i <- 0 until 16){
            c.io.req.bits.in(i).poke((17 + i).S)
            c.io.req.bits.mask(i).poke(true.B)
        }
        c.io.req.bits.laddr.data.poke(20.U)
        

        c.clock.step()

        
        c.io.req.valid.poke(false.B)

        c.clock.step()
        c.clock.step()
        c.clock.step()
        
    }
  }
}

class PixelRepeaterTester3 extends AnyFlatSpec with ChiselScalatestTester with Matchers {
  behavior of "PixelRepeater"
  it should "repeat pixel3" in {
    test(new PixelRepeater1(GemminiConfigs.defaultConfig.inputType,(new LocalAddr(4, 256, 4, 256)), 16, 1, false)) { c => 
        /*add your code here*/

        c.io.req.valid.poke(true.B)

        for(i <- 0 until 16){
            c.io.req.bits.in(i).poke((1 + i).S)
            c.io.req.bits.mask(i).poke(true.B)
        }
        // c.io.req.bits.in.poke(VecInit(Seq.fill(16)(123.S))) // Example data
        // c.io.req.bits.mask.poke(VecInit(Seq.fill(16)(true.B)))
        c.io.req.bits.laddr.data.poke(10.U)
        c.io.req.bits.len.poke(18.U) // Example length
        c.io.req.bits.pixel_repeats.poke(1.U)
        c.io.req.bits.last.poke(false.B)
        c.io.resp.ready.poke(true.B)


        
        c.clock.step()

        for(i <- 0 until 16){
            c.io.req.bits.in(i).poke(0.S)
            c.io.req.bits.mask(i).poke(false.B)
        }
        c.io.req.bits.in(0).poke(17.S)
        c.io.req.bits.in(1).poke(18.S)
        c.io.req.bits.mask(0).poke(true.B)
        c.io.req.bits.mask(1).poke(true.B)
        c.io.req.bits.last.poke(true.B)
        c.clock.step() 
        c.io.req.valid.poke(false.B)
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
        
        c.io.req.valid.poke(true.B)

        for(i <- 0 until 16){
            c.io.req.bits.in(i).poke((1 + i).S)
            c.io.req.bits.mask(i).poke(true.B)
        }


        // c.io.req.bits.in.poke(VecInit(Seq.fill(16)(123.S))) // Example data
        // c.io.req.bits.mask.poke(VecInit(Seq.fill(16)(true.B)))
        c.io.req.bits.laddr.data.poke(0x100.U)
        c.io.req.bits.len.poke(18.U) // Example length
        c.io.req.bits.pixel_repeats.poke(10.U)
        c.io.req.bits.last.poke(false.B)
        c.io.resp.ready.poke(true.B)

        c.clock.step()

        for(i <- 0 until 16){
            c.io.req.bits.in(i).poke(0.S)
            c.io.req.bits.mask(i).poke(false.B)
        }
        c.io.req.bits.in(0).poke(17.S)
        c.io.req.bits.in(1).poke(18.S)
        c.io.req.bits.mask(0).poke(true.B)
        c.io.req.bits.mask(1).poke(true.B)
        c.io.req.bits.last.poke(true.B)
        
        c.clock.step() 
        c.io.req.valid.poke(false.B)

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