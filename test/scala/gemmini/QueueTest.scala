package gemmini
import chisel3._
import chisel3.util._
import chisel3.iotesters.{ChiselFlatSpec, Driver, PeekPokeTester}
import chisel3.experimental.BundleLiterals._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class QueueTestXyc extends AnyFlatSpec with ChiselScalatestTester with Matchers {
  behavior of "Queue"

  it should "demonstrate flow and pipe behavior" in {
    test(new Queue(UInt(8.W), 1, true, true)) { c =>
      // Initiate the test by ensuring the queue is empty
      c.io.enq.valid.poke(false.B)
      c.io.deq.ready.poke(true.B)
      c.clock.step()

      // Test for flow-through behavior
      c.io.enq.bits.poke(10.U)
      c.io.enq.valid.poke(true.B)
      c.io.deq.ready.poke(true.B)

      c.clock.step()

      c.io.enq.bits.poke(11.U)
      c.io.enq.valid.poke(true.B)
      c.io.deq.ready.poke(false.B)
      c.clock.step()

      c.io.enq.bits.poke(12.U)
      c.io.enq.valid.poke(true.B)
      c.io.deq.ready.poke(false.B)
      c.clock.step()
      
      c.io.enq.bits.poke(13.U)
      c.io.enq.valid.poke(true.B)
      c.io.deq.ready.poke(false.B)
      c.clock.step()
      
      // c.io.deq.bits.expect(10.U)

      // Test for pipelined behavior
      c.io.enq.bits.poke(20.U)
      c.io.enq.valid.poke(true.B)
      c.io.deq.ready.poke(true.B)

      c.clock.step()

      // c.io.deq.bits.expect(20.U)
    }
  }
}

class QueueTestXyc2 extends AnyFlatSpec with ChiselScalatestTester with Matchers {
  behavior of "Queue"

  it should "demonstrate pipe behavior" in {
    test(new Queue(UInt(8.W), 3, true)) { c =>
      // Initiate the test by ensuring the queue is empty
      c.io.enq.valid.poke(false.B)
      c.io.deq.ready.poke(true.B)
      c.clock.step()

      // Test for flow-through behavior
      c.io.enq.bits.poke(10.U)
      c.io.enq.valid.poke(true.B)
      c.io.deq.ready.poke(true.B)

      c.clock.step()

      c.io.enq.bits.poke(11.U)
      c.io.enq.valid.poke(true.B)
      c.io.deq.ready.poke(true.B)
      c.clock.step()

      c.io.enq.bits.poke(12.U)
      c.io.enq.valid.poke(true.B)
      c.io.deq.ready.poke(true.B)
      c.clock.step()
      
      c.io.enq.bits.poke(13.U)
      c.io.enq.valid.poke(true.B)
      c.io.deq.ready.poke(false.B)
      c.clock.step()
      
      // c.io.deq.bits.expect(10.U)

      // Test for pipelined behavior
      c.io.enq.bits.poke(20.U)
      c.io.enq.valid.poke(true.B)
      c.io.deq.ready.poke(true.B)

      c.clock.step()

      // c.io.deq.bits.expect(20.U)
    }
  }
}

class QueueTestXyc3 extends AnyFlatSpec with ChiselScalatestTester with Matchers {
  behavior of "Queue"

  it should "demonstrate flow behavior" in {
    test(new Queue(UInt(8.W), 3, true, true)) { c =>
      // Initiate the test by ensuring the queue is empty
      c.io.enq.valid.poke(false.B)
      c.io.deq.ready.poke(true.B)
      c.clock.step()

      // Test for flow-through behavior
      c.io.enq.bits.poke(10.U)
      c.io.enq.valid.poke(true.B)
      c.io.deq.ready.poke(true.B)

      c.clock.step()

      c.io.enq.bits.poke(11.U)
      c.io.enq.valid.poke(true.B)
      c.io.deq.ready.poke(true.B)
      c.clock.step()

      c.io.enq.bits.poke(12.U)
      c.io.enq.valid.poke(true.B)
      c.io.deq.ready.poke(true.B)
      c.clock.step()
      
      c.io.enq.bits.poke(13.U)
      c.io.enq.valid.poke(true.B)
      c.io.deq.ready.poke(false.B)
      c.clock.step()
      
      // c.io.deq.bits.expect(10.U)

      // Test for pipelined behavior
      c.io.enq.bits.poke(20.U)
      c.io.enq.valid.poke(true.B)
      c.io.deq.ready.poke(true.B)

      c.clock.step()

      // c.io.deq.bits.expect(20.U)
    }
  }
}