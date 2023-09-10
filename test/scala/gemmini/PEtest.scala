package gemmini
import chisel3._
import chisel3.util._
import chisel3.iotesters.{ChiselFlatSpec, Driver, PeekPokeTester}
import chisel3.experimental.BundleLiterals._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers


// class PETests(c: PE) extends PeekPokeTester(c) {
//   // for (i <- 0 until 10) {
//   //   val in0 = rnd.nextInt(1 << c.w)
//   //   val in1 = rnd.nextInt(1 << c.w)
//   //   poke(c.io.in0, in0)
//   //   poke(c.io.in1, in1)
//   //   step(1)
//   //   expect(c.io.out, (in0 + in1)&((1 << c.w)-1))
//   // }

//   val in_a = 1
//   val in_b = 1
//   val in_d = 1
//   val dataflow = 1
//   val propagate = 1
//   val shift = 1
//   val in_id  = 0
//   val in_last = 0
//   val in_valid = 1
//   poke(c.io.in_a, in_a)
//   poke(c.io.in_b, in_b)
//   poke(c.io.in_d, in_d)
//   poke(c.io.in_control.dataflow, dataflow)
//   poke(c.io.in_control.propagate, propagate)
//   poke(c.io.in_control.shift, shift)
//   poke(c.io.in_control.shift, shift)
//   poke(c.io.in_id, in_id)
//   poke(c.io.in_last, in_last)
//   poke(c.io.in_valid, in_valid)
//   step(1)


//   // val in_a = Input(inputType)
//   //   val in_b = Input(outputType)
//   //   val in_d = Input(outputType)
//   //   val out_a = Output(inputType)
//   //   val out_b = Output(outputType)
//   //   val out_c = Output(outputType)

//   //   val in_control = Input(new PEControl(accType))
//   //   val out_control = Output(new PEControl(accType))

//   //   val in_id = Input(UInt(log2Up(max_simultaneous_matmuls).W))
//   //   val out_id = Output(UInt(log2Up(max_simultaneous_matmuls).W))

//   //   val in_last = Input(Bool())
//   //   val out_last = Output(Bool())

//   //   val in_valid = Input(Bool())
//   //   val out_valid = Output(Bool())

//   //   val bad_dataflow = Output(Bool())


// }
class PETester extends AnyFlatSpec with ChiselScalatestTester with Matchers {

    it should "test static circuits" in {
    test(new PE(UInt(8.W), UInt(16.W), UInt(32.W), Dataflow.BOTH,4)) { c =>
        val in_a = 1
        val in_b = 1
        val in_d = 1
        val dataflow = 1
        val propagate = 1
        val shift = 1
        val in_id  = 0
        val in_last = 0
        val in_valid = 1
        // dataflow = 1 is WS, propagate = 0: compute, 1 propagate
        val df = 0.U    // dataflow 1:WS, 0:OS
        val prop_para1 = 1.U //propagate
        val d = 3
        for(i <- 1 to 1)
        {
          c.io.in_a.poke(0.U)
          c.io.in_b.poke(0.U)
          c.io.in_d.poke((d+i).U)
          c.io.in_control.dataflow.poke(df)
          c.io.in_control.propagate.poke(prop_para1)
          c.io.in_control.shift.poke(1.U)
          c.io.in_id.poke(0.U)
          c.io.in_last.poke(0.B)
          c.io.in_valid.poke(1.B)
          c.io.out_a.peek()
          c.io.out_b.peek()
          c.clock.step(1)
        }
        val a = 1
        val b = 1
        val prop_para2 = 0.U //compute
        println("Debug")
        println(c.io.out_a.peek())
        for(j <- 1 to 8)
        {
          c.io.in_a.poke((a+j).U)
          c.io.in_b.poke((b+j-1).U)
          c.io.in_d.poke(0.U)
          c.io.in_control.dataflow.poke(df)
          c.io.in_control.propagate.poke(prop_para2)
          c.io.in_control.shift.poke(1.U)
          c.io.in_id.poke(0.U)
          c.io.in_last.poke(0.B)
          c.io.in_valid.poke(1.B)
          c.io.out_a.peek()
          c.io.out_b.peek()
          c.clock.step(1)
        }
    }
  }

}
// class PETester extends ChiselFlatSpec {
//   behavior of "PE"
//   backends foreach {backend =>
//     it should s"correctly add randomly generated numbers in $backend" in {
//       Driver(() => new PE(SInt(8.W), SInt(8.W), SInt(8.W), Dataflow.BOTH,4), backend)(c => new PETests(c)) should be (true)
//     }
//   }
// }


//./run-examples.sh GCD --backend-name verilator