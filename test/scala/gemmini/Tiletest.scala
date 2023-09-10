package gemmini
import chisel3._
import chisel3.util._
import chisel3.iotesters.{ChiselFlatSpec, Driver, PeekPokeTester}
import chisel3.experimental.BundleLiterals._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import gemmini.PEControl


class TileSpec extends AnyFlatSpec with ChiselScalatestTester with Matchers  {
  it should "broadcast inputs and outputs" in {
    test(new Tile(UInt(8.W), UInt(8.W), UInt(32.W), Dataflow.BOTH, false, 4, 1, 1)) { c =>
      // Input values
      val in_a_1 = Seq(0, 0)
      val in_b_1 = Seq(0, 0)
      val in_d_1 = Seq(2, 3)
      val in_a_2 = Seq(4, 5)
      val in_b_2 = Seq(6, 7)
      val in_d_2 = Seq(0, 0)
    //   val in_control_PEControl1 = new PEControl(UInt(8.W))
    //   in_control_PEControl1.dataflow := 1.U
    //   in_control_PEControl1.propagate := 1.U
    //   in_control_PEControl1.shift := 0.U
    //   val in_control_PEControl2 = new PEControl(UInt(8.W))
    //   in_control_PEControl2.dataflow := 1.U
    //   in_control_PEControl2.propagate := 1.U
    //   in_control_PEControl2.shift := 0.U
     
      // {1, 1, 0} WS Propagate shift
      val in_control_1 = Seq(
        Seq(0.U, 1.U, 0.U),
        Seq(0.U, 1.U, 0.U)
      )

      // {1, 0, 0} WS Compute
      val in_control_2 = Seq(
        Seq(0.U, 0.U, 0.U),
        Seq(0.U, 0.U, 0.U)
      )

      val in_id = Seq(0.U, 1.U)
      val in_last = Seq(false.B, false.B)
      val in_valid = Seq(true.B, false.B)

      // Expected outputs
      val exp_out_a = Seq(1.U, 2.U)
      val exp_out_b = Seq(555.U, 20.U)
      val exp_out_c = Seq(63711.U, 127497.U)
    //   val exp_out_control = Seq(
    //     new PEControl(10.U, 0.U, 1.U),
    //     new PEControl(20.U, 1.U, 0.U)
    //   )
      val exp_out_id = Seq(0.U, 1.U)
      val exp_out_last = Seq(false.B, true.B)
      val exp_out_valid = Seq(true.B, false.B)

      // Propagate weight
      for(i <- 1 to 1)
      {
        c.io.in_a.zip(in_a_1).foreach { case (port, value) => port.poke(value.U) }
        c.io.in_b.zip(in_b_1).foreach { case (port, value) => port.poke(value.U) }
        c.io.in_d.zip(in_d_1).foreach { case (port, value) => port.poke((value+i).U) }
        c.io.in_control.zip(in_control_1).foreach { case (port, value) => port.dataflow.poke(value(0)) }
        c.io.in_control.zip(in_control_1).foreach { case (port, value) => port.propagate.poke(value(1)) }
        c.io.in_control.zip(in_control_1).foreach { case (port, value) => port.shift.poke(value(2)) }
        c.io.in_id.zip(in_id).foreach { case (port, value) => port.poke(value) }
        c.io.in_last.zip(in_last).foreach { case (port, value) => port.poke(value) }
        c.io.in_valid.zip(in_valid).foreach { case (port, value) => port.poke(value) }

        // Verify outputs after a single clock cycle
        c.clock.step(1)
      }

      // Do Compute
      for(j <- 1 to 8)
      {
        c.io.in_a.zip(in_a_2).foreach { case (port, value) => port.poke((value+j).U) }
        c.io.in_b.zip(in_b_2).foreach { case (port, value) => port.poke((value+j).U) }
        c.io.in_d.zip(in_d_2).foreach { case (port, value) => port.poke(value.U) }
        c.io.in_control.zip(in_control_2).foreach { case (port, value) => port.dataflow.poke(value(0)) }
        c.io.in_control.zip(in_control_2).foreach { case (port, value) => port.propagate.poke(value(1)) }
        c.io.in_control.zip(in_control_2).foreach { case (port, value) => port.shift.poke(value(2)) }
        c.io.in_id.zip(in_id).foreach { case (port, value) => port.poke(value) }
        c.io.in_last.zip(in_last).foreach { case (port, value) => port.poke(value) }
        c.io.in_valid.zip(in_valid).foreach { case (port, value) => port.poke(value) }

        // Verify outputs after a single clock cycle
        c.clock.step(1)
      }
    //   c.io.out_a.zip(exp_out_a).foreach { case (port, value) => port.expect(value.asUInt) }
    //   c.io.out_b.zip(exp_out_b).foreach { case (port, value) => port.expect(value.asUInt) }
    //   c.io.out_c.zip(exp_out_c).foreach { case (port, value) => port.expect(value.asUInt) }
    //   //c.io.out_control.zip(exp_out_control).foreach { case (port, value) => port.expect(value) }
    //   c.io.out_id.zip(exp_out_id).foreach { case (port, value) => port.expect(value) }
    //   c.io.out_last.zip(exp_out_last).foreach { case (port, value) => port.expect(value) }
    //   c.io.out_valid.zip(exp_out_valid).foreach { case (port, value) => port.expect(value) }
    }
  }
}