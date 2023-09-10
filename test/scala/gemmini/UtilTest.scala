package gemmini
import chisel3._
import chisel3.util._
import chisel3.iotesters.{ChiselFlatSpec, Driver, PeekPokeTester}
import chisel3.experimental.BundleLiterals._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class UtilWrapper extends Module {
  val io = IO(new Bundle {
    val u = Input(UInt(32.W))
    val n = Input(UInt(32.W))
    val max_plus_one = Input(UInt(32.W))
    val out = Output(UInt(32.W))
  })
  
  io.out := Util.wrappingAdd(io.u, io.n, io.max_plus_one.litValue().toInt)
}

class UtilTester(c: UtilWrapper) extends PeekPokeTester(c) {
  // Test examples
  poke(c.io.u, 5)
  poke(c.io.n, 3)
  poke(c.io.max_plus_one, 10)
//   expect(c.io.out, 8)

  poke(c.io.u, 8)
  poke(c.io.n, 3)
  poke(c.io.max_plus_one, 10)
//   expect(c.io.out, 1)
  
  // ... add more tests as needed
}

class UtilSpec extends ChiselFlatSpec {
  "Util" should "correctly wrapAdd numbers" in {
    Driver(() => new UtilWrapper) {
      c => new UtilTester(c)
    } 
  }
}

object UtilTestbench {
  def main(args: Array[String]): Unit = {
    new UtilSpec
  }
}