package gemmini
import chisel3._
import chisel3.util._
import chisel3.iotesters.{ChiselFlatSpec, Driver, PeekPokeTester}
import chisel3.experimental.BundleLiterals._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
// import robotchip.PEControl

class MeshTest extends AnyFlatSpec with ChiselScalatestTester {
  behavior of "Mesh"

  it should "perform matrix multiplication using Gemmini tiles" in {
    test(new Mesh(UInt(8.W), UInt(16.W), UInt(32.W), Dataflow.BOTH, false, 0, 4, 1, 1, 1, 2, 2)) { c =>
      // Initialize input signals
      var in_a_1 = Seq(Seq(0, 0), Seq(0, 0))
      var in_b_1 = Seq(Seq(0, 0), Seq(0, 0))
      var in_d_1 = Seq(Seq(6, 5), Seq(7, 4))
      var in_a_2 = Seq(Seq(2, 1), Seq(3, 4))
      var in_b_2 = Seq(Seq(4, 6), Seq(5, 7))
      var in_d_2 = Seq(Seq(0, 0), Seq(0, 0))
      var in_control_1 = Seq(Seq(Seq(1.U, 1.U, 0.U), Seq(1.U, 1.U, 0.U)), Seq(Seq(1.U, 1.U, 0.U), Seq(1.U, 1.U, 0.U)))
      var in_control_2 = Seq(Seq(Seq(1.U, 0.U, 0.U), Seq(1.U, 0.U, 0.U)), Seq(Seq(1.U, 0.U, 0.U), Seq(1.U, 0.U, 0.U)))
      // var in_control_1 = Seq(Seq(1.U, 1.U, 1.U), Seq(1.U, 1.U, 1.U))
      // var in_control_2 = Seq(Seq(1.U, 0.U, 0.U), Seq(1.U, 0.U, 0.U))
      var in_id = Seq(Seq(0.U), Seq(0.U))
      var in_last = Seq(Seq(false.B), Seq(false.B))
      var in_valid = Seq(Seq(true.B), Seq(true.B))

      for(i <- 1 to 2)
      {
        c.io.in_a.zip(in_a_1).foreach{ case(in_a_row_tile, a_row_tile) =>
          in_a_row_tile.zip(a_row_tile).foreach{ case(in_a_row, a_row) =>
              in_a_row.poke(a_row.U)
          }
        }

        c.io.in_b.zip(in_b_1).foreach{ case(in_b_col_tile, b_col_tile) =>
          in_b_col_tile.zip(b_col_tile).foreach{ case(in_b_col, b_col) =>
              in_b_col.poke(b_col.U)
          }
        }

        c.io.in_d.zip(in_d_1).foreach{ case(in_d_col_tile, d_col_tile) =>
          in_d_col_tile.zip(d_col_tile).foreach{ case(in_d_col, d_col) =>
              in_d_col.poke((d_col+i).U)
              // println(in_d_col.peek())
          }
        }

        c.io.in_control.zip(in_control_1).foreach{case(in_control_tile, control_tile) =>
            in_control_tile.zip(control_tile).foreach{case(i_control,control) =>
            i_control.dataflow.poke(control(0))
            i_control.propagate.poke(control(1))
            i_control.shift.poke(control(2))
            // println(control(2)) //issue
            // println(i_control.shift.peek()) //issue
            }
        }

        c.io.in_id.zip(in_id).foreach{ case(in_id_col_tile, id_col_tile) =>
          in_id_col_tile.zip(id_col_tile).foreach{ case(in_id_col, id_col) =>
              in_id_col.poke(id_col)
          }
        }
        
        c.io.in_last.zip(in_last).foreach{ case(in_last_col_tile, last_col_tile) =>
          in_last_col_tile.zip(last_col_tile).foreach{ case(in_last_col, last_col) =>
              in_last_col.poke(last_col)
          }
        }

        c.io.in_valid.zip(in_valid).foreach{ case(in_valid_col_tile, valid_col_tile) =>
          in_valid_col_tile.zip(valid_col_tile).foreach{ case(in_valid_col, valid_col) =>
              in_valid_col.poke(valid_col)
          }
        }

        c.clock.step(1)

        // println("debug_output_1")

      }

      for(j <- 1 to 8)
      {
        c.io.in_a.zip(in_a_2).foreach{ case(in_a_row_tile, a_row_tile) =>
          in_a_row_tile.zip(a_row_tile).foreach{ case(in_a_row, a_row) =>
              in_a_row.poke((a_row+j).U)
          }
        }

        c.io.in_b.zip(in_b_2).foreach{ case(in_b_col_tile, b_col_tile) =>
          in_b_col_tile.zip(b_col_tile).foreach{ case(in_b_col, b_col) =>
              in_b_col.poke((b_col+j).U)
          }
        }

        c.io.in_d.zip(in_d_2).foreach{ case(in_d_col_tile, d_col_tile) =>
          in_d_col_tile.zip(d_col_tile).foreach{ case(in_d_col, d_col) =>
              in_d_col.poke(d_col.U)
          }
        }

        c.io.in_control.zip(in_control_2).foreach{case(in_control_tile, control_tile) =>
            in_control_tile.zip(control_tile).foreach{case(i_control,control) =>
            i_control.dataflow.poke(control(0))
            i_control.propagate.poke(control(1))
            i_control.shift.poke(control(2))
            }
        }

        c.io.in_id.zip(in_id).foreach{ case(in_id_col_tile, id_col_tile) =>
          in_id_col_tile.zip(id_col_tile).foreach{ case(in_id_col, id_col) =>
              in_id_col.poke(id_col)
          }
        }
        
        c.io.in_last.zip(in_last).foreach{ case(in_last_col_tile, last_col_tile) =>
          in_last_col_tile.zip(last_col_tile).foreach{ case(in_last_col, last_col) =>
              in_last_col.poke(last_col)
          }
        }

        c.io.in_valid.zip(in_valid).foreach{ case(in_valid_col_tile, valid_col_tile) =>
          in_valid_col_tile.zip(valid_col_tile).foreach{ case(in_valid_col, valid_col) =>
              in_valid_col.poke(valid_col)
          }
        }

        c.clock.step(1)
        // println("debug_output_2")

      }

      c.clock.step(2)
      c.reset.poke(true.B)
      c.clock.step(2)

    }
  }
}



class MeshTest2 extends AnyFlatSpec with ChiselScalatestTester {
  behavior of "Mesh"

  it should "perform matrix multiplication using Gemmini tiles" in {
    test(new Mesh(SInt(8.W), SInt(16.W), SInt(32.W), Dataflow.WS, false, 0, 2, 1, 1, 1, 2, 2)) { c =>
      // Initialize input signals
      val clock = 8
      fork{var in_a = Seq(Seq(Seq(0.S), Seq(0.S)), Seq(Seq(0.S), Seq(0.S)), Seq(Seq(3.S), Seq(0.S)),
      Seq(Seq(3.S), Seq(3.S)), Seq(Seq(0.S), Seq(3.S)), Seq(Seq(0.S), Seq(3.S))
       )
      
      for (i <- 1 to 6){
        c.io.in_a.zip(in_a(i - 1)).foreach{ case(in_a_row_tile, a_row_tile) =>
          in_a_row_tile.zip(a_row_tile).foreach{ case(in_a_row, a_row) =>
              in_a_row.poke(a_row)
          }
        }
        c.clock.step(1)
      }
      }.fork{

        var in_b = Seq(Seq(Seq(0.S), Seq(0.S)), Seq(Seq(0.S), Seq(0.S)), Seq(Seq(0.S), Seq(0.S)), 
        Seq(Seq(0.S), Seq(0.S)), Seq(Seq(0.S), Seq(0.S)), Seq(Seq(0.S), Seq(0.S))     
        )
        for (i <- 1 to 6){
          c.io.in_b.zip(in_b(i - 1)).foreach{ case(in_b_col_tile, b_col_tile) =>
            in_b_col_tile.zip(b_col_tile).foreach{ case(in_b_col, b_col) =>
                in_b_col.poke(b_col)
            }
          }
          c.clock.step(1)
        }

      }.fork{

        var in_d = Seq(Seq(Seq(2.S), Seq(1.S)), Seq(Seq(2.S), Seq(2.S)), Seq(Seq(0.S), Seq(2.S)), 
        Seq(Seq(0.S), Seq(0.S)), Seq(Seq(0.S), Seq(0.S)), Seq(Seq(0.S), Seq(0.S)))
        for (i <- 1 to 6){
          c.io.in_d.zip(in_d(i - 1)).foreach{ case(in_d_col_tile, d_col_tile) =>
            in_d_col_tile.zip(d_col_tile).foreach{ case(in_d_col, d_col) =>
                in_d_col.poke(d_col)
            }
          }  
          c.clock.step(1)
        }

      }.fork{
        var in_control = Seq(Seq(Seq(Seq(0.U, 1.U, 0.U)), Seq(Seq(0.U, 1.U, 0.U))), Seq(Seq(Seq(0.U, 1.U, 1.U)), Seq(Seq(0.U, 1.U, 1.U))),
        Seq(Seq(Seq(0.U, 1.U, 0.U)), Seq(Seq(0.U, 1.U, 0.U))), Seq(Seq(Seq(0.U, 1.U, 0.U)), Seq(Seq(0.U, 1.U, 0.U))), 
        Seq(Seq(Seq(0.U, 1.U, 0.U)), Seq(Seq(0.U, 1.U, 0.U))), Seq(Seq(Seq(0.U, 1.U, 0.U)), Seq(Seq(0.U, 1.U, 0.U)))
        
        )
        for (i <- 1 to 6){
          c.io.in_control.zip(in_control(i - 1)).foreach{case(in_control_tile, control_tile) =>
              in_control_tile.zip(control_tile).foreach{case(i_control,control) =>
              i_control.shift.poke(control(0))
              i_control.dataflow.poke(control(1))
              i_control.propagate.poke(control(2))
              }
          }
          c.clock.step(1)
        }


      }.fork{

        var in_id = Seq(Seq(Seq(0.U), Seq(0.U)), Seq(Seq(0.U), Seq(0.U)), Seq(Seq(0.U), Seq(0.U)),
        Seq(Seq(0.U), Seq(0.U)), Seq(Seq(0.U), Seq(0.U)), Seq(Seq(0.U), Seq(0.U)))
        for (i <- 1 to 6){
          c.io.in_id.zip(in_id(i - 1)).foreach{ case(in_id_col_tile, id_col_tile) =>
            in_id_col_tile.zip(id_col_tile).foreach{ case(in_id_col, id_col) =>
                in_id_col.poke(id_col)
            }
          }
          c.clock.step(1)
        }


      }.fork{

        var in_last = Seq(Seq(Seq(false.B), Seq(false.B)), Seq(Seq(false.B), Seq(false.B)), Seq(Seq(false.B), Seq(false.B)),
        Seq(Seq(true.B), Seq(false.B)), Seq(Seq(false.B), Seq(true.B)), Seq(Seq(false.B), Seq(false.B))
        )
        for (i <- 1 to 6){
          c.io.in_last.zip(in_last(i - 1)).foreach{ case(in_last_col_tile, last_col_tile) =>
            in_last_col_tile.zip(last_col_tile).foreach{ case(in_last_col, last_col) =>
                in_last_col.poke(last_col)
            }
          }
          c.clock.step(1)
        }

      }.fork{
        
        var in_valid = Seq(Seq(Seq(true.B), Seq(true.B)), Seq(Seq(true.B), Seq(true.B)), Seq(Seq(true.B), Seq(true.B)),
        Seq(Seq(true.B), Seq(true.B)), Seq(Seq(false.B), Seq(true.B)), Seq(Seq(false.B), Seq(false.B)))
        for (i <- 1 to 6){
          c.io.in_valid.zip(in_valid(i - 1)).foreach{ case(in_valid_col_tile, valid_col_tile) =>
            in_valid_col_tile.zip(valid_col_tile).foreach{ case(in_valid_col, valid_col) =>
                in_valid_col.poke(valid_col)
            }
          }
          c.clock.step(1)
        }


      }.join()
      c.clock.step(4)


    }
  }
}