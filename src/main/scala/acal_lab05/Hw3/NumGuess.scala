package acal_lab05.Hw3

import chisel3._
import chisel3.util._

class NumGuess(seed:Int = 1) extends Module{
    require (seed > 0 , "Seed cannot be 0")

    val io  = IO(new Bundle{
        val gen = Input(Bool())
        val guess = Input(UInt(16.W))
        val puzzle = Output(Vec(4,UInt(4.W)))
        val ready  = Output(Bool())
        val g_valid  = Output(Bool())
        val A      = Output(UInt(3.W))
        val B      = Output(UInt(3.W))

        //don't care at Hw6-3-2 but should be considered at Bonus
        val s_valid = Input(Bool())
    })

    io.puzzle := VecInit(Seq.fill(4)(0.U(4.W)))
    io.ready  := false.B
    io.g_valid  := false.B
    io.A      := 0.U
    io.B      := 0.U

    //prng from hw5-3-1
    val prng = Module(new PRNG(seed))
    prng.io.gen := io.gen
    io.ready := prng.io.ready
    io.puzzle := prng.io.puzzle
val A_num = RegInit(0.U(3.W))
    val B_num = RegInit(0.U(3.W))
    
    //sIdle : 等玩家 等出題  
    //sGuess : 開始猜, if A-num = 4 -> 猜對進入sIdel, 猜錯不走
    val sIdle :: sGuess :: Nil = Enum(2)
    val state = RegInit(sIdle)
    when(state === sIdle){
        when(io.ready){state:=sGuess}
    }otherwise{
        // 4A -> 新題目
        when(A_num === 4.U){state := sIdle}
    }
    //把guess變成四個數字
    //
    

    val puzzle_reg = RegInit(VecInit(Seq.fill(4)(0.U(4.W))))
    puzzle_reg := prng.io.puzzle
    val guess_reg = VecInit((0 until 4).map(i => io.guess((4*i)+3, 4*i)))
    when(state === sGuess){
        
        A_num := A_num+1.U
        
    }
    
    val b =VecInit(Seq.fill(4)(false.B))
    b(3) := (guess_reg(3) === puzzle_reg(2)) ||
                (guess_reg(3) === puzzle_reg(1)) ||
                (guess_reg(3) === puzzle_reg(0))

    b(2) := (guess_reg(2) === puzzle_reg(3)) ||
                (guess_reg(2) === puzzle_reg(1)) ||
                (guess_reg(2) === puzzle_reg(0))

    b(1) := (guess_reg(1) === puzzle_reg(3)) ||
                (guess_reg(1) === puzzle_reg(2)) ||
                (guess_reg(1) === puzzle_reg(0))

    b(0) := (guess_reg(0) === puzzle_reg(3)) ||
                (guess_reg(0) === puzzle_reg(2)) ||
                (guess_reg(0) === puzzle_reg(1))

    io.B := b(0).asUInt+b(1).asUInt+b(2).asUInt+b(3).asUInt
}
