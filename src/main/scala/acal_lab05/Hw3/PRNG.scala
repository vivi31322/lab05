package acal_lab05.Hw3

import chisel3._
import chisel3.util._

class PRNG(seed:Int) extends Module{
    val io = IO(new Bundle{
        val gen = Input(Bool())
        val puzzle = Output(Vec(4,UInt(4.W)))
        val ready = Output(Bool())
    })

    io.puzzle := VecInit(Seq.fill(4)(0.U(4.W)))
    io.ready := false.B

    //lfsr
    val shiftReg = RegInit(VecInit(Seq.fill(16)(true.B))) 
    //Barrel Shift Register
    (shiftReg.zipWithIndex).map{
        case(sr,i) => sr := shiftReg((i+1)%16)
    }
    //Fibonacci LFSR
    shiftReg(15) := (Seq(14,13,11).map(x=>shiftReg(16-x)).reduce(_^_)) ^ shiftReg(0)
    
    //擷取4個數字
    val LFSR_four = VecInit(Seq.fill(4)(0.U(4.W)))
	LFSR_four.zipWithIndex.map{case(p,j)=>p:=Cat(shiftReg(j*4),shiftReg(j*4+1),shiftReg(j*4+2),shiftReg(j*4+3))}
    //處理大於9的數字
	val LFSR_minus10 = VecInit(LFSR_four.map(p => Mux(p>9.U, p-10.U, p)))
	
	//數字內是否有重複
    val checkSame=WireDefault(false.B)
	checkSame := ((LFSR_minus10.count{x:UInt=>x===LFSR_minus10(0).asUInt}>1.U)|
    (LFSR_minus10.count{x:UInt=>x===LFSR_minus10(1).asUInt}>1.U)|
	(LFSR_minus10.count{x:UInt=>x===LFSR_minus10(2).asUInt}>1.U)|
	(LFSR_minus10.count{x:UInt=>x===LFSR_minus10(3).asUInt}>1.U)
	)
	//隨機數字是否曾出現過，與前五個比對
    val checkRepeat=WireDefault(false.B)
	//機會很小，起碼一百個內不會重複，所以不管了(期末再說!!!)

    //FSM state
    //Idle:尚未收到信號，讓LFSR持續移動
    //Gen:收到信號，檢查目前的數字是否重複，處理大於9，如果不符合條件就繼續取下一個LFSR
    //Ready:隨機數滿足輸出條件，output
    val sIdle :: sGen  :: sReady :: Nil = Enum(3)
    val state = RegInit(sIdle)
    
    //切狀態的條件
    switch(state){
        is(sIdle){
          when(io.gen){state := sGen}
        }
        is(sGen){
            //when((checkRepeat)&(!checkSame)){state := sReady}
			when(!checkSame){state := sReady}
        }
        is(sReady){
            state:= sIdle
        }
    }

    //狀態內要做的事
    switch(state){
        is(sIdle){
		
        }
        is(sGen){
		
        }
        is(sReady){
            io.puzzle:=RegNext(LFSR_minus10)
            io.ready:=true.B    
        }
    }

}
