package acal_lab05.Hw3

import chisel3._
import chisel3.util._

class PRNGs(seed:Int) extends Module{
    val io = IO(new Bundle{
        val gen = Input(Bool())
        val puzzle = Output(Vec(4,UInt(4.W)))
        val ready = Output(Bool())
    })

    io.puzzle := VecInit(Seq.fill(4)(0.U(4.W)))
    io.ready := false.B

    //lfsr
    val shiftReg = RegInit(VecInit(Seq.fill(16)(true.B))) 
    //shiftReg(2):=false.B
    //Barrel Shift Register
    (shiftReg.zipWithIndex).map{
        case(sr,i) => sr := shiftReg((i+1)%16)
    }
    //Fibonacci LFSR
    shiftReg(15) := (Seq(14,13,11).map(x=>shiftReg(16-x)).reduce(_^_)) ^ shiftReg(0)
    
    //擷取數字然後判斷是否大於十，是的話%10，接著判斷有無重複，有則再擷取一次shiftreg
    val LFSR_four = VecInit(Seq.fill(4)(0.U(4.W)))
	LFSR_four.zipWithIndex.map{case(p,j)=>p:=Cat(shiftReg(j*4),shiftReg(j*4+1),shiftReg(j*4+2),shiftReg(j*4+3))}
    
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
    
	//val output_reg = RegInit(VecInit(Seq.fill(4)(0.U(4.W))))
	//output_reg:=
	
    val sIdle :: sGen  :: sReady :: Nil = Enum(4)
    val state = RegInit(sIdle)
    //切狀態的條件
    switch(state){
        is(sIdle){
          when(io.gen){state := sGen}
        }
        is(sGen){//generator
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
		//讓lfsr跑，並且抓下來站存區
            //r.zipWithIndex.map{case(p,j)=>p:=Cat(shiftReg(j*4),shiftReg(j*4+1),shiftReg(j*4+2),shiftReg(j*4+3))}

        }
        is(sGen){
		//
            
        }
        is(sReady){
            io.puzzle:=RegNext(LFSR_minus10)
            io.ready:=true.B    
        }
    }
/*
    when(io.gen ){//close| noo(2)
        //(io.puzzle.zipWithIndex).map{case(p,i)=>p:=shiftReg(Seq(i,i+1,i+2,i+3))}//
            noo(1):=false.B
            noo(2):=false.B
            //check:=true.B
            //noo:=true.B
            //println(r(1)(2))
            //for(i <- 0 until 4){
                //for(j<- 0 until 4){
                    //r(i)(j):=shiftReg(4*i+j)
                //}           
            r.zipWithIndex.map{case(p,j)=>p:=Cat(shiftReg(j*4),shiftReg(j*4+1),shiftReg(j*4+2),shiftReg(j*4+3))}
            
            for(i <- 0 until 4){
                when(r(i)>9.U){
                    r(i):=r(i)-10.U
                }//.elsewhen(i.asUInt===3.U){noo(1):=true.B}
            }
            
            for(i <- 0 until 3){           
                when(r.count{x:UInt=>x===r(i).asUInt}>1.U){
                    noo(2):=true.B
                }.elsewhen(i.asUInt===2.U){
                    noo(2):=false.B
                    noo(1):=true.B
                }
            }
            
        
    }
   // 0
    //1 must 0, 0:1:看有沒有重複
    //2 start must 0, 1:繼續抓數字 0:停止抓數字
    when(noo(1)){
        io.ready:=true.B    
        io.puzzle:=r
    //noo(1):=false.B
        //noo:=true.B
    }
    */
}
