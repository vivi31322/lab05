package acal_lab05.Hw1

import chisel3._
import chisel3.util._

class TrafficLight_p(Ytime:Int, Gtime:Int, Ptime:Int) extends Module{
  val io = IO(new Bundle{
    val P_button = Input(Bool())
    val H_traffic = Output(UInt(2.W))
    val V_traffic = Output(UInt(2.W))
    val P_traffic = Output(UInt(2.W))
    val timer     = Output(UInt(5.W))
  })

  //parameter declaration
  val Off = 0.U
  val Red = 1.U
  val Yellow = 2.U
  val Green = 3.U
  
  val sIdle :: sHGVR :: sHYVR :: sHRVG :: sHRVY :: sPG ::Nil = Enum(6)

  //State register
  val state = RegInit(sIdle)
  /************************************ 
  * pre_state: 按行人按鈕前的狀態   
  * Pmode: 分辨 sPG 是自然循環(Pmode = false)還是按鈕造成(Pmode = true)
  * 因為 counter 是用上一個 cntMode 判斷下一個 cntMode 要數幾秒
  * 行人狀態(sPG)結束可能接 sHGVR or 被打斷的state，所以需要分開處理 
  *************************************/
  val pre_state = RegInit(sIdle)
  val Pmode = RegInit(0.B)

  //Counter============================
  val cntMode = WireDefault(0.U(2.W))
  val cntReg = RegInit(0.U(4.W))
  val cntDone = Wire(Bool())
  cntDone := (cntReg === 0.U)

  when(cntDone){
    
    when(io.P_button){
	//如果是按按鈕觸發 cntDone，進入sPG
		cntReg := (Ptime-1).U
	}.elsewhen(cntMode === 0.U){
	//黃燈後，根據現在狀態決定進入"對向綠燈"或是"sPG"
		when(state === sHRVY){
			cntReg := (Ptime-1).U
		}.otherwise{
			cntReg := (Gtime-1).U
		}
	}.elsewhen(cntMode === 1.U){
	//綠燈後，接黃燈
		cntReg := (Ytime-1).U
	}.elsewhen(cntMode === 2.U){
	//sPG後，自然循環下接綠燈
		cntReg := (Gtime-1).U
	}
	
  }.otherwise{
      cntReg := cntReg - 1.U
  }
  //Counter end========================


//Next State Decoder
when(io.P_button && state=/=sPG){
	//遭按鈕打斷，紀錄當前狀態並強制進入sPG
	//標示 sPG 為按鈕造成，強制歸零倒數
	pre_state := state
	Pmode := 1.B
	cntDone := 1.B
	state := sPG

}.otherwise{

	switch(state){
		is(sIdle){
		  state := sHGVR
		}
		is(sHGVR){
		  when(cntDone) {state := sHYVR}
		}
		is(sHYVR){
		  when(cntDone) {state := sHRVG}
		}
		is(sHRVG){
		  when(cntDone) {state := sHRVY}
		}
		is(sHRVY){
		  when(cntDone) {state := sPG}
		}
		is(sPG){
			when(cntDone){
			//根據Pmode判斷是否讀取pre_state作為下一個狀態
				when(Pmode){
					state := pre_state
					//歸零 Pmode 以便下次使用
					pre_state := sIdle
					Pmode := 0.B
				}.otherwise{
					state := sHGVR
				}
			
			}
		}
	}
}
  //Output Decoder
  //Default statement
  cntMode := 0.U
  io.H_traffic := Off
  io.V_traffic := Off
  io.P_traffic := Off

  switch(state){
    is(sHGVR){
      cntMode := 1.U
      io.H_traffic := Green
      io.V_traffic := Red
	  io.P_traffic := Red
    }
    is(sHYVR){
      cntMode := 0.U
      io.H_traffic := Yellow
      io.V_traffic := Red
	  io.P_traffic := Red
    }
    is(sHRVG){
      cntMode := 1.U
      io.H_traffic := Red
      io.V_traffic := Green
	  io.P_traffic := Red
    }
    is(sHRVY){
      cntMode := 0.U
      io.H_traffic := Red
      io.V_traffic := Yellow
	  io.P_traffic := Red
    }
	is(sPG){
	  cntMode := 2.U
	  io.H_traffic := Red
      io.V_traffic := Red
	  io.P_traffic := Green
	}
  }

  io.timer := cntReg
}