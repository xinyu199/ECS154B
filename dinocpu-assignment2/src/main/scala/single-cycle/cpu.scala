// This file is where all of the CPU components are assembled into the whole CPU

package dinocpu

import chisel3._
import chisel3.util._
import dinocpu.components._

/**
 * The main CPU definition that hooks up all of the other components.
 *
 * For more information, see section 4.4 of Patterson and Hennessy
 * This follows figure 4.21
 */
class SingleCycleCPU(implicit val conf: CPUConfig) extends BaseCPU {
  // All of the structures required
  val pc              = dontTouch(RegInit(0.U(64.W)))
  val control         = Module(new Control())
  val registers       = Module(new RegisterFile())
  val aluControl      = Module(new ALUControl())
  val alu             = Module(new ALU())
  val immGen          = Module(new ImmediateGenerator())
  val controlTransfer = Module(new ControlTransferUnit())
  val (cycleCount, _) = Counter(true.B, 1 << 30)


  //control.io := DontCare
  //registers.io := DontCare
  //aluControl.io := DontCare
  //alu.io := DontCare
  //immGen.io := DontCare
  //controlTransfer.io := DontCare
  //io.dmem <> DontCare
  //immGen.io <> DontCare
  //controlTransfer.io <> DontCare
  //io.dmem <> DontCare

  //FETCH
  io.imem.address := pc
  io.imem.valid := true.B

  val instruction = Wire(UInt(32.W))
  when ((pc % 8.U) === 4.U) {
    instruction := io.imem.instruction(63, 32)
  } .otherwise {
    instruction := io.imem.instruction(31, 0)
  }
  
  // Your code goes here
  control.io.opcode := instruction(6,0)

  immGen.io.instruction := instruction
  
  registers.io.readreg1 := instruction(19,15)
  registers.io.readreg2 := instruction(24,20)
  registers.io.writereg := instruction(11,7)
  registers.io.writedata := MuxCase(0.U, Array((control.io.writeback_src === 0.U) -> alu.io.result, (control.io.writeback_src === 1.U) -> immGen.io.sextImm, (control.io.writeback_src === 2.U) -> io.dmem.readdata))
  registers.io.wen := (registers.io.writereg =/= 0.U) & (control.io.writeback_valid === 1.U)

  controlTransfer.io.controltransferop := control.io.controltransferop
  controlTransfer.io.operand1 := registers.io.readdata1
  controlTransfer.io.operand2 := alu.io.operand2
  controlTransfer.io.funct3 := instruction(14,12)
  controlTransfer.io.pc := pc
  controlTransfer.io.imm := immGen.io.sextImm

  aluControl.io.aluop := control.io.aluop 
  aluControl.io.funct3 := instruction(14,12)
  aluControl.io.funct7 := instruction(31,25)

  io.dmem.address := alu.io.result
  io.dmem.valid := control.io.memop =/= 0.U
  io.dmem.memread := control.io.memop === 1.U
  io.dmem.memwrite := control.io.memop === 2.U
  io.dmem.writedata := registers.io.readdata2
  io.dmem.maskmode := instruction(13,12) //funct3(1,0)
  io.dmem.sext := ~instruction(14) //~funct3(2)

  alu.io.operation := aluControl.io.operation
  alu.io.operand1 := MuxCase(0.U, Array((control.io.op1_src === 0.U) -> registers.io.readdata1, (control.io.op1_src === 1.U) -> pc))
  alu.io.operand2 := MuxCase(0.U, Array((control.io.op2_src === 0.U) -> registers.io.readdata2, (control.io.op2_src === 1.U) -> 4.U, (control.io.op2_src === 2.U) -> immGen.io.sextImm))

  when(control.io.controltransferop === 0.U){
    pc := pc + 4.U
  }
  .otherwise{
    pc := controlTransfer.io.nextpc
  }
}

/*
 * Object to make it easier to print information about the CPU
 */
object SingleCycleCPUInfo {
  def getModules(): List[String] = {
    List(
      "dmem",
      "imem",
      "control",
      "registers",
      "csr",
      "aluControl",
      "alu",
      "immGen",
      "controlTransfer"
    )
  }
}
