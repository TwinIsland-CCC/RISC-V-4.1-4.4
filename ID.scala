
package riscv64

import chisel3._
import chisel3.util._

object defs {
  val InstW = 32.W
  val DataW = 64.W
  val AddrW = 64.W
  val RegW = 64.W
  val RegAddrW = 5.W
  val OpcodeW = 7.W
  val ZeroWord = 5.U
  val ResetAddr = 0x80000000L U 64.W

  object OpcodeType {
    val EBREAK = "b1110011".U(OpcodeW)
    val I = "b0010011".U(OpcodeW)
    val B = "b1100011".U(OpcodeW)
    val R = "b0110011".U(OpcodeW)
    val IW = "b0011011".U(OpcodeW)
    val RW = "b0111011".U(OpcodeW)
    val JAL = "b1101111".U(OpcodeW)
    val JALR = "b1100111".U(OpcodeW)
    val S = "b0100011".U(OpcodeW)
    val L = "b0000011".U(OpcodeW)
    val LUI = "b0110111".U(OpcodeW)
    val AUIPC = "b0010111".U(OpcodeW)
  }

  object Funct3 {
    // I-type
    val addi = "b000".U(3.W)
    val slti = "b010".U(3.W)
    val sltiu = "b011".U(3.W)
    val xori = "b100".U(3.W)
    val ori = "b110".U(3.W)
    val andi = "b111".U(3.W)
    val slli = "b001".U(3.W)
    val srlai = "b101".U(3.W)

    // R-type
    val add = "b000".U(3.W)
    val sll = "b001".U(3.W)
    val slt = "b010".U(3.W)
    val sltu = "b011".U(3.W)
    val xor = "b100".U(3.W)
    val srla = "b101".U(3.W)
    val or = "b110".U(3.W)
    val and = "b111".U(3.W)

    // B-Type
    val beq = "b000".U(3.W)
    val bne = "b001".U(3.W)
    val blt = "b100".U(3.W)
    val bge = "b101".U(3.W)
    val bltu = "b110".U(3.W)
    val bgeu = "b111".U(3.W)

  }

  object JID {
    val none = 0.U(2.W)
    val b = 1.U(2.W)
    val jal = 2.U(2.W)
    val jalr = 3.U(2.W)
  }

  val funct7v = "b0100000".U(7.W)
} 

class ID2EXio extends Bundle {
  val isWriteReg = Output(Bool())
  val isJump = Output(UInt(2.W))
  val isLoad = Output(Bool())
  val isStore = Output(Bool())
  val isDiv = Output(Bool())
  val isWord = Output(Bool())
  val hasImm = Output(Bool())
  val funct3 = Output(UInt(3.W))
  val rs = Output(Vec(2, UInt(defs.RegAddrW)))
  val rd = Output(UInt(defs.RegAddrW))
  val Imm = Output(SInt(defs.RegW))
  val funct7 = Input(UInt(7.W))
  val pc = Output(UInt(defs.RegW))
}

object ID {
  def apply() = new ID
}
// 纯组合逻辑
class ID extends Module {
  val io = IO(new Bundle {
    val inst = Flipped(Valid(UInt(defs.InstW)))
    val pc = UInt(defs.AddrW)
    val toEX = new ID2EXio()
  })
  val opcode = Wire(UInt(defs.OpcodeW))
  val rd = Wire(UInt(defs.RegAddrW))
  val rs = Wire(Vec(2, UInt(defs.RegAddrW)))
  val inst = Wire(UInt(defs.InstW))
  val immI = Wire(SInt(64.W))
  val immS = Wire(SInt(64.W))
  val immU = Wire(SInt(64.W))
  val immB = Wire(SInt(64.W))
  val immJ = Wire(SInt(64.W))
  val funct3 = Wire(UInt(3.W))
  val funct7 = Wire(UInt(7.W))

  inst := io.inst.bits
  opcode := inst(6, 0)
  rd := inst(11, 7)
  rs := Seq(inst(19, 15), inst(24, 20))
  funct3 := inst(14, 12)
  funct7 := inst(31, 25)

  immI := inst(31, 20).asSInt
  immS := Cat(inst(31, 25), inst(11, 7)).asSInt
  immU := Cat(inst(31, 12), 0 U 12.W).asSInt
  immB := Cat(inst(31), inst(7), inst(30, 25), inst(11, 8), 0 U 1.W).asSInt
  immJ := Cat(inst(31), inst(19, 12), inst(20), inst(30, 21), 0 U 1.W).asSInt


  // init toEX io
  io.toEX.isWriteReg := false.B
  io.toEX.isJump := 0.U
  io.toEX.isLoad := false.B
  io.toEX.isStore := false.B
  io.toEX.isDiv := false.B
  io.toEX.isWord := false.B
  io.toEX.hasImm := 0.U
  io.toEX.funct3 := funct3
  io.toEX.rs := Seq(0.U, 0.U)
  io.toEX.rd := 0.U
  io.toEX.Imm := 0.S
  io.toEX.funct7 := funct7
  io.toEX.pc := io.pc

  when(io.inst.valid) {
    printf(p"op:${Binary(opcode)} funct3:${Binary(funct3)}\n")
    switch(opcode) {
      is(defs.OpcodeType.I) {
        printf(p"opcode:$opcode(I)\n")
        io.toEX.isWriteReg := true.B
        io.toEX.hasImm := true.B
        io.toEX.Imm := Mux(funct3 === "b101".U, Cat(0.U(1.W), inst(25, 20)).asSInt, immI)
        io.toEX.rs := Seq(rs(0), 0.U)
        io.toEX.rd := rd
      }
      is(defs.OpcodeType.R) {
        printf(p"opcode:$opcode(R)\n")
        io.toEX.isWriteReg := true.B
        io.toEX.rs := rs
        io.toEX.rd := rd
      }
      is(defs.OpcodeType.IW) {
        printf(p"opcode:$opcode(IW)\n")
        io.toEX.isWriteReg := true.B
        io.toEX.isWord := true.B
        io.toEX.hasImm := true.B
        io.toEX.Imm := Mux(funct3 === "b101".U, Cat(0.U(1.W), inst(25, 20)).asSInt, immI)
        io.toEX.rs := Seq(rs(0), 0.U)
        io.toEX.rd := rd
      }
      is(defs.OpcodeType.RW) {
        printf(p"opcode:$opcode(RW)\n")
        io.toEX.isWriteReg := true.B
        io.toEX.isWord := true.B
        io.toEX.rs := rs
        io.toEX.rd := rd
      }
      is(defs.OpcodeType.B) {
        printf(p"opcode:$opcode(B)\n")
        io.toEX.isJump := defs.JID.b
        io.toEX.rs := rs
        io.toEX.hasImm := true.B
        io.toEX.Imm := immB
      }
      is(defs.OpcodeType.JAL) {
        printf(p"opcode:$opcode(JAL)\n")
        io.toEX.isWriteReg := true.B
        io.toEX.isJump := defs.JID.jal
        io.toEX.hasImm := true.B
        io.toEX.rd := rd
        io.toEX.Imm := immJ
      }
      is(defs.OpcodeType.JALR) {
        printf(p"opcode:$opcode(JALR)\n")
        io.toEX.isWriteReg := true.B
        io.toEX.isJump := defs.JID.jalr
        io.toEX.hasImm := true.B
        io.toEX.rs := Seq(rs(0), 0.U)
        io.toEX.rd := rd
        io.toEX.Imm := immI
      }
      is(defs.OpcodeType.LUI) {
        printf(p"opcode:$opcode(LUI)\n")
        io.toEX.isWriteReg := true.B
        io.toEX.hasImm := true.B
        io.toEX.funct3 := "b000".U(3.W) // translate lui to addi
        io.toEX.Imm := immU
        io.toEX.rs := Seq(0.U, 0.U)
        io.toEX.rd := rd
      }
      is(defs.OpcodeType.AUIPC) {
        printf(p"opcode:$opcode(AUIPC)\n")
        io.toEX.isWriteReg := true.B
        io.toEX.hasImm := true.B
        io.toEX.funct3 := "b000".U(3.W) // translate auipc to addi
        io.toEX.Imm := immU + io.pc.asSInt
        io.toEX.rs := Seq(0.U, 0.U)
        io.toEX.rd := rd
      }
    }
  }
}