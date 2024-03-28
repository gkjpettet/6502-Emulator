#tag Class
Protected Class CPU
	#tag Method, Flags = &h21
		Private Sub AND_(addressMode As MOS6502.AddressModes)
		  /// AND - "AND" Memory with Accumulator
		  ///
		  /// Operation: A ∧ M → A
		  ///
		  /// Transfers the accumulator and memory to the adder which performs a bit-by-bit AND operation 
		  /// and stores the result back in the accumulator.
		  ///
		  /// Affects the accumulator.
		  /// Sets the zero flag if the result in the accumulator is 0, otherwise resets the zero flag.
		  /// Sets the negative flag if the result in the accumulator has bit 7 on, otherwise resets it.
		  
		  // Get the data.
		  Var data As UInt8
		  If addressMode = AddressModes.Immediate Then
		    data = FetchByte
		  Else
		    data = Memory(EffectiveAddress(addressMode))
		  End If
		  
		  // Do the operation.
		  A = A And data
		  
		  // Set flags.
		  ZeroFlag = (A = 0)
		  NegativeFlag = ((A And &h80) <> 0)
		  
		  // Update the cycles.
		  Select Case addressMode
		  Case AddressModes.Immediate
		    TotalCycles = TotalCycles + 2
		    
		  Case AddressModes.Absolute
		    TotalCycles = TotalCycles + 4
		    
		  Case AddressModes.XIndexedAbsolute
		    TotalCycles = TotalCycles + 4 + If(CrossedPageBoundary, 1, 0)
		    
		  Case AddressModes.YIndexedAbsolute
		    TotalCycles = TotalCycles + 4 + If(CrossedPageBoundary, 1, 0)
		    
		  Case AddressModes.ZeroPage
		    TotalCycles = TotalCycles + 3
		    
		  Case AddressModes.XIndexedZeroPage
		    TotalCycles = TotalCycles + 4
		    
		  Case AddressModes.XIndexedZeroPageIndirect
		    TotalCycles = TotalCycles + 6
		    
		  Case AddressModes.ZeroPageIndirectYIndexed
		    TotalCycles = TotalCycles + 5 + If(CrossedPageBoundary, 1, 0)
		  End Select
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h21, Description = 41726974686D65746963207368696674206C6566742E20536869667473206569746865722074686520616363756D756C61746F72206F72207468652061646472657373206D656D6F7279206C6F636174696F6E20312062697420746F20746865206C6566742C20776974682074686520626974203020616C77617973206265696E672073657420746F203020616E64207468652074686520696E707574206269742037206265696E672073746F72656420696E2074686520636172727920666C61672E
		Private Sub ASL(addressMode As MOS6502.AddressModes)
		  /// Arithmetic shift left.
		  /// Shifts either the accumulator or the address memory location 1 bit to the left, with the 
		  /// bit 0 always being set to 0 and the the input bit 7 being stored in the carry flag.
		  ///
		  /// Operation: C ← /M7...M0/ ← 0
		  ///
		  /// ASL either shifts the accumulator left 1 bit or is a read/modify/write instruction that 
		  /// affects only memory.
		  /// The instruction does not affect the overflow bit.
		  /// Sets N equal to the result bit 7 (bit 6 in the input).
		  /// Sets Z flag if the result is equal to 0, otherwise resets Z and stores the input bit 7 in 
		  /// the carry flag.
		  
		  // Get the data to work on.
		  Var address As UInt16
		  Var data As UInt8
		  If addressMode <> Addressmodes.Accumulator Then
		    address = EffectiveAddress(addressMode)
		    data = Memory(address)
		  Else
		    data = A
		  End If
		  
		  // Compute the result.
		  Var result As UInt8 = ShiftLeft(data, 1)
		  
		  // Store the result back where it came from.
		  If addressMode = AddressModes.Accumulator Then
		    A = result
		  Else
		    Memory(address) = result
		  End If
		  
		  // Set the flags.
		  CarryFlag = (data And &h80) <> 0
		  NegativeFlag = (data And &h40) <> 0
		  ZeroFlag = (result = 0)
		  
		  // How many cycles?
		  Select Case addressMode
		  Case AddressModes.Accumulator
		    TotalCycles = TotalCycles + 2
		    
		  Case AddressModes.Absolute
		    TotalCycles = TotalCycles + 6
		    
		  Case AddressModes.XIndexedAbsolute
		    TotalCycles = TotalCycles + 7
		    
		  Case AddressModes.ZeroPage
		    TotalCycles = TotalCycles + 5
		    
		  Case AddressModes.XIndexedZeroPage
		    TotalCycles = TotalCycles + 6
		  End Select
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h21
		Private Sub BIT(addressMode As MOS6502.AddressModes)
		  /// BIT - Test Bits in Memory with Accumulator
		  ///
		  /// Operation: A ∧ M, M7 → N, M6 → V
		  ///
		  /// Performs an AND between a memory location and the accumulator but does not store the result 
		  /// of the AND into the accumulator.
		  ///
		  /// Affects the N flag with N being set to the value of bit 7 of the memory being tested.
		  /// Affects the V flag with V being set equal to bit 6 of the memory being tested
		  /// Z is set by the result of the AND operation between the accumulator and the memory if the 
		  /// result is Zero, Z is reset otherwise. 
		  /// It does not affect the accumulator.
		  
		  // Get the data to test against.
		  Var data As UInt8 = Memory(EffectiveAddress(addressMode))
		  
		  NegativeFlag = (data And &b10000000) <> 0
		  OverflowFlag = (data And &b01000000) <> 0
		  ZeroFlag = (A And Data) = 0
		  
		  // How many cycles?
		  Select Case addressMode
		  Case AddressModes.Absolute
		    TotalCycles = TotalCycles + 4
		  Case AddressModes.ZeroPage
		    TotalCycles = TotalCycles + 3
		  End Select
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h21, Description = 424D49202D204272616E6368206F6E20526573756C74204D696E7573
		Private Sub BMI()
		  /// BMI - Branch on Result Minus
		  ///
		  /// Operation: Branch on N = 1
		  /// 
		  /// Takes the conditional branch if the N bit is set.
		  /// Does not affect any of the flags or any other part of the machine other than the program 
		  /// counter and then only if the N bit is on.
		  
		  Var targetAddress As UInt16 = EffectiveAddress(AddressModes.Relative)
		  
		  If NegativeFlag Then
		    PC = targetAddress
		    TotalCycles = TotalCycles + 3 + If(CrossedPageBoundary, 1, 0)
		  Else
		    TotalCycles = TotalCycles + 2 + If(CrossedPageBoundary, 1, 0)
		  End If
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h21, Description = 42504C202D204272616E6368206F6E20526573756C7420506C75732E
		Private Sub BPL()
		  /// BPL - Branch on Result Plus.
		  /// 
		  /// Operation: Branch on N = 0
		  ///
		  /// This instruction is the complementary branch to branch on result minus. 
		  /// It's a conditional branch which takes the branch when the N bit is reset (0). 
		  /// BPL is used to test if the previous result bit 7 was off (0) and branch on result minus is used 
		  /// to determine if the previous result was minus or bit 7 was on (1).
		  ///
		  /// The instruction affects no flags or other registers other than the P counter and only affects 
		  /// the P counter when the N bit is reset.
		  
		  Var targetAddress As UInt16 = EffectiveAddress(AddressModes.Relative)
		  
		  If Not NegativeFlag Then
		    PC = targetAddress
		    TotalCycles = TotalCycles + 3 + If(CrossedPageBoundary, 1, 0)
		  Else
		    TotalCycles = TotalCycles + 2 + If(CrossedPageBoundary, 1, 0)
		  End If
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h21, Description = 4578656375746573207468652042524B206F70636F64652C2072657475726E696E6720746865206E756D626572206F66206379636C65732074616B656E2E
		Private Sub BRK()
		  /// Executes the BRK opcode, returning the number of cycles taken.
		  
		  // Push the program counter + 1 to the stack.
		  PushWord(PC + 1)
		  
		  // Push the status to the stack, setting the break flag of the saved byte to 1 (bit 4).
		  PushByte(P Or &h10)
		  
		  // Set the interrupt disable flag (bit 2).
		  P = P Or &h04
		  
		  // Load the interrupt vector from memory locations &hFFFE and &hFFFF
		  PC = ShiftLeft(Memory.Read(&hFFFF), 8) Or Memory.Read(&hFFFE)
		  
		  TotalCycles = TotalCycles + 7
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Function Clone() As MOS6502.CPU
		  /// Returns a clone of this CPU. Mostly used for debugging whilst testing.
		  
		  Var cpu As New MOS6502.CPU(Memory)
		  
		  cpu.A = A
		  cpu.Halted = Halted
		  cpu.P = P
		  cpu.PC = PC
		  cpu.SP = SP
		  cpu.TotalCycles = TotalCycles
		  cpu.X = X
		  cpu.Y = Y
		  
		  Return cpu
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub Constructor(mem As MOS6502.Memory)
		  Self.Memory = mem
		  Reset
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h21, Description = 52657475726E732074686520656666656374697665206164647265737320676976656E20616E2061646472657373206D6F64652E20446F206E6F74207573652074686973206D6574686F64206966207468652061646472657373206D6F646520697320416363756D756C61746F722E2055706461746573206050436020616E64206043726F7373656450616765426F756E64617279602E
		Private Function EffectiveAddress(addressMode As MOS6502.AddressModes) As UInt16
		  /// Returns the effective address given an address mode.
		  /// Do not use this method if the address mode is Accumulator.
		  /// Updates `PC` and `CrossedPageBoundary`.
		  
		  CrossedPageBoundary = False
		  
		  Select Case addressMode
		  Case AddressModes.Absolute
		    Var lsb As UInt8 = FetchByte
		    Return FetchByte * 256 + lsb
		    
		  Case AddressModes.Relative
		    #Pragma Warning "TODO: Figure out page boundary crossing"
		    Var offset As Int8 = FetchByte // NB: Signed byte.
		    Return PC + offset
		    
		  Case AddressModes.XIndexedAbsolute
		    #Pragma Warning "TODO: Figure out page boundary crossing"
		    Var baseLSB As UInt8 = FetchByte
		    Return CType(FetchByte, UInt16) * 256 + baseLSB + X
		    
		  Case AddressModes.XIndexedZeroPage
		    Return FetchByte + X
		    
		  Case AddressModes.XIndexedZeroPageIndirect
		    Var lowAddress As UInt16 = (X + FetchByte) And &hFF // Constrain to 8-bits.
		    Var highAddress As UInt16 = (lowAddress + 1) And &hFF // Constrain to 8-bits.
		    Var lowAddressByte As UInt8 = Memory(lowAddress)
		    Var highAddressByte As UInt8 = Memory(highAddress)
		    Var addressByte As UInt16 = ShiftLeft(highAddressByte, 8) Or lowAddressByte
		    Return addressByte
		    
		  Case AddressModes.YIndexedAbsolute
		    #Pragma Warning "TODO: Figure out page boundary crossing"
		    Var baseLSB As UInt8 = FetchByte
		    Var baseAddress As UInt16 = CType(FetchByte, UInt16) * 256 + baseLSB + Y
		    Return baseAddress
		    
		  Case AddressModes.ZeroPage
		    Return FetchByte
		    
		  Case AddressModes.ZeroPageIndirectYIndexed
		    Var zeroPageAddress As UInt8 = FetchByte
		    Var zeroPageContents As UInt8 = Memory(zeroPageAddress)
		    Var lsb As UInt16 = zeroPageContents + Y
		    Var carry As Integer = 0
		    If lsb < zeroPageContents Then
		      carry = 1
		      CrossedPageBoundary = True
		    End If
		    
		    Var msbAddress As UInt8 = zeroPageAddress + 1
		    Var msb As UInt16 = Memory(msbAddress) + carry
		    Var actualAddress As UInt16 = (msb * 256) + lsb
		    Return actualAddress
		    
		  Else
		    Raise New UnsupportedOperationException("Unsupported address mode.")
		  End Select
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0, Description = 457865637574657320746865206E65787420696E737472756374696F6E2E205468697320697320612073696E676C652066657463682F6465636F64652F6578656375746520737465702E
		Sub Execute()
		  /// Executes the next instruction. 
		  /// This is a single fetch/decode/execute step.
		  
		  If Not Halted Then
		    ExecuteInstruction(FetchByte)
		  End If
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h21, Description = 457865637574657320606F70636F64656020616E642072657475726E7320746865206E756D626572206F66206379636C657320697420746F6F6B2E
		Private Sub ExecuteInstruction(opcode As UInt8)
		  /// Executes `opcode` and returns the number of cycles it took.
		  
		  Select Case opcode
		  Case &h00 // BRK
		    BRK
		    
		  Case &h01 // ORA ($nn,X)
		    ORA(AddressModes.XIndexedZeroPageIndirect)
		    
		  Case &h05 // ORA $nn
		    ORA(AddressModes.ZeroPage)
		    
		  Case &h06 // ASL $nn
		    ASL(AddressModes.ZeroPage)
		    
		  Case &h08 // PHP
		    PHP
		    
		  Case &h09 // ORA #$nn
		    ORA(AddressModes.Immediate)
		    
		  Case &h0A // ASL A
		    ASL(AddressModes.Accumulator)
		    
		  Case &h0D // ORA $nnnn
		    ORA(AddressModes.Absolute)
		    
		  Case &h0E // ASL $nnnn
		    ASL(AddressModes.Absolute)
		    
		  Case &h10 // BPL
		    BPL
		    
		  Case &h11 // ORA ($nn),Y
		    ORA(AddressModes.ZeroPageIndirectYIndexed)
		    
		  Case &h15 // ORA $nn,X
		    ORA(AddressModes.XIndexedZeroPage)
		    
		  Case &h16 // ASL $nn,X
		    ASL(AddressModes.XIndexedZeroPage)
		    
		  Case &h18 // CLC
		    CarryFlag = False
		    TotalCycles = TotalCycles + 2
		    
		  Case &h19 // ORA $nnnn,Y
		    ORA(AddressModes.YIndexedAbsolute)
		    
		  Case &h1D // ORA $nnnn,X
		    ORA(AddressModes.XIndexedAbsolute)
		    
		  Case &h1E // ASL $nnnn,X
		    ASL(AddressModes.XIndexedAbsolute)
		    
		  Case &h20 // JSR
		    JSR
		    
		  Case &h21 // AND ($nn,X)
		    AND_(AddressModes.XIndexedZeroPageIndirect)
		    
		  Case &h24 // BIT $nn
		    BIT(AddressModes.ZeroPage)
		    
		  Case &h25 // AND $nn
		    AND_(AddressModes.ZeroPage)
		    
		  Case &h26 //ROL $nn
		    ROL(AddressModes.ZeroPage)
		    
		  Case &h28 // PLP
		    PLP
		    
		  Case &h29 // AND #$nn
		    AND_(AddressModes.Immediate)
		    
		  Case &h2A // ROL A
		    ROL(AddressModes.Accumulator)
		    
		  Case &h2C // BIT $nnnn
		    BIT(AddressModes.Absolute)
		    
		  Case &h2D // AND $nnnn
		    AND_(AddressModes.Absolute)
		    
		  Case &h2E // ROL $nnnn
		    ROL(AddressModes.Absolute)
		    
		  Case &h30 // BMI $nnnn
		    BMI
		    
		  Case &h31 // AND ($nn),Y
		    AND_(AddressModes.ZeroPageIndirectYIndexed)
		    
		  Case &h35 // AND $nn,X
		    AND_(AddressModes.XIndexedZeroPage)
		    
		  Case &h36 // ROL $nn,X
		    ROL(AddressModes.XIndexedZeroPage)
		    
		  Case &h38 // SEC
		    CarryFlag = True
		    TotalCycles = TotalCycles + 2
		    
		  Case &h39 // AND $nnnn,Y
		    AND_(AddressModes.YIndexedAbsolute)
		    
		  Case &h3D // AND $nnnn,X
		    AND_(AddressModes.XIndexedAbsolute)
		    
		  Case &h3E // ROL $nnnn,X
		    ROL(AddressModes.XIndexedAbsolute)
		    
		  Case &h40 // RTI
		    RTI
		    
		  Case &h48 // PHA
		    PushByte(A)
		    TotalCycles = TotalCycles + 3
		    
		  Case &h60 // RTS
		    RTS
		    
		  Case &hF8 // SED
		    DecimalFlag = True
		    TotalCycles = TotalCycles + 2
		    
		  Else
		    // Invalid opcode. Halt the CPU.
		    Halted = True
		    Raise New MOS6502.Error("Invalid opcode " + opcode.ToString + ".")
		  End Select
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h21, Description = 4665746368657320746865206E65787420627974652066726F6D206D656D6F7279202861742061646472657373205043292E
		Private Function FetchByte() As UInt8
		  /// Fetches the next byte from memory (at address PC).
		  
		  Var value As UInt8 = Memory.Read(PC)
		  
		  PC = PC + 1
		  
		  Return value
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h21
		Private Sub JSR()
		  /// JSR - Jump To Subroutine
		  ///
		  /// Operation: PC + 2↓, [PC + 1] → PCL, [PC + 2] → PCH
		  ///
		  /// Transfers control of the program counter to a subroutine location but leaves a return pointer 
		  /// on the stack to allow the user to return to perform the next instruction in the main program 
		  /// after the subroutine is complete. To accomplish this, JSR instruction stores the program counter 
		  /// address which points to the last byte of the jump instruc­tion onto the stack using the stack pointer.
		  /// The stack byte contains the program count high first, followed by program count low. 
		  /// The JSR then transfers the addresses following the jump instruction to the program counter low 
		  /// and the program counter high, thereby directing the program to begin at that new address.
		  ///
		  /// The JSR instruction affects no flags, causes the stack pointer to be decremented by 2 and 
		  /// substitutes new values into the program counter low and the program counter high.
		  
		  Var address As UInt16 = EffectiveAddress(AddressModes.Absolute)
		  
		  PushWord(PC)
		  
		  PC = address
		  
		  TotalCycles = TotalCycles + 6
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h21, Description = 546865204F524120696E737472756374696F6E207472616E736665727320746865206D656D6F727920616E642074686520616363756D756C61746F7220746F2074686520616464657220776869636820706572666F726D7320612062696E61727920224F5222206F6E2061206269742D62792D62697420626173697320616E642073746F7265732074686520726573756C7420696E2074686520616363756D756C61746F722E2052657475726E7320746865206E756D626572206F66206379636C65732074616B656E2E
		Private Sub ORA(addressMode As MOS6502.AddressModes)
		  /// The ORA instruction transfers the memory and the accumulator to the adder which performs a 
		  /// binary "OR" on a bit-by-bit basis and stores the result in the accumulator.
		  /// 
		  /// Operation: A ∨ M → A
		  ///
		  /// This instruction affects the accumulator.
		  /// Sets the zero flag if the result in the accumulator is 0, otherwise resets the zero flag
		  /// Sets the negative flag if the result in the accumulator has bit 7 on, otherwise resets the 
		  /// negative flag.
		  
		  // Get the data.
		  Var data As UInt8
		  If addressMode = AddressModes.Immediate Then
		    data = FetchByte
		  Else
		    data = Memory(EffectiveAddress(addressMode))
		  End If
		  
		  A = A Or data
		  
		  ZeroFlag = (A = 0)
		  
		  NegativeFlag = (A And &b10000000) <> 0
		  
		  // How many cycles?
		  Select Case addressMode
		  Case AddressModes.Absolute
		    TotalCycles = TotalCycles + 4
		    
		  Case AddressModes.Immediate
		    TotalCycles = TotalCycles + 2
		    
		  Case AddressModes.XIndexedAbsolute
		    TotalCycles = TotalCycles + 4 + If(CrossedPageBoundary, 1, 0)
		    
		  Case AddressModes.XIndexedZeroPage
		    TotalCycles = TotalCycles + 4
		    
		  Case AddressModes.XIndexedZeroPageIndirect
		    TotalCycles = TotalCycles + 6
		    
		  Case AddressModes.YIndexedAbsolute
		    TotalCycles = TotalCycles + 4 + If(CrossedPageBoundary, 1, 0)
		    
		  Case AddressModes.ZeroPage
		    TotalCycles = TotalCycles + 3
		    
		  Case AddressModes.ZeroPageIndirectYIndexed
		    TotalCycles = TotalCycles + 5 + If(CrossedPageBoundary, 1, 0)
		    
		  Else
		    Raise New UnsupportedOperationException("Unsupported ORA instruction.")
		  End Select
		  
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h21, Description = 504850202D20507573682050726F636573736F7220537461747573204F6E20537461636B2E
		Private Sub PHP()
		  /// PHP - Push Processor Status On Stack.
		  ///
		  /// Operation: P↓
		  /// Transfers the contents of the processor status reg­ister unchanged to the stack, as governed by 
		  /// the stack pointer.
		  /// Affects no registers or flags in the micropro­cessor.
		  
		  PushByte(P)
		  
		  TotalCycles = TotalCycles + 3
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h21, Description = 504C50202D2050756C6C2050726F636573736F72205374617475732046726F6D20537461636B2E
		Private Sub PLP()
		  /// PLP - Pull Processor Status From Stack.
		  ///
		  /// Operation: P↑
		  ///
		  /// Transfers the next value on the stack to the Proces­sor Status register, thereby changing all 
		  /// of the flags and setting the mode switches to the values from the stack.
		  
		  P = PopByte
		  
		  BreakFlag = False
		  SetStatusBit5
		  
		  TotalCycles = TotalCycles + 4
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h21, Description = 506F707320612073696E676C6520627974652066726F6D20746865206D656D6F7279206C6F636174696F6E20706F696E74656420746F2062792074686520737461636B20706F696E746572202853502920616E64207468656E20696E6372656D656E74732074686520737461636B20706F696E7465722E
		Private Function PopByte() As UInt8
		  /// Pops a single byte from the memory location pointed to by the stack pointer (SP) 
		  /// and then increments the stack pointer.
		  
		  SP = SP + 1
		  
		  Var value As UInt8 = Memory.Read(&h0100 + SP)
		  
		  Return value
		End Function
	#tag EndMethod

	#tag Method, Flags = &h21
		Private Function PopWord() As UInt16
		  /// Pops a 16-bit word off the stack.
		  ///
		  /// First pops the low byte then the high byte. This is because the 6502 stack grows downwards
		  /// and the values are stored in little-endian order.
		  
		  Var low As UInt8 = PopByte
		  Var high As UInt8 = PopByte
		  
		  Return (high * 256) + low
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h21, Description = 50757368657320612073696E676C65206279746520746F20746865206D656D6F7279206C6F636174696F6E20706F696E74656420746F2062792074686520737461636B20706F696E746572202853502920616E64207468656E2064656372656D656E74732074686520737461636B20706F696E7465722E
		Private Sub PushByte(value As UInt8)
		  /// Pushes a single byte to the memory location pointed to by the stack pointer (SP) 
		  /// and then decrements the stack pointer.
		  
		  Memory.Write(&h0100 + SP, value)
		  
		  SP = SP - 1
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h21, Description = 50757368657320612031362D62697420776F726420746F2074686520737461636B2E
		Private Sub PushWord(value As UInt16)
		  /// Pushes a 16-bit word to the stack.
		  ///
		  /// First pushes the high byte of the value, followed by the low byte. 
		  /// This is because the 6502 stack grows downward and the pushed values are stored in
		  /// little-endian order.
		  
		  // Push high byte.
		  PushByte(ShiftRight(value, 8) And &hFF)
		  
		  // Push low byte.
		  PushByte(value And &hFF)
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0, Description = 526573657473207468652070726F636573736F722E
		Sub Reset()
		  /// Resets the processor.
		  
		  // Load the reset vector into the program counter.
		  PC = ShiftLeft(Memory(&hFFFD), 8) Or Memory(&hFFFC)
		  
		  // A, X & Y actually retain their previous values but we'll initialise to 0.
		  A = 0
		  X = 0
		  Y = 0
		  
		  // This means that the stack will start at memory address 01FF and grow downwards.
		  SP = &hFF
		  
		  // Status register.
		  // N V - B D I Z C
		  // 0 0 1 1 0 1 0 0
		  P = &h34
		  
		  Halted = False
		  TotalCycles = 0
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h21, Description = 524F4C202D20526F74617465204C6566742E
		Private Sub ROL(addressMode As MOS6502.AddressModes)
		  /// ROL - Rotate Left.
		  ///
		  /// Operation: C ← /M7...M0/ ← C
		  /// 
		  /// Shifts either the accumulator or addressed memory left 1 bit, with the input carry being 
		  /// stored in bit 0 and with the input bit 7 being stored in the carry flags.
		  /// 
		  /// Either shifts the accumulator left 1 bit and stores the carry in accumulator bit 0 or does 
		  /// not affect the internal reg­isters at all. 
		  /// Sets the carry equal to the input bit 7.
		  /// Sets N equal to the input bit 6
		  /// Sets the Z flag if the result of the ro­tate is 0, otherwise it resets Z
		  /// Does not affect the overflow flag at all.
		  
		  // Get the data to work on.
		  Var address As UInt16
		  Var data As UInt16
		  If addressMode <> Addressmodes.Accumulator Then
		    address = EffectiveAddress(addressMode)
		    data = Memory(address)
		  Else
		    data = A
		  End If
		  
		  // Rotate left. Use 16-bits to track the new carry.
		  Var result As UInt16 = ShiftLeft(data, 1)
		  
		  // The old carry moves into bit 0.
		  If CarryFlag Then
		    result = result Or &b00000001
		  Else
		    result = result And &b11111110
		  End If
		  
		  // Set the flags.
		  CarryFlag = (data And &b10000000) <> 0
		  NegativeFlag = (data And &b01000000) <> 0
		  ZeroFlag = (result = 0)
		  
		  // Set the result to appropriate location.
		  If addressMode = AddressModes.Accumulator Then
		    A = result
		  Else
		    Memory(address) = result
		  End If
		  
		  // How many cycles?
		  Select Case addressMode
		  Case addressModes.Accumulator
		    TotalCycles = TotalCycles + 2
		    
		  Case addressModes.Absolute
		    TotalCycles = TotalCycles + 6
		    
		  Case addressModes.XIndexedAbsolute
		    TotalCycles = TotalCycles + 7
		    
		  Case addressModes.ZeroPage
		    TotalCycles = TotalCycles + 5
		    
		  Case addressModes.XIndexedZeroPage
		    TotalCycles = TotalCycles + 6
		  End Select
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h21, Description = 525449202D2052657475726E2046726F6D20496E74657272757074
		Private Sub RTI()
		  /// RTI - Return From Interrupt
		  /// 
		  /// Operation: P↑ PC↑
		  ///
		  /// Transfers from the stack into the microprocessor the processor status and the program counter 
		  /// location for the instruction which was interrupted. By virtue of the interrupt having
		  /// stored this data before executing the instruction and the fact that the RTI re-initialises
		  /// the microprocessor to the same state as when it was interrupted, the combination of 
		  /// interrupt plus RTI allows truly re-entrant coding.
		  ///
		  /// Re-initializes all flags to the position to the point they were at the time the interrupt 
		  /// was taken and sets the program counter back to its pre-interrupt state. 
		  /// It affects no other registers in the microprocessor.
		  
		  P = PopByte
		  
		  // Clear the break flag and ensure bit 5 is set.
		  BreakFlag = False
		  SetStatusBit5
		  
		  PC = PopWord
		  
		  TotalCycles = TotalCycles + 6
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h21, Description = 525453202D2052657475726E2046726F6D20537562726F75746D65
		Private Sub RTS()
		  /// RTS - Return From Subroutme
		  ///
		  /// Operation: PC↑, PC + 1 → PC
		  /// Restore the program counter from the stack and increment it by one. Adjust the stack pointer.
		  /// The RTS instruction does not affect any flags and affects only PCL and PCH.
		  
		  Var pcl As UInt8 = PopByte
		  Var pch As UInt8 = PopByte
		  
		  PC = (pch * 256) + pcl + 1
		  
		  TotalCycles = TotalCycles + 6
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h21, Description = 536574732074686520756E7573656420626974203520696E20746865207374617475732072656769737465722E
		Private Sub SetStatusBit5()
		  /// Sets the unused bit 5 in the status register.
		  
		  P = P Or &b00100000
		End Sub
	#tag EndMethod


	#tag Note, Name = Address Modes
		Credit: https://www.pagetable.com/c64ref/6502/?tab=3#a16,X
		
		Absolute
		--------
		3 bytes.
		The second byte of the instruction specifies the eight low order bits of the effective 
		address while the third byte specifies the eight high order bits. 
		Thus, the absolute addressing mode allows access to the entire 65 K bytes of addressable memory.
		
		Absolute Indirect
		-----------------
		3 bytes.
		The second byte of the instruction contains the low order eight bits of a memory location. 
		The high order eight bits of that memory location is contained in the third byte of the instruction.
		The contents of the fully specified memory location is the low order byte of the effective address. 
		The next memory location contains the high order byte of the effective address which is loaded into 
		the sixteen bits of the program counter.
		
		Accumulator
		-----------
		1 byte.
		Represented with a one byte instruction, implying an operation on the accumulator.
		
		Immediate
		---------
		2 bytes.
		The operand is contained in the second byte of the instruction, with no further memory 
		addressing required.
		
		Implied
		-------
		1 byte.
		The address containing the operand is implicitly stated in the operation code of the instruction.
		
		Relative
		--------
		2 bytes.
		Used only with branch instructions and establishes a destination for the conditional branch.
		The second byte of-the instruction becomes the operand which is an “Offset" added to the contents of 
		the lower eight bits of the program counter when the counter is set at the next instruction. 
		The range of the offset is —128 to +127 bytes from the next instruction.
		
		X-Indexed Absolute
		------------------
		3 bytes.
		Used in conjunction with the X index register. The effective address is formed by adding the 
		contents of X to the address contained in the second and third bytes of the instruction. 
		This mode allows the index register to contain the index or count value and the instruction 
		to contain the base address. This type of indexing allows any location referencing and the 
		index to modify multiple fields resulting in reduced coding and execution time.
		
		X-Indexed Zero Page
		-------------------
		2 bytes.
		Used in conjunction with the X index register. The effective address is 
		calculated by adding the second byte to the contents of the index register. Since this is a 
		form of "Zero Page" addressing, the content of the second byte references a location in page zero.
		Additionally, due to the “Zero Page" addressing nature of this mode, no carry is added to the 
		high order 8 bits of memory and crossing of page boundaries does not occur.
		
		X-Indexed Zero Page Indirect
		----------------------------
		2 bytes.
		The second byte of the instruction is added to the contents of the 
		X index register, discarding the carry. The result of this addition points to a memory location on 
		page zero whose contents is the low order eight bits of the effective address. The next memory 
		location in page zero contains the high order eight bits of the effective address. 
		Both memory locations specifying the high and low order bytes of the effective address must be in 
		page zero.
		
		Y-Indexed Absolute
		------------------
		3 bytes.
		Used in conjunction with the Y index register. 
		The effective address is formed by adding the contents of Y to the address contained in the 
		second and third bytes of the instruction. This mode allows the index register to contain the 
		index or count value and the instruction to contain the base address. 
		This type of indexing allows any location referencing and the index to modify multiple fields 
		resulting in reduced coding and execution time.
		
		Y-Indexed Zero Page
		-------------------
		2 bytes.
		Used in conjunction with the Y index register. The effective address 
		is calculated by adding the second byte to the contents of the index register. Since this is a 
		form of "Zero Page" addressing, the content of the second byte references a location in page zero.
		Additionally, due to the “Zero Page" addressing nature of this mode, no carry is added to the 
		high order 8 bits of memory and crossing of page boundaries does not occur.
		
		Zero Page
		---------
		2 bytes.
		The zero page instructions allow for shorter code and execution times by only fetching the second 
		byte of the instruction and assuming a zero high address byte. Careful use of the zero page can 
		result in significant increase in code efficiency.
		
		Zero Page Indirect Y-Indexed
		----------------------------
		2 bytes.
		The second byte of the instruction points to a memory location in page zero. The contents of this 
		memory location is added to the contents of the Y index register, the result being the low order 
		eight bits of the effective address. The carry from this addition is added to the contents of the
		next page zero memory location, the result being the high order eight bits of the effective address.
		
		
	#tag EndNote

	#tag Note, Name = Status Register (P)
		Bit:  7 6 5 4 3 2 1 0
		Flag: N V - B D I Z C
		
		N: Negative
		V: Overflow
		B: Break
		D: Decimal
		I: Interrupt disable
		Z: Zero
		C: Carry
		
	#tag EndNote


	#tag Property, Flags = &h0, Description = 382D62697420616363756D756C61746F722E
		A As UInt8
	#tag EndProperty

	#tag ComputedProperty, Flags = &h0
		#tag Getter
			Get
			  Return (P And &b00010000) <> 0
			End Get
		#tag EndGetter
		#tag Setter
			Set
			  If value Then
			    P = P Or &b00010000
			  Else
			    P = P And &b11101111
			  End If
			End Set
		#tag EndSetter
		BreakFlag As Boolean
	#tag EndComputedProperty

	#tag ComputedProperty, Flags = &h0
		#tag Getter
			Get
			  Return (P And &b00000001) <> 0
			End Get
		#tag EndGetter
		#tag Setter
			Set
			  If value Then
			    P = P Or &b00000001
			  Else
			    P = P And &b11111110
			  End If
			End Set
		#tag EndSetter
		CarryFlag As Boolean
	#tag EndComputedProperty

	#tag Property, Flags = &h21, Description = 53657420746F205472756520647572696E672061206D656D6F72792066657463682069662061207061676520626F756E646172792069732063726F737365642E
		Private CrossedPageBoundary As Boolean = False
	#tag EndProperty

	#tag ComputedProperty, Flags = &h0
		#tag Getter
			Get
			  Return (P And &b00001000) <> 0
			End Get
		#tag EndGetter
		#tag Setter
			Set
			  If value Then
			    P = P Or &b00001000
			  Else
			    P = P And &b11110111
			  End If
			End Set
		#tag EndSetter
		DecimalFlag As Boolean
	#tag EndComputedProperty

	#tag Property, Flags = &h0
		Halted As Boolean = False
	#tag EndProperty

	#tag ComputedProperty, Flags = &h0
		#tag Getter
			Get
			  Return (P And &b00000100) <> 0
			End Get
		#tag EndGetter
		#tag Setter
			Set
			  If value Then
			    P = P Or &b00000100
			  Else
			    P = P And &b11111011
			  End If
			End Set
		#tag EndSetter
		InterruptDisableFlag As Boolean
	#tag EndComputedProperty

	#tag Property, Flags = &h0, Description = 54686520435055277320616365737369626C65206D656D6F72792E
		Memory As Memory
	#tag EndProperty

	#tag ComputedProperty, Flags = &h0
		#tag Getter
			Get
			  Return (P And &b10000000) <> 0
			End Get
		#tag EndGetter
		#tag Setter
			Set
			  If value Then
			    P = P Or &b10000000
			  Else
			    P = P And &b01111111
			  End If
			End Set
		#tag EndSetter
		NegativeFlag As Boolean
	#tag EndComputedProperty

	#tag ComputedProperty, Flags = &h0
		#tag Getter
			Get
			  Return (P And &b01000000) <> 0
			End Get
		#tag EndGetter
		#tag Setter
			Set
			  If value Then
			    P = P Or &b01000000
			  Else
			    P = P And &b10111111
			  End If
			End Set
		#tag EndSetter
		OverflowFlag As Boolean
	#tag EndComputedProperty

	#tag Property, Flags = &h0, Description = 54686520382D6269742070726F636573736F72207374617475732072656769737465722028666C616773292E
		P As UInt8
	#tag EndProperty

	#tag Property, Flags = &h0, Description = 31362D6269742070726F6772616D20636F756E7465722E
		PC As UInt16
	#tag EndProperty

	#tag ComputedProperty, Flags = &h0, Description = 54686520686967682062797465206F662050432E
		#tag Getter
			Get
			  Return (PC And &hFF00) / 256
			End Get
		#tag EndGetter
		PCH As UInt8
	#tag EndComputedProperty

	#tag ComputedProperty, Flags = &h0, Description = 546865206C6F772062797465206F662050432E
		#tag Getter
			Get
			  Return PC And &hFF
			End Get
		#tag EndGetter
		PCL As UInt8
	#tag EndComputedProperty

	#tag Property, Flags = &h0, Description = 382D62697420737461636B20706F696E7465722E
		SP As UInt8
	#tag EndProperty

	#tag Property, Flags = &h0, Description = 54686520746F74616C206E756D626572206F66206379636C657320657865637574656420627920746865204350552073696E636520746865206C6173742072657365742E
		TotalCycles As Integer = 0
	#tag EndProperty

	#tag Property, Flags = &h0, Description = 382D62697420582072656769737465722E
		X As UInt8
	#tag EndProperty

	#tag Property, Flags = &h0, Description = 382D62697420592072656769737465722E
		Y As UInt8
	#tag EndProperty

	#tag ComputedProperty, Flags = &h0
		#tag Getter
			Get
			  Return (P And &b00000010) <> 0
			End Get
		#tag EndGetter
		#tag Setter
			Set
			  If value Then
			    P = P Or &b00000010
			  Else
			    P = P And &b11111101
			  End If
			  
			End Set
		#tag EndSetter
		ZeroFlag As Boolean
	#tag EndComputedProperty


	#tag ViewBehavior
		#tag ViewProperty
			Name="Name"
			Visible=true
			Group="ID"
			InitialValue=""
			Type="String"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="Index"
			Visible=true
			Group="ID"
			InitialValue="-2147483648"
			Type="Integer"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="Super"
			Visible=true
			Group="ID"
			InitialValue=""
			Type="String"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="Left"
			Visible=true
			Group="Position"
			InitialValue="0"
			Type="Integer"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="Top"
			Visible=true
			Group="Position"
			InitialValue="0"
			Type="Integer"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="A"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="UInt8"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="X"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="UInt8"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="Y"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="UInt8"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="SP"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="UInt8"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="PC"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="UInt16"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="P"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="UInt8"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="Halted"
			Visible=false
			Group="Behavior"
			InitialValue="False"
			Type="Boolean"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="TotalCycles"
			Visible=false
			Group="Behavior"
			InitialValue="0"
			Type="Integer"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="BreakFlag"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Boolean"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="CarryFlag"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Boolean"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="DecimalFlag"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Boolean"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="InterruptDisableFlag"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Boolean"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="NegativeFlag"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Boolean"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="OverflowFlag"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Boolean"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="PCH"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="UInt8"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="PCL"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="UInt8"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="ZeroFlag"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Boolean"
			EditorType=""
		#tag EndViewProperty
	#tag EndViewBehavior
End Class
#tag EndClass
