#tag Class
Protected Class CPU
	#tag Method, Flags = &h21, Description = 414443202D20416464204D656D6F727920746F20416363756D756C61746F7220776974682043617272792E
		Private Sub ADC(addressMode As MOS6502.AddressModes)
		  /// ADC - Add Memory to Accumulator with Carry.
		  ///
		  /// Operation: A + M + C → A, C
		  ///
		  /// Adds the value of memory and carry from the previous operation to the value of the accumulator 
		  /// and stores the result in the accumulator.
		  ///
		  /// Affects the accumulator.
		  /// Sets the carry flag when the sum of a binary add exceeds 255 or when the sum of a decimal 
		  /// add exceeds 99, otherwise carry is reset. 
		  /// The overflow flag is set when the sign or bit 7 is changed due to the result 
		  /// exceeding +127 or -128, otherwise overflow is reset. 
		  /// The negative flag is set if the accumulator result contains bit 7 on, otherwise the 
		  /// negative flag is reset. 
		  /// The zero flag is set if the accumulator result is 0, otherwise the zero flag is reset.
		  
		  // Get the operand.
		  Var operand As UInt8
		  If addressMode = AddressModes.Immediate Then
		    operand = FetchByte
		  Else
		    operand = Memory(EffectiveAddress(addressMode))
		  End If
		  
		  If DecimalFlag Then
		    // https://github.com/mnaberez/py65/blob/main/py65/devices/mpu6502.py#L317
		    
		    Var halfcarry, decimalcarry, adjust0, adjust1 As Integer = 0
		    Var nibble0 As Integer = (operand And &hf) + (A And &hf) + (P And 1)
		    
		    If nibble0 > 9 Then
		      adjust0 = 6
		      halfcarry = 1
		    End If
		    
		    Var nibble1 As Integer = (ShiftRight(operand, 4) And &hf) + (ShiftRight(A, 4) And &hf) + halfcarry
		    
		    If nibble1 > 9 Then
		      adjust1 = 6
		      decimalcarry = 1
		    End If
		    
		    // The ALU outputs are not decimally adjusted.
		    nibble0 = nibble0 And &hf
		    nibble1 = nibble1 And &hf
		    Var aluresult As Integer = ShiftLeft(nibble1, 4) + nibble0
		    
		    // The final A contents will be decimally adjusted.
		    nibble0 = (nibble0 + adjust0) And &hf
		    nibble1 = (nibble1 + adjust1) And &hf
		    P = P And -196 // -196 = Not(CARRY Or OVERFLOW Or NEGATIVE Or ZERO)
		    If aluresult = 0 Then
		      P = P Or 2
		    Else
		      P = P Or aluresult And 128
		    End If
		    
		    If decimalcarry = 1 Then
		      P = P Or 1
		    End If
		    
		    If ((Not(A Xor operand) And (A Xor aluresult)) And 128) <> 0 Then
		      p = P Or 64
		    End If
		    
		    A = ShiftLeft(nibble1, 4) + nibble0
		    
		  Else
		    // https://github.com/sethm/symon/blob/master/src/main/java/com/loomcom/symon/Cpu.java#L1311
		    
		    Var result As Integer = (operand And &hff) + (A And &hff) + If(CarryFlag, 1, 0)
		    Var carry6 As Integer = (operand And &h7f) + (A And &h7f) + If(CarryFlag, 1, 0)
		    CarryFlag = (result And &h100) <> 0
		    OverflowFlag = CarryFlag Xor ((carry6 And &h80) <> 0)
		    A = result And &hff
		    SetArithmeticFlags(A)
		  End If
		  
		  // How many cycles?
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
		Private Sub BCC()
		  /// BCC - Branch on Carry Clear.
		  ///
		  /// Operation: Branch on C = 0
		  /// 
		  /// Tests the state of the carry bit and takes a conditional branch if the carry bit is reset.
		  /// Affects no flags or registers other than the program counter and then only if the C flag 
		  /// is not on.
		  
		  Var targetAddress As UInt16 = EffectiveAddress(AddressModes.Relative)
		  
		  If Not CarryFlag Then
		    PC = targetAddress
		    TotalCycles = TotalCycles + 3 + If(CrossedPageBoundary, 1, 0)
		  Else
		    TotalCycles = TotalCycles + 2 + If(CrossedPageBoundary, 1, 0)
		  End If
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h21
		Private Sub BCS()
		  /// BCS - Branch on Carry Set.
		  ///
		  /// Operation: Branch on C = 1
		  ///
		  /// Takes the conditional branch if the carry flag is on.
		  /// Does not affect any of the flags or registers except for the program counter and only then 
		  /// if the carry flag is on.
		  
		  Var targetAddress As UInt16 = EffectiveAddress(AddressModes.Relative)
		  
		  If CarryFlag Then
		    PC = targetAddress
		    TotalCycles = TotalCycles + 3 + If(CrossedPageBoundary, 1, 0)
		  Else
		    TotalCycles = TotalCycles + 2 + If(CrossedPageBoundary, 1, 0)
		  End If
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h21
		Private Sub BEQ()
		  /// BEQ - Branch on Result Zero.
		  ///
		  /// Operation: Branch on Z = 1
		  ///
		  /// This instruction could also be called "Branch on Equal."
		  /// It takes a conditional branch whenever the Z flag is on or the previ­ous result is equal to 0.
		  /// Does not affect any of the flags or registers other than the program counter and only then 
		  /// when the Z flag is set.
		  
		  Var targetAddress As UInt16 = EffectiveAddress(AddressModes.Relative)
		  
		  If ZeroFlag Then
		    PC = targetAddress
		    TotalCycles = TotalCycles + 3 + If(CrossedPageBoundary, 1, 0)
		  Else
		    TotalCycles = TotalCycles + 2 + If(CrossedPageBoundary, 1, 0)
		  End If
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

	#tag Method, Flags = &h21
		Private Sub BNE()
		  /// BNE - Branch on Result Not Zero.
		  ///
		  /// Operation: Branch on Z = 0
		  ///
		  /// This instruction could also be called "Branch on Not Equal." It tests the Z flag and takes 
		  /// the conditional branch if the Z flag is not on, indicating that the previous result was not zero.
		  /// Does not affect any of the flags or registers other than the program counter and only then 
		  /// if the Z flag is reset.
		  
		  Var targetAddress As UInt16 = EffectiveAddress(AddressModes.Relative)
		  
		  If Not ZeroFlag Then
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

	#tag Method, Flags = &h21
		Private Sub BVC()
		  /// BVC - Branch on Overflow Clear.
		  ///
		  /// Operation: Branch on V = 0
		  ///
		  /// Tests the status of the V flag and takes the conditional branch if the flag is not set.
		  /// Does not affect any of the flags and registers other than the program counter and only 
		  /// when the overflow flag is reset.
		  
		  Var targetAddress As UInt16 = EffectiveAddress(AddressModes.Relative)
		  
		  If Not OverflowFlag Then
		    PC = targetAddress
		    TotalCycles = TotalCycles + 3 + If(CrossedPageBoundary, 1, 0)
		  Else
		    TotalCycles = TotalCycles + 2 + If(CrossedPageBoundary, 1, 0)
		  End If
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h21
		Private Sub BVS()
		  /// BVS - Branch on Overflow Set.
		  ///
		  /// Operation: Branch on V = 1
		  ///
		  /// Tests the V flag and takes the conditional branch if V is on.
		  /// Does not affect any flags or registers other than the program counter and only when the 
		  /// overflow flag is set.
		  
		  Var targetAddress As UInt16 = EffectiveAddress(AddressModes.Relative)
		  
		  If OverflowFlag Then
		    PC = targetAddress
		    TotalCycles = TotalCycles + 3 + If(CrossedPageBoundary, 1, 0)
		  Else
		    TotalCycles = TotalCycles + 2 + If(CrossedPageBoundary, 1, 0)
		  End If
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

	#tag Method, Flags = &h21
		Private Sub DEX()
		  /// DEX - Decrement Index Register X By One
		  ///
		  /// Operation: X - 1 → X
		  ///
		  /// Subtracts one from the current value of the index register X and stores the result in the 
		  /// index register X.
		  ///
		  /// Does not affect the carry or overflow flag.
		  /// Sets the N flag if it has bit 7 on as a result of the decrement, otherwise it resets the N flag.
		  /// Sets the Z flag if X is a 0 as a result of the decrement, otherwise it resets the Z flag.
		  
		  X = X - 1
		  
		  NegativeFlag = (X And &b10000000) <> 0
		  
		  ZeroFlag = (X = 0)
		  
		  TotalCycles = TotalCycles + 2
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h21, Description = 444559202D2044656372656D656E7420496E6465782052656769737465722059204279204F6E652E
		Private Sub DEY()
		  /// DEY - Decrement Index Register Y By One.
		  ///
		  /// Operation: Y - 1 → Y
		  /// Subtracts one from the current value in the in­dex register Y and stores the result into the 
		  /// index register Y. The result does not affect or consider carry so that the value in the 
		  /// index register Y is decremented to 0 and then through 0 to FF.
		  ///
		  /// Does not affect the carry or overflow flags.
		  /// If the Y register contains bit 7 on as a result of the decrement the N flag is set, otherwise 
		  /// the N flag is reset. 
		  /// If the Y register is 0 as a result of the decrement, the Z flag is set otherwise the 
		  /// Z flag is reset. 
		  /// This instruction only affects the index register Y.
		  
		  Y = Y - 1
		  
		  NegativeFlag = (Y And &b10000000) <> 0
		  ZeroFlag = (Y = 0)
		  
		  TotalCycles = TotalCycles + 2
		  
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
		    
		  Case AddressModes.AbsoluteIndirect
		    // Only used by the JMP opcode.
		    Var lsb As UInt8 = FetchByte
		    Var fullySpecified As UInt16 = FetchByte * 256 + lsb
		    Var effectiveLSB As UInt8 = Memory(fullySpecified)
		    
		    // The indirect jump instruction does not increment the page address when the indirect 
		    // pointer crosses a page boundary. JMP ($xxFF) will fetch the address from $xxFF and $xx00.
		    If lsb = 255 Then
		      Return Memory(fullySpecified - 255) * 256 + effectiveLSB
		    Else
		      Return Memory(fullySpecified + 1) * 256 + effectiveLSB
		    End If
		    
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
		    
		  Case AddressModes.YIndexedZeroPage
		    Return FetchByte + Y
		    
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

	#tag Method, Flags = &h21, Description = 454F52202D20224578636C7573697665204F5222204D656D6F7279207769746820416363756D756C61746F722E
		Private Sub EOR(addressMode As MOS6502.AddressModes)
		  /// EOR - "Exclusive OR" Memory with Accumulator.
		  ///
		  /// Operation: A ⊻ M → A
		  ///
		  /// Transfers the memory and the accumulator to the adder which performs a binary "EXCLUSIVE OR" 
		  /// on a bit-by-bit basis and stores the result in the accumulator.
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
		  
		  A = A Xor data
		  
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
		    Raise New UnsupportedOperationException("Unsupported EOR instruction.")
		  End Select
		End Sub
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
		    
		  Case &h41 // EOR ($nn,X)
		    EOR(AddressModes.XIndexedZeroPageIndirect)
		    
		  Case &h45 // EOR $nn
		    EOR(AddressModes.ZeroPage)
		    
		  Case &h46 // LSR $nn
		    LSR(AddressModes.ZeroPage)
		    
		  Case &h48 // PHA
		    PushByte(A)
		    TotalCycles = TotalCycles + 3
		    
		  Case &h49 // EOR #$nn
		    EOR(AddressModes.Immediate)
		    
		  Case &h4A // LSR A
		    LSR(AddressModes.Accumulator)
		    
		  Case &h4C // JMP $nnnn
		    JMP(AddressModes.Absolute)
		    
		  Case &h4D // EOR $nnnn
		    EOR(AddressModes.Absolute)
		    
		  Case &h4E // LSR $nnnn
		    LSR(AddressModes.Absolute)
		    
		  Case &h50 // BVC
		    BVC
		    
		  Case &h51 // EOR ($nn),Y
		    EOR(AddressModes.ZeroPageIndirectYIndexed)
		    
		  Case &h55 // EOR $nn,X
		    EOR(AddressModes.XIndexedZeroPage)
		    
		  Case &h56 // LSR $nn,X
		    LSR(AddressModes.XIndexedZeroPage)
		    
		  Case &h58 // CLI
		    InterruptDisableFlag = False
		    TotalCycles = TotalCycles + 2
		    
		  Case &h59 // EOR $nnnn,Y
		    EOR(AddressModes.YIndexedAbsolute)
		    
		  Case &h5D // EOR $nnnn,X
		    EOR(AddressModes.XIndexedAbsolute)
		    
		  Case &h5E // LSR $nnnn,X
		    LSR(AddressModes.XIndexedAbsolute)
		    
		  Case &h60 // RTS
		    RTS
		    
		  Case &h61 // ADC ($nn,X)
		    ADC(AddressModes.XIndexedZeroPageIndirect)
		    
		  Case &h65 // ADC $nn
		    ADC(AddressModes.ZeroPage)
		    
		  Case &h66 // ROR $nn
		    ROR(AddressModes.ZeroPage)
		    
		  Case &h68 // PLA
		    PLA
		    
		  Case &h69 // ADC #$nn
		    ADC(AddressModes.Immediate)
		    
		  Case &h6A // ROR A
		    ROR(AddressModes.Accumulator)
		    
		  Case &h6C // JMP ($nnnn)
		    JMP(AddressModes.AbsoluteIndirect)
		    
		  Case &h6D // ADC $nnnn
		    ADC(AddressModes.Absolute)
		    
		  Case &h6E // ROR $nnnn
		    ROR(AddressModes.Absolute)
		    
		  Case &h70 // BVS
		    BVS
		    
		  Case &h71 // ADC ($nn),Y
		    ADC(AddressModes.ZeroPageIndirectYIndexed)
		    
		  Case &h75 // ADC $nn,X
		    ADC(AddressModes.XIndexedZeroPage)
		    
		  Case &h76 // ROR $nn,X
		    ROR(AddressModes.XIndexedZeroPage)
		    
		  Case &h7D // ADC $nnnn,X
		    ADC(AddressModes.XIndexedAbsolute)
		    
		  Case &h7E // ROR $nnnn,X
		    ROR(AddressModes.XIndexedAbsolute)
		    
		  Case &h78 // SEI
		    InterruptDisableFlag = True
		    TotalCycles = TotalCycles + 2
		    
		  Case &h79 //ADC $nnnn,Y
		    ADC(AddressModes.YIndexedAbsolute)
		    
		  Case &h81 // STA ($nn,X)
		    STA(AddressModes.XIndexedZeroPageIndirect)
		    
		  Case &h84 // STY $nn
		    STY(AddressModes.ZeroPage)
		    
		  Case &h85 // STA $nn
		    STA(AddressModes.ZeroPage)
		    
		  Case &h86 // STX $nn
		    STX(AddressModes.ZeroPage)
		    
		  Case &h88 // DEY
		    DEY
		    
		  Case &h8A // TXA
		    TXA
		    
		  Case &h8C // STY $nnnn
		    STY(AddressModes.Absolute)
		    
		  Case &h8D // STA $nnnn
		    STA(AddressModes.Absolute)
		    
		  Case &h8E // STX $nnnn
		    STX(AddressModes.Absolute)
		    
		  Case &h90 // BCC
		    BCC
		    
		  Case &h91 // STA ($nn),Y
		    STA(AddressModes.ZeroPageIndirectYIndexed)
		    
		  Case &h94 // STY $nn,X
		    STY(AddressModes.XIndexedZeroPage)
		    
		  Case &h95 // STA $nn,X
		    STA(AddressModes.XIndexedZeroPage)
		    
		  Case &h96 // STX $nn,Y
		    STX(AddressModes.YIndexedZeroPage)
		    
		  Case &h98 // TYA
		    TYA
		    
		  Case &h99 // STA $nnnn,Y
		    STA(AddressModes.YIndexedAbsolute)
		    
		  Case &h9A // TXS
		    SP = X
		    TotalCycles = TotalCycles + 2
		    
		  Case &h9D // STA $nnnn,X
		    STA(AddressModes.XIndexedAbsolute)
		    
		  Case &hA0 // LDY #$nn
		    LDY(AddressModes.Immediate)
		    
		  Case &hA4 // LDY $nn
		    LDY(AddressModes.ZeroPage)
		    
		  Case &hA8 // TAY
		    TAY
		    
		  Case &hAA // TAX
		    TAX
		    
		  Case &hAC // LDY $nnnn
		    LDY(AddressModes.Absolute)
		    
		  Case &hB0 // BCS
		    BCS
		    
		  Case &hB4 // LDY $nn,X
		    LDY(AddressModes.XIndexedZeroPage)
		    
		  Case &hB8 // CLV
		    OverflowFlag = False
		    TotalCycles = TotalCycles + 2
		    
		  Case &hBA // TSX
		    TSX
		    
		  Case &hBC // LDY $nnnn,X
		    LDY(AddressModes.XIndexedAbsolute)
		    
		  Case &hC8 // INY
		    INY
		    
		  Case &hCA // DEX
		    DEX
		    
		  Case &hD0 // BNE
		    BNE
		    
		  Case &hD8 // CLD
		    DecimalFlag = False
		    TotalCycles = TotalCycles + 2
		    
		  Case &hE1 // SBC ($nn,X)
		    SBC(AddressModes.XIndexedZeroPageIndirect)
		    
		  Case &hE5 // SBC $nn
		    SBC(AddressModes.ZeroPage)
		    
		  Case &hE8 // INX
		    INX
		    
		  Case &hE9 // SBC #$nn
		    SBC(AddressModes.Immediate)
		    
		  Case &hEA // NOP
		    TotalCycles = TotalCycles + 2
		    
		  Case &hED // SBC $nnnn
		    SBC(AddressModes.Absolute)
		    
		  Case &hF0 // BEQ
		    BEQ
		    
		  Case &hF1 // SBC ($nn),Y
		    SBC(AddressModes.ZeroPageIndirectYIndexed)
		    
		  Case &hF5 // SBC $nn,X
		    SBC(AddressModes.XIndexedZeroPage)
		    
		  Case &hF8 // SED
		    DecimalFlag = True
		    TotalCycles = TotalCycles + 2
		    
		  Case &hF9 // SBC $nnnn,Y
		    SBC(AddressModes.YIndexedAbsolute)
		    
		  Case &hFD // SBC $nnnn,X
		    SBC(AddressModes.XIndexedAbsolute)
		    
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

	#tag Method, Flags = &h21, Description = 494E58202D20496E6372656D656E7420496E6465782052656769737465722058204279204F6E652E
		Private Sub INX()
		  /// INX - Increment Index Register X By One.
		  /// 
		  /// Operation: X + 1 → X
		  /// 
		  /// Adds 1 to the current value of the X register. This is an 8-bit increment which does not 
		  /// affect the carry operation, therefore, if the value of X before the increment was FF, 
		  /// the resulting value is 00.
		  /// 
		  /// Does not affect the carry or overflow flags.
		  /// Sets the N flag if the result of the increment has a one in bit 7, otherwise resets N.
		  /// Sets the Z flag if the result of the increment is 0, otherwise it resets the Z flag.
		  ///
		  /// Does not affect any other register other than the X register.
		  
		  X = X + 1
		  
		  NegativeFlag = (X And &b10000000) <> 0
		  
		  ZeroFlag = (X = 0)
		  
		  TotalCycles = TotalCycles + 2
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h21
		Private Sub INY()
		  /// INY - Increment Index Register Y By One.
		  ///
		  /// Operation: Y + 1 → Y
		  ///
		  /// Adds one to the current value in the Y register, storing the result in the Y register. 
		  /// As in the case of INX the primary application is to step through a set of values using the 
		  /// Y register.
		  /// Does not affect the carry or overflow flags.
		  /// Sets the N flag if the result of the increment has a one in bit 7, otherwise resets N.
		  /// Sets Z if as a result of the increment the Y register is zero otherwise resets the Z flag.
		  
		  Y = Y + 1
		  
		  NegativeFlag = (Y And &b10000000) <> 0
		  
		  ZeroFlag = (Y = 0)
		  
		  TotalCycles = TotalCycles + 2
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h21, Description = 4A4D50202D204A4D5020496E6469726563742E
		Private Sub JMP(addressMode As MOS6502.AddressModes)
		  /// JMP - JMP Indirect.
		  ///
		  /// A new address is loaded into the program counter. 
		  ///
		  /// It affects only the program counter in the microprocessor and affects no flags in the 
		  /// status register.
		  
		  PC = EffectiveAddress(addressMode)
		  
		  Select Case addressMode
		  Case AddressModes.Absolute
		    TotalCycles = TotalCycles + 3
		    
		  Case AddressModes.AbsoluteIndirect
		    TotalCycles = TotalCycles + 5
		  End Select
		  
		End Sub
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

	#tag Method, Flags = &h21, Description = 4C4459202D204C6F616420496E64657820526567697374657220592046726F6D204D656D6F72792E
		Private Sub LDY(addressMode As MOS6502.AddressModes)
		  /// LDY - Load Index Register Y From Memory.
		  ///
		  /// Operation: M → Y
		  /// 
		  /// Load the index register Y from memory.
		  ///
		  /// Does not affect the C or V flags.
		  /// Sets the N flag if the value loaded in bit 7 is a 1, otherwise resets N.
		  /// Sets Z flag if the loaded value is zero otherwise resets Z.
		  /// Only affects the Y register.
		  
		  // Get the data.
		  Var data As UInt8
		  If addressMode = AddressModes.Immediate Then
		    data = FetchByte
		  Else
		    data = Memory(EffectiveAddress(addressMode))
		  End If
		  
		  Y = data
		  
		  NegativeFlag = (Y And &b10000000) <> 0
		  
		  ZeroFlag = (Y = 0)
		  
		  Select Case addressMode
		  Case AddressModes.Immediate
		    TotalCycles = TotalCycles + 2
		    
		  Case AddressModes.Absolute
		    TotalCycles = TotalCycles + 4
		    
		  Case AddressModes.XIndexedAbsolute
		    TotalCycles = TotalCycles + 4 + If(CrossedPageBoundary, 1, 0)
		    
		  Case AddressModes.ZeroPage
		    TotalCycles = TotalCycles + 3
		    
		  Case AddressModes.XIndexedZeroPage
		    TotalCycles = TotalCycles + 4
		  End Select
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h21, Description = 4C5352202D204C6F676963616C2053686966742052696768742E
		Private Sub LSR(addressMode As MOS6502.AddressModes)
		  /// LSR - Logical Shift Right.
		  ///
		  /// Shift the specified contents (accumulator or memory) right by one bit position. A "0" is 
		  /// forced in bit 7. bit 0 is transferred to the carry. The shifted data is deposited in the 
		  /// source (i.e. either the accumulator or memory).
		  ///
		  /// Does not affect the overflow flag. 
		  /// The N flag is always reset. 
		  /// The Z flag is set if the result of the shift is 0 and reset otherwise. 
		  /// The carry is set equal to bit 0 of the input.
		  
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
		  Var result As UInt8 = ShiftRight(data, 1)
		  
		  // Store the result back where it came from.
		  If addressMode = AddressModes.Accumulator Then
		    A = result
		  Else
		    Memory(address) = result
		  End If
		  
		  // Set the flags.
		  NegativeFlag = False
		  ZeroFlag = (result = 0)
		  CarryFlag = (data And &b00000001) <> 0
		  
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

	#tag Method, Flags = &h21
		Private Sub PLA()
		  /// PLA - Pull Accumulator From Stack.
		  ///
		  /// Adds 1 to the current value of the stack pointer and uses it to address the stack and loads 
		  /// the contents of the stack into the A register.
		  /// 
		  /// Does not affect the carry or overflow flags. 
		  /// Sets N if the bit 7 is on in accumulator A as a result of instructions, otherwise it is reset.
		  /// If accumulator A is zero as a result of the PLA, then the Z flag is set, otherwise it is reset. 
		  /// Changes content of the accumulator A to the contents of the memory location at stack register 
		  /// plus 1.
		  /// Increments the stack register.
		  
		  A = PopWord
		  
		  SP = SP - 1
		  
		  NegativeFlag = (A And &b10000000) <> 0
		  ZeroFlag = (A = 0)
		  
		  TotalCycles = TotalCycles + 4
		  
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

	#tag Method, Flags = &h21, Description = 524F52202D20526F746174652052696768742E
		Private Sub ROR(addressMode As MOS6502.AddressModes)
		  /// ROR - Rotate Right.
		  ///
		  /// Operation: C → /M7...M0/ → C
		  ///
		  /// Shifts either the accumulator or addressed memory right 1 bit with bit 0 shifted into the 
		  /// carry and carry shifted into bit 7.
		  ///
		  /// Either shifts the accumulator right 1 bit and stores the carry in accumulator bit 7 or does 
		  /// not affect the internal regis­ters at all. 
		  /// Sets carry equal to input bit 0.
		  /// Sets N equal to the input carry.
		  /// Sets the Z flag if the result of the rotate is 0; otherwise it resets Z.
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
		  
		  // Rotate right.
		  Var result As UInt8 = ShiftRight(data, 1)
		  
		  // Put the carry into bit 7.
		  If CarryFlag Then
		    result = result Or &b10000000
		  End If
		  
		  NegativeFlag = CarryFlag
		  
		  // Put data's bit 0 into the carry.
		  CarryFlag = (data And &b00000001) <> 0
		  
		  // Set the result to appropriate location.
		  If addressMode = AddressModes.Accumulator Then
		    A = result
		  Else
		    Memory(address) = result
		  End If
		  
		  ZeroFlag = (result = 0)
		  
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

	#tag Method, Flags = &h21, Description = 534243202D205375627472616374204D656D6F72792066726F6D20416363756D756C61746F72207769746820426F72726F772E
		Private Sub SBC(addressMode As MOS6502.AddressModes)
		  /// SBC - Subtract Memory from Accumulator with Borrow.
		  ///
		  /// Operation: A - M - ~C → A
		  ///
		  /// Subtracts the value of memory and borrow from the value of the accumulator, using two's 
		  /// complement arithmetic, and stores the result in the accumulator. 
		  /// Borrow is defined as the carry flag complemented; therefore, a resultant carry flag 
		  /// indicates that a borrow has not occurred.
		  ///
		  /// Affects the accumulator.
		  /// The carry flag is set if the result is greater than or equal to 0. The carry flag is 
		  /// reset when the result is less than 0, indicating a borrow. 
		  /// The over­flow flag is set when the result exceeds +127 or -127, otherwise it is reset. 
		  /// The negative flag is set if the result in the accumulator has bit 7 on, otherwise it is reset.
		  /// The Z flag is set if the result in the accumulator is 0, otherwise it is reset.
		  
		  // Get the operand.
		  Var operand As UInt8
		  If addressMode = AddressModes.Immediate Then
		    operand = FetchByte
		  Else
		    operand = Memory(EffectiveAddress(addressMode))
		  End If
		  
		  If DecimalFlag Then
		    
		    Var halfcarry As Integer = 1
		    Var decimalcarry, adjust0, adjust1 As Integer = 0
		    Var nibble0 As Integer = (A And &hf) + ((Not operand) And &hf) + (P And 1)
		    
		    If nibble0 <= &hf Then
		      halfcarry = 0
		      adjust0 = 10
		    End If
		    
		    Var nibble1 As Integer = (ShiftRight(A, 4) And &hf) + (ShiftRight((Not operand), 4) And &hf) + halfcarry
		    
		    If nibble1 <= &hf Then adjust1 = ShiftLeft(10, 4)
		    
		    // The ALU outputs are not decimally adjusted.
		    Const BYTE_MASK = 255
		    Var aluresult as Integer = A + ((Not operand) And BYTE_MASK) + (P And 1)
		    
		    If aluresult > BYTE_MASK Then decimalcarry = 1
		    
		    aluresult = aluresult And BYTE_MASK
		    
		    // The final result will be adjusted.
		    nibble0 = (aluresult + adjust0) And &hf
		    nibble1 = ShiftRight((aluresult + adjust1), 4) And &hf
		    
		    P = P And -196 // -196 = Not(CARRY Or ZERO Or NEGATIVE Or OVERFLOW)
		    
		    If aluresult = 0 Then
		      P = P Or 2
		    Else
		      P = P Or (aluresult And 128)
		    End If
		    
		    If decimalcarry = 1 Then P = P Or 1
		    
		    If (((A Xor operand) And (A Xor aluresult)) And 128) <> 0 Then
		      P = P Or 64
		    End If
		    
		    A = ShiftLeft(nibble1, 4) + nibble0
		    
		  Else
		    
		    operand = Not operand
		    Var result As Integer = (operand And &hff) + (A And &hff) + If(CarryFlag, 1, 0)
		    Var carry6 As Integer = (operand And &h7f) + (A And &h7f) + If(CarryFlag, 1, 0)
		    CarryFlag = (result And &h100) <> 0
		    OverflowFlag = CarryFlag Xor ((carry6 And &h80) <> 0)
		    A = result And &hff
		    SetArithmeticFlags(A)
		    
		  End If
		  
		  // How many cycles?
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

	#tag Method, Flags = &h21, Description = 5365747320746865207A65726F20616E64206E6567617469766520666C616773206F662074686520737461747573207265676973746572206261736564206F6E206120726567697374657227732076616C75652E
		Private Sub SetArithmeticFlags(registerValue As UInt8)
		  /// Sets the zero and negative flags of the status register based on a register's value.
		  
		  ZeroFlag = (registerValue = 0)
		  NegativeFlag = (registerValue And &h80) <> 0
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h21, Description = 536574732074686520756E7573656420626974203520696E20746865207374617475732072656769737465722E
		Private Sub SetStatusBit5()
		  /// Sets the unused bit 5 in the status register.
		  
		  P = P Or &b00100000
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h21, Description = 535441202D2053746F726520416363756D756C61746F7220696E204D656D6F72792E
		Private Sub STA(addressMode As MOS6502.AddressModes)
		  /// STA - Store Accumulator in Memory.
		  ///
		  /// Operation: A → M
		  ///
		  /// Transfers the contents of the accumulator to memory.
		  /// Affects none of the flags in the processor status register and does not affect the accumulator.
		  
		  Memory(EffectiveAddress(addressMode)) = A
		  
		  Select Case addressMode
		  Case AddressModes.Absolute
		    TotalCycles = TotalCycles + 4
		    
		  Case AddressModes.XIndexedAbsolute
		    TotalCycles = TotalCycles + 5
		    
		  Case AddressModes.YIndexedAbsolute
		    TotalCycles = TotalCycles + 5
		    
		  Case AddressModes.ZeroPage
		    TotalCycles = TotalCycles + 3
		    
		  Case AddressModes.XIndexedZeroPage
		    TotalCycles = TotalCycles + 4
		    
		  Case AddressModes.XIndexedZeroPageIndirect
		    TotalCycles = TotalCycles + 6
		    
		  Case AddressModes.ZeroPageIndirectYIndexed
		    TotalCycles = TotalCycles + 6
		  End Select
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h21, Description = 535458202D2053746F726520496E646578205265676973746572205820496E204D656D6F7279
		Private Sub STX(addressMode As MOS6502.AddressModes)
		  /// STX - Store Index Register X In Memory
		  ///
		  /// Operation: X → M
		  ///
		  /// Transfer the value of the X register to the addressed memory location.
		  /// 
		  /// Does not affect any flags or registers in the microprocessor.
		  
		  Memory(EffectiveAddress(addressMode)) = X
		  
		  Select Case addressMode
		  Case AddressModes.Absolute
		    TotalCycles = TotalCycles + 4
		    
		  Case AddressModes.ZeroPage
		    TotalCycles = TotalCycles + 3
		    
		  Case AddressModes.YIndexedZeroPage
		    TotalCycles = TotalCycles + 4
		  End Select
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h21, Description = 535459202D2053746F726520496E646578205265676973746572205920496E204D656D6F7279
		Private Sub STY(addressMode As MOS6502.AddressModes)
		  /// STY - Store Index Register Y In Memory
		  ///
		  /// Operation: Y → M
		  ///
		  /// Transfer the value of the Y register to the addressed memory location.
		  /// 
		  /// Does not affect any flags or registers in the microprocessor.
		  
		  Memory(EffectiveAddress(addressMode)) = Y
		  
		  Select Case addressMode
		  Case AddressModes.Absolute
		    TotalCycles = TotalCycles + 4
		    
		  Case AddressModes.ZeroPage
		    TotalCycles = TotalCycles + 3
		    
		  Case AddressModes.XIndexedZeroPage
		    TotalCycles = TotalCycles + 4
		  End Select
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h21, Description = 544158202D205472616E7366657220416363756D756C61746F7220546F20496E64657820582E
		Private Sub TAX()
		  /// TAX - Transfer Accumulator To Index X.
		  ///
		  /// Operation: A → X
		  ///
		  /// Takes the value from accumulator A and trans­fers or loads it into the index register X 
		  /// without disturbing the content of the accumulator A.
		  ///
		  /// Only affects the index register X, does not affect the carry or overflow flags. 
		  /// The N flag is set if the resultant value in the index register X has bit 7 on, 
		  /// otherwise N is reset. 
		  /// The Z bit is set if the content of the register X is 0 as aresult of the opera­tion, 
		  /// otherwise it is reset.
		  
		  X = A
		  
		  NegativeFlag = (X And &b10000000) <> 0
		  
		  ZeroFlag = (X = 0)
		  
		  TotalCycles = TotalCycles + 2
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h21, Description = 544159202D205472616E7366657220416363756D756C61746F7220546F20496E64657820592E
		Private Sub TAY()
		  /// TAY - Transfer Accumulator To Index Y.
		  ///
		  /// Operation: A → Y
		  ///
		  /// Moves the value of the accumulator into index register Y without affecting the accumulator.
		  /// 
		  /// Only affects the Y register and does not affect either the carry or overflow flags. 
		  /// If the index register Y has bit 7 on, then N is set, otherwise it is reset. 
		  /// If the content of the index register Y equals 0 as a result of the operation, 
		  /// Z is set on, otherwise it is reset.
		  
		  Y = A
		  
		  NegativeFlag = (Y And &b10000000) <> 0
		  
		  ZeroFlag = (Y = 0)
		  
		  TotalCycles = TotalCycles + 2
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h21, Description = 545358202D205472616E7366657220537461636B20506F696E74657220546F20496E64657820582E
		Private Sub TSX()
		  /// TSX - Transfer Stack Pointer To Index X.
		  ///
		  /// Operation: S → X
		  /// 
		  /// Transfers the value in the stack pointer to the index register X.
		  ///
		  /// Does not affect the carry or overflow flags. 
		  /// It sets N if bit 7 is on in index X as a result of the instruction, otherwise it is reset. 
		  /// If index X is zero as a result of the TSX, the Z flag is set, other­wise it is reset. 
		  /// TSX changes the value of index X, making it equal to the content of the stack pointer.
		  
		  X = SP
		  
		  NegativeFlag = (X And &b10000000) <> 0
		  
		  ZeroFlag = (X = 0)
		  
		  TotalCycles = TotalCycles + 2
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h21, Description = 545841202D205472616E7366657220496E646578205820546F20416363756D756C61746F722E
		Private Sub TXA()
		  /// TXA - Transfer Index X To Accumulator.
		  ///
		  /// Operation: X → A
		  ///
		  /// Moves the value that is in the index register X to the accumulator without disturbing the 
		  /// content of the index register X.
		  ///
		  /// Does not affect any register other than the accumula­tor and does not affect the carry or 
		  /// overflow flag. 
		  /// If the result in A has bit 7 on, then the N flag is set, otherwise it is reset. 
		  /// If the resultant value in the accumulator is 0, then the Z flag is set, other­ wise it's reset.
		  
		  A = X
		  
		  NegativeFlag = (A And &b10000000) <> 0
		  
		  ZeroFlag = (A = 0)
		  
		  TotalCycles = TotalCycles + 2
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h21
		Private Sub TYA()
		  /// TYA - Transfer Index Y To Accumulator.
		  ///
		  /// Operation: Y → A
		  ///
		  /// Moves the value that is in the index register Y to the accumulator without disturbing the 
		  /// content of the register Y.
		  ///
		  /// Does not affect any other register other than the accumula­tor and does not affect the 
		  /// carry or overflow flag. 
		  /// If the result in the accumulator A has bit 7 on, the N flag is set, otherwise it is reset. 
		  /// If the resultant value in the accumulator A is 0, then the Z flag is set, otherwise it's reset.
		  
		  A = Y
		  
		  NegativeFlag = (A And &b10000000) <> 0
		  
		  ZeroFlag = (A = 0)
		  
		  TotalCycles = TotalCycles + 2
		  
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
