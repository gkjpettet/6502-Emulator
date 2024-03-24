#tag Class
Protected Class CPU
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

	#tag Method, Flags = &h0, Description = 457865637574657320746865206E65787420696E737472756374696F6E2E205468697320697320612073696E676C652066657463682F6465636F64652F6578656375746520737465702E
		Sub Execute()
		  /// Executes the next instruction. 
		  /// This is a single fetch/decode/execute step.
		  
		  If Not Halted Then
		    Var opcode As UInt8 = FetchByte
		    Var cycles As Integer = ExecuteInstruction(opcode)
		    TotalCycles = TotalCycles + cycles
		  End If
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h21, Description = 4578656375746573207468652042524B206F70636F64652C2072657475726E696E6720746865206E756D626572206F66206379636C65732074616B656E2E
		Private Function ExecuteBRK() As Integer
		  /// Executes the BRK opcode, returning the number of cycles taken.
		  
		  // Push the program counter + 1 to the stack.
		  PushWord(PC + 1)
		  
		  // Push the status to the stack, setting the break flag of the saved byte to 1 (bit 4).
		  PushByte(P Or &h10)
		  
		  // Set the interrupt disable flag (bit 2).
		  P = P Or &h04
		  
		  // Load the interrupt vector from memory locations &hFFFE and &hFFFF
		  PC = ShiftLeft(Memory.Read(&hFFFF), 8) Or Memory.Read(&hFFFE)
		  
		  // 7 cycles.
		  Return 7
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h21, Description = 457865637574657320606F70636F64656020616E642072657475726E7320746865206E756D626572206F66206379636C657320697420746F6F6B2E
		Private Function ExecuteInstruction(opcode As UInt8) As Integer
		  /// Executes `opcode` and returns the number of cycles it took.
		  
		  Select Case opcode
		    
		  Case &h00 // BRK
		    TotalCycles = TotalCycles + ExecuteBRK
		    
		  Case &h01 // ORA ($nn,X)
		    TotalCycles = TotalCycles + ExecuteORA(AddressModes.XIndexedZeroPageIndirect)
		    
		  Case &h05 // ORA $nn
		    TotalCycles = TotalCycles + ExecuteORA(AddressModes.ZeroPage)
		    
		  Case &h09 // ORA #$nn
		    TotalCycles = TotalCycles + ExecuteORA(AddressModes.Immediate)
		    
		  Case &h0D // ORA $nnnn
		    TotalCycles = TotalCycles + ExecuteORA(AddressModes.Absolute)
		    
		  Case &h11 // ORA ($nn),Y
		    TotalCycles = TotalCycles + ExecuteORA(AddressModes.ZeroPageIndirectYIndexed)
		    
		  Case &h15 // ORA $nn,X
		    TotalCycles = TotalCycles + ExecuteORA(AddressModes.XIndexedZeroPage)
		    
		  Case &h19 // ORA $nnnn,Y
		    TotalCycles = TotalCycles + ExecuteORA(AddressModes.YIndexedAbsolute)
		    
		  Case &h1D // ORA $nnnn,X
		    TotalCycles = TotalCycles + ExecuteORA(AddressModes.XIndexedAbsolute)
		    
		  Else
		    // Invalid opcode. Halt the CPU.
		    Halted = True
		    Raise New MOS6502.Error("Invalid opcode " + opcode.ToString + ".")
		  End Select
		End Function
	#tag EndMethod

	#tag Method, Flags = &h21, Description = 546865204F524120696E737472756374696F6E207472616E736665727320746865206D656D6F727920616E642074686520616363756D756C61746F7220746F2074686520616464657220776869636820706572666F726D7320612062696E61727920224F5222206F6E2061206269742D62792D62697420626173697320616E642073746F7265732074686520726573756C7420696E2074686520616363756D756C61746F722E2052657475726E7320746865206E756D626572206F66206379636C65732074616B656E2E
		Private Function ExecuteORA(addressMode As MOS6502.AddressModes) As Integer
		  /// The ORA instruction transfers the memory and the accumulator to the adder which performs a 
		  /// binary "OR" on a bit-by-bit basis and stores the result in the accumulator.
		  /// Returns the number of cycles taken.
		  /// 
		  /// Operation: A ∨ M → A
		  ///
		  /// This instruction affects the accumulator.
		  /// Sets the zero flag if the result in the accumulator is 0, otherwise resets the zero flag
		  /// Sets the negative flag if the result in the accumulator has bit 7 on, otherwise resets the 
		  /// negative flag.
		  
		  // Get the data.
		  Var data As UInt8 = MemoryFetch(addressMode)
		  
		  A = A Or data
		  
		  ZeroFlag(A = 0)
		  
		  NegativeFlag((A And &b10000000) <> 0)
		  
		  // How many cycles?
		  Select Case addressMode
		  Case AddressModes.Absolute
		    Return 4
		    
		  Case AddressModes.Immediate
		    Return 2
		    
		  Case AddressModes.XIndexedAbsolute
		    Return 4 + If(CrossedPageBoundary, 1, 0)
		    
		  Case AddressModes.XIndexedZeroPage
		    Return 4
		    
		  Case AddressModes.XIndexedZeroPageIndirect
		    Return 6
		    
		  Case AddressModes.YIndexedAbsolute
		    Return 4 + If(CrossedPageBoundary, 1, 0)
		    
		  Case AddressModes.ZeroPage
		    Return 3
		    
		  Case AddressModes.ZeroPageIndirectYIndexed
		    Return 5 + If(CrossedPageBoundary, 1, 0)
		    
		  Else
		    Raise New UnsupportedOperationException("Unsupported ORA instruction.")
		  End Select
		  
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h21, Description = 4665746368657320746865206E65787420627974652066726F6D206D656D6F7279202861742061646472657373205043292E
		Private Function FetchByte() As UInt8
		  /// Fetches the next byte from memory (at address PC).
		  
		  Var value As UInt8 = Memory.Read(PC)
		  
		  PC = PC + 1
		  
		  Return value
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h21, Description = 46657463686573206120627974652066726F6D206D656D6F72792E
		Private Function MemoryFetch(addressMode As MOS6502.AddressModes) As UInt8
		  /// Fetches a byte from memory.
		  
		  CrossedPageBoundary = False
		  
		  Select Case addressMode
		  Case AddressModes.Absolute
		    Var lsb As UInt16 = FetchByte
		    Return Memory(CType(FetchByte, UInt16) * 256 + lsb)
		    
		  Case AddressModes.Immediate
		    Return FetchByte
		    
		  Case AddressModes.XIndexedAbsolute
		    #Pragma Warning "TODO: Figure out page boundary crossing"
		    Var baseLSB As UInt8 = FetchByte
		    Var baseAddress As UInt16 = CType(FetchByte, UInt16) * 256 + baseLSB + X
		    Return Memory(baseAddress)
		    
		  Case AddressModes.XIndexedZeroPage
		    Var zeroPageAddress As UInt8 = FetchByte + X
		    Return Memory(zeroPageAddress)
		    
		  Case AddressModes.XIndexedZeroPageIndirect
		    Var lowAddress As UInt16 = (X + FetchByte) And &hFF // Constrain to 8-bits.
		    Var highAddress As UInt16 = (lowAddress + 1) And &hFF // Constrain to 8-bits.
		    Var lowAddressByte As UInt8 = Memory(lowAddress)
		    Var highAddressByte As UInt8 = Memory(highAddress)
		    Var addressByte As UInt16 = ShiftLeft(highAddressByte, 8) Or lowAddressByte
		    Return Memory(addressByte)
		    
		  Case AddressModes.YIndexedAbsolute
		    #Pragma Warning "TODO: Figure out page boundary crossing"
		    Var baseLSB As UInt8 = FetchByte
		    Var baseAddress As UInt16 = CType(FetchByte, UInt16) * 256 + baseLSB + Y
		    Return Memory(baseAddress)
		    
		  Case AddressModes.ZeroPage
		    Return Memory(FetchByte)
		    
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
		    Return Memory(actualAddress)
		    
		  Else
		    Raise New UnsupportedOperationException("Unsupported address mode.")
		  End Select
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h21, Description = 5365747320746865206E6567617469766520666C616720286269742037206F662074686520737461747573207265676973746572292069662060736574203D2054727565602C207265736574732069662060736574203D2046616C7365602E
		Private Sub NegativeFlag(set As Boolean)
		  /// Sets the negative flag (bit 7 of the status register) if `set = True`, resets if `set = False`.
		  
		  If set Then
		    P = P Or &b10000000
		  Else 
		    P = P And &b01111111
		  End If
		  
		End Sub
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

	#tag Method, Flags = &h21, Description = 5365747320746865207A65726F20666C616720286269742031206F662074686520737461747573207265676973746572292069662060736574203D2054727565602C207265736574732069662060736574203D2046616C7365602E
		Private Sub ZeroFlag(set As Boolean)
		  /// Sets the zero flag (bit 1 of the status register) if `set = True`, resets if `set = False`.
		  
		  If set Then
		    P = P Or &b00000010
		  Else
		    P = P And &b11111101
		  End If
		  
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

	#tag Property, Flags = &h21, Description = 53657420746F205472756520647572696E672061206D656D6F72792066657463682069662061207061676520626F756E646172792069732063726F737365642E
		Private CrossedPageBoundary As Boolean = False
	#tag EndProperty

	#tag Property, Flags = &h0
		Halted As Boolean = False
	#tag EndProperty

	#tag Property, Flags = &h0, Description = 54686520435055277320616365737369626C65206D656D6F72792E
		Memory As Memory
	#tag EndProperty

	#tag Property, Flags = &h0, Description = 54686520382D6269742070726F636573736F72207374617475732072656769737465722028666C616773292E
		P As UInt8
	#tag EndProperty

	#tag Property, Flags = &h0, Description = 31362D6269742070726F6772616D20636F756E7465722E
		PC As UInt16
	#tag EndProperty

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
	#tag EndViewBehavior
End Class
#tag EndClass
