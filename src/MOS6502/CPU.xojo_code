#tag Class
Protected Class CPU
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
		    
		  Else
		    // Invalid opcode. Halt the CPU.
		    Halted = True
		    Raise New MOS6502.Error("Invalid opcode " + opcode.ToString + ".")
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
