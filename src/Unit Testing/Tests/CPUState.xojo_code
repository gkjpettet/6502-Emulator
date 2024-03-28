#tag Class
Protected Class CPUState
	#tag Method, Flags = &h0
		Sub Constructor()
		  Memory = New Dictionary
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub Constructor(d As Dictionary, testName As String = "")
		  /// Constructs a new CPU state from a dictionary derived from a JSON test file.
		  
		  Self.TestName = testName
		  
		  Self.A = d.Value("a")
		  Self.P = d.Value("p")
		  Self.PC = d.Value("pc")
		  Self.SP = d.Value("s")
		  Self.X = d.Value("x")
		  Self.Y = d.Value("y")
		  
		  Self.Memory = New Dictionary
		  
		  // Get the memory values specified in the test.
		  // Just look at the state of this code just to extract the values from JSON!
		  If d.HasKey("ram") Then
		    Var ramArray() As Variant = d.Value("ram")
		    For Each entry As Variant In ramArray
		      Var addressDataPair() As Variant = entry
		      Var address As UInt16 = addressDataPair(0)
		      Var data As UInt8 = addressDataPair(1)
		      Self.Memory.Value(address) = data
		    Next entry
		  End If
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0, Description = 52657475726E732061206E6577204350552073746174652C2074616B696E67206974732076616C7565732066726F6D2074686520706173736564204350552C2073657474696E6720746865206D656D6F72792076616C75657320746F206F6E6C792074686F736520696E20746865206578706563746564204350552073746174652E
		Shared Function FromCPUAndExpected(expectedState As CPUState, cpu As MOS6502.CPU) As CPUState
		  /// Returns a new CPU state, taking its values from the passed CPU, setting the memory values to only 
		  /// those in the expected CPU state.
		  
		  Var state As New CPUState
		  
		  state.A = cpu.A
		  state.P = cpu.P
		  state.PC = cpu.PC
		  state.SP = cpu.SP
		  state.X = cpu.X
		  state.Y = cpu.Y
		  
		  For Each entry As DictionaryEntry In expectedState.Memory
		    state.Memory.Value(entry.Key) = cpu.Memory(entry.Key)
		  Next entry
		  
		  Return state
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h21, Description = 4C656674206A75737469666965732060736020746F20607769647468602063686172616374657273207573696E672060636861726020746F207061642074686520726967687420656467652069662072657175697265642E
		Private Function JustifyLeft(s As String, width As Integer, char As String = " ") As String
		  /// Left justifies `s` to `width` characters using `char` to pad the right edge if required.
		  ///
		  /// ```xojo
		  /// "Hello".JustifyLeft(10) // Becomes "Hello     "
		  /// ```
		  
		  Var padCount As Integer = width - s.Length
		  
		  // Quick escape?
		  If padCount <= 0 Then Return s
		  
		  Var padding() As String
		  For i As Integer = 1 To padCount
		    padding.Add(char)
		  Next i
		  
		  Return s + String.FromArray(padding, "")
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0, Description = 52657475726E73206120737472696E6720726570726573656E746174696F6E206F662074686973204350552073746174652E
		Function ToString() As String
		  /// Returns a string representation of this CPU state.
		  
		  Var s() As String
		  
		  s.Add("A:  " + Self.A.ToString)
		  s.Add("P:  " + Self.P.ToString)
		  s.Add("PC: " + Self.PC.ToString + " (" + Self.PC.ToHex(4) + ")")
		  
		  Var actualSP As UInt16 = &h0100 + Self.SP
		  s.Add("SP: " + Self.SP.ToString + " (Address: " + actualSP.ToString + ")")
		  
		  
		  s.Add("X:  " + Self.X.ToString)
		  s.Add("Y:  " + Self.Y.ToString)
		  
		  s.Add("")
		  s.Add("N V - B D I Z C")
		  
		  Var N As String = If(NegativeFlag, "1 ", "0 ")
		  Var V As String = If(OverflowFlag, "1 ", "0 ")
		  Var B As String = If(BreakFlag, "1 ", "0 ")
		  Var D As String = If(DecimalFlag, "1 ", "0 ")
		  Var I As String = If(InterruptDisableFlag, "1 ", "0 ")
		  Var Z As String = If(ZeroFlag, "1 ", "0 ")
		  Var C As String = If(CarryFlag, "1 ", "0 ")
		  Var bit5 As String = If(StatusBit5, "1 ", "0 ")
		  s.Add(N + V + bit5 + B + D + I + Z + C)
		  s.Add("")
		  
		  If Self.Memory.KeyCount > 0 Then
		    s.Add("")
		    
		    s.Add("------")
		    s.Add("Memory")
		    s.Add("------")
		    s.Add("Address | Value")
		    For Each entry As DictionaryEntry In Self.Memory
		      Var address As UInt16 = entry.Key
		      Var value As UInt8 = entry.Value
		      Var hexValue As String = value.ToHex(2)
		      
		      Var addressStr As String = address.ToString
		      addressStr = JustifyLeft(addressStr, 8, " ")
		      s.Add(addressStr + "| " + JustifyLeft(value.ToString, 5, " ") + " (" + hexValue + ")")
		    Next entry
		  End If
		  
		  Return String.FromArray(s, EndOfLine)
		End Function
	#tag EndMethod


	#tag Property, Flags = &h0
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

	#tag Property, Flags = &h0, Description = 4B6579203D2061646472657373202855496E743136292C2056616C7565203D2064617461202855496E7438292E
		Memory As Dictionary
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

	#tag Property, Flags = &h0, Description = 382D62697420737461636B20706F696E7465722E
		SP As UInt8
	#tag EndProperty

	#tag ComputedProperty, Flags = &h0
		#tag Getter
			Get
			  Return (P And &b00100000) <> 0
			End Get
		#tag EndGetter
		StatusBit5 As Boolean
	#tag EndComputedProperty

	#tag Property, Flags = &h0, Description = 54686520756E697175652074657374206E616D652E
		TestName As String
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
			Name="P"
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
			Name="SP"
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
			Name="TestName"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="String"
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
