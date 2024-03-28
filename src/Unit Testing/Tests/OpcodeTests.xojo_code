#tag Class
Protected Class OpcodeTests
Inherits TestGroup
	#tag Event
		Sub Setup()
		  ' CPU = New MOS6502.CPU(New MOS6502.Memory)
		  ' CPU.Reset
		End Sub
	#tag EndEvent

	#tag Event
		Function UnhandledException(err As RuntimeException, methodName As String) As Boolean
		  #pragma unused err
		  
		  Const kMethodName As Text = "UnhandledException"
		  
		  If methodName.Length >= kMethodName.Length And methodName.Left(kMethodName.Length) = kMethodName Then
		    Assert.Pass("Exception was handled")
		    Return True
		  End If
		End Function
	#tag EndEvent


	#tag Method, Flags = &h21, Description = 52657475726E732074686520636F6E74656E7473206F6620746865204A534F4E207465732066696C6520666F722074686520737065636966696564206F70636F64652028696E20686578292E
		Private Function GetTestFileContents(hexOpcode As String) As String
		  /// Returns the contents of the JSON tes file for the specified opcode (in hex).
		  
		  Var f As FolderItem = SpecialFolder.Resource("tests").Child("opcodes").Child(hexOpcode + ".json")
		  
		  Var tin As TextInputStream = TextInputStream.Open(f)
		  
		  Var contents As String = tin.ReadAll
		  
		  tin.Close
		  
		  Return contents
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub Opcode00Test()
		  Run("00")
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub Opcode01Test()
		  Run("01")
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub Opcode05Test()
		  Run("05")
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub Opcode06Test()
		  Run("06")
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub Opcode08Test()
		  Run("08")
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub Opcode09Test()
		  Run("09")
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub Opcode0ATest()
		  Run("0A")
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub Opcode0DTest()
		  Run("0D")
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub Opcode0ETest()
		  Run("0E")
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub Opcode10Test()
		  Run("10")
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub Opcode11Test()
		  Run("11")
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub Opcode15Test()
		  Run("15")
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub Opcode16Test()
		  Run("16")
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub Opcode18Test()
		  Run("18")
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub Opcode19Test()
		  Run("19")
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub Opcode1DTest()
		  Run("1D")
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub Opcode1ETest()
		  Run("1E")
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub Opcode20Test()
		  Run("20")
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub Opcode21Test()
		  Run("21")
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub Opcode24Test()
		  Run("24")
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub Opcode25Test()
		  Run("25")
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub Opcode26Test()
		  Run("26")
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub Opcode28Test()
		  Run("28")
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub Opcode29Test()
		  Run("29")
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub Opcode2ATest()
		  Run("2A")
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub Opcode2CTest()
		  Run("2C")
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub Opcode2DTest()
		  Run("2D")
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub Opcode2ETest()
		  Run("2E")
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub Opcode30Test()
		  Run("30")
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub Opcode31Test()
		  Run("31")
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub Opcode35Test()
		  Run("35")
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub Opcode36Test()
		  Run("36")
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub Opcode38Test()
		  Run("38")
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub Opcode39Test()
		  Run("39")
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub Opcode3DTest()
		  Run("3D")
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub Opcode3ETest()
		  Run("3E")
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub Opcode40Test()
		  Run("40")
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub Opcode41Test()
		  Run("41")
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub Opcode45Test()
		  Run("45")
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub Opcode48Test()
		  Run("48")
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub Opcode49Test()
		  Run("49")
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub Opcode4DTest()
		  Run("4D")
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub Opcode51Test()
		  Run("51")
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub Opcode55Test()
		  Run("55")
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub Opcode58Test()
		  Run("58")
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub Opcode59Test()
		  Run("59")
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub Opcode5DTest()
		  Run("5D")
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub Opcode60Test()
		  Run("60")
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub OpcodeF8Test()
		  Run("F8")
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0, Description = 52756E732074686520746573747320666F722074686520737065636966696564206F70636F64652028696E20686578292E
		Sub Run(hexOpcode As String)
		  /// Runs the tests for the specified opcode (in hex).
		  
		  // Get the JSON contents of the test file.
		  Var json As String = GetTestFileContents(hexOpcode)
		  
		  Var tests() As Variant = ParseJSON(json)
		  
		  // Run each test.
		  For Each test As Dictionary In tests
		    CPU = New MOS6502.CPU(New MOS6502.Memory)
		    CPU.Reset
		    
		    Var initial As New CPUState(test.Value("initial"), test.Lookup("name", ""))
		    Var expected As New CPUState(test.Value("final"), test.Lookup("name", ""))
		    SetCPUState(initial)
		    
		    ' if test.Value("name") = "40 f5 73" then
		    ' break
		    ' end if
		    
		    CPU.Execute
		    Assert.StatesEqual(CPU, expected, initial)
		  Next test
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h21, Description = 536574732074686520435055277320737461746520746F207468652076616C756573207061737365642E
		Private Sub SetCPUState(state As CPUState)
		  /// Sets the CPU's state to the values passed.
		  
		  CPU.Memory = New MOS6502.Memory
		  CPU.Reset
		  
		  CPU.A = state.A
		  CPU.P = state.P
		  CPU.PC = state.PC
		  CPU.SP = state.SP
		  CPU.X = state.X
		  CPU.Y = state.Y
		  
		  For Each entry As DictionaryEntry In state.Memory
		    Var address As UInt16 = entry.Key
		    Var data As UInt8 = entry.Value
		    CPU.Memory(address) = data
		  Next entry
		  
		End Sub
	#tag EndMethod


	#tag Property, Flags = &h0
		CPU As MOS6502.CPU
	#tag EndProperty


	#tag ViewBehavior
		#tag ViewProperty
			Name="Duration"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="FailedTestCount"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Integer"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="IncludeGroup"
			Visible=false
			Group="Behavior"
			InitialValue="True"
			Type="Boolean"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="IsRunning"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Boolean"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="NotImplementedCount"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Integer"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="PassedTestCount"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Integer"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="RunTestCount"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Integer"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="SkippedTestCount"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Integer"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="StopTestOnFail"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Boolean"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="TestCount"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Integer"
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
			Name="Left"
			Visible=true
			Group="Position"
			InitialValue="0"
			Type="Integer"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="Name"
			Visible=true
			Group="ID"
			InitialValue=""
			Type="String"
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
			Name="Top"
			Visible=true
			Group="Position"
			InitialValue="0"
			Type="Integer"
			EditorType=""
		#tag EndViewProperty
	#tag EndViewBehavior
End Class
#tag EndClass
