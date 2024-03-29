#tag Class
Protected Class TempTests
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
		Sub Opcode8DTest()
		  Run("8D")
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
		    
		    ' if initial.TestName = "66 8e f6" then
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
