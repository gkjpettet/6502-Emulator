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
		Sub Opcode46Test()
		  Run("46")
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
		Sub Opcode4ATest()
		  Run("4A")
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub Opcode4CTest()
		  Run("4C")
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub Opcode4DTest()
		  Run("4D")
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub Opcode4ETest()
		  Run("4E")
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub Opcode50Test()
		  Run("50")
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
		Sub Opcode56Test()
		  Run("56")
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
		Sub Opcode5ETest()
		  Run("5E")
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub Opcode60Test()
		  Run("60")
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub Opcode61Test()
		  Run("61")
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub Opcode65Test()
		  Run("65")
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub Opcode66Test()
		  Run("66")
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub Opcode68Test()
		  Run("68")
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub Opcode69Test()
		  Run("69")
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub Opcode69Test1()
		  Run("69")
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub Opcode6ATest()
		  Run("6A")
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub Opcode6CTest()
		  Run("6C")
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub Opcode6DTest()
		  Run("6D")
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub Opcode6ETest()
		  Run("6E")
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub Opcode70Test()
		  Run("70")
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub Opcode71Test()
		  Run("71")
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub Opcode75Test()
		  Run("75")
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub Opcode76Test()
		  Run("76")
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub Opcode78Test()
		  Run("78")
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub Opcode79Test()
		  Run("79")
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub Opcode7DTest()
		  Run("7D")
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub Opcode7ETest()
		  Run("7E")
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub Opcode81Test()
		  Run("81")
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub Opcode84Test()
		  Run("84")
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub Opcode85Test()
		  Run("85")
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub Opcode86Test()
		  Run("86")
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub Opcode88Test()
		  Run("88")
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub Opcode8ATest()
		  Run("8A")
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub Opcode8CTest()
		  Run("8C")
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub Opcode8DTest()
		  Run("8D")
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub Opcode8ETest()
		  Run("8E")
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub Opcode90Test()
		  Run("90")
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub Opcode91Test()
		  Run("91")
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub Opcode94Test()
		  Run("94")
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub Opcode95Test()
		  Run("95")
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub Opcode96Test()
		  Run("96")
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub Opcode98Test()
		  Run("98")
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub Opcode99Test()
		  Run("99")
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub Opcode9ATest()
		  Run("9A")
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub Opcode9DTest()
		  Run("9D")
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub OpcodeA0Test()
		  Run("A0")
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub OpcodeA1Test()
		  Run("A1")
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub OpcodeA2Test()
		  Run("A2")
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub OpcodeA4Test()
		  Run("A4")
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub OpcodeA5Test()
		  Run("A5")
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub OpcodeA6Test()
		  Run("A6")
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub OpcodeA8Test()
		  Run("A8")
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub OpcodeA9Test()
		  Run("A9")
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub OpcodeAATest()
		  Run("AA")
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub OpcodeACTest()
		  Run("AC")
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub OpcodeADTest()
		  Run("AD")
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub OpcodeAETest()
		  Run("AE")
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub OpcodeB0Test()
		  Run("B0")
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub OpcodeB1Test()
		  Run("B1")
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub OpcodeB4Test()
		  Run("B4")
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub OpcodeB5Test()
		  Run("B5")
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub OpcodeB6Test()
		  Run("B6")
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub OpcodeB8Test()
		  Run("B8")
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub OpcodeB9Test()
		  Run("B9")
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub OpcodeBATest()
		  Run("BA")
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub OpcodeBCTest()
		  Run("BC")
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub OpcodeBDTest()
		  Run("BD")
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub OpcodeBETest()
		  Run("BE")
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub OpcodeC0Test()
		  Run("C0")
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub OpcodeC1Test()
		  Run("C1")
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub OpcodeC4Test()
		  Run("C4")
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub OpcodeC5Test()
		  Run("C5")
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub OpcodeC6Test()
		  Run("C6")
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub OpcodeC8Test()
		  Run("C8")
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub OpcodeC9Test()
		  Run("C9")
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub OpcodeCATest()
		  Run("CA")
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub OpcodeCCTest()
		  Run("CC")
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub OpcodeCDTest()
		  Run("CD")
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub OpcodeCETest()
		  Run("CE")
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub OpcodeD0Test()
		  Run("D0")
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub OpcodeD1Test()
		  Run("C1")
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub OpcodeD5Test()
		  Run("D5")
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub OpcodeD6Test()
		  Run("D6")
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub OpcodeD8Test()
		  Run("D8")
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub OpcodeD9Test()
		  Run("D9")
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub OpcodeDDTest()
		  Run("DD")
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub OpcodeDETest()
		  Run("DE")
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub OpcodeE0Test()
		  Run("E0")
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub OpcodeE1Test()
		  Run("E1")
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub OpcodeE1Test1()
		  Run("E1")
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub OpcodeE4Test()
		  Run("E4")
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub OpcodeE5Test()
		  Run("E5")
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub OpcodeE6Test()
		  Run("E6")
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub OpcodeE8Test()
		  Run("E8")
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub OpcodeE9Test()
		  Run("E9")
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub OpcodeEATest()
		  Run("EA")
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub OpcodeECTest()
		  Run("EC")
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub OpcodeEDTest()
		  Run("ED")
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub OpcodeEETest()
		  Run("EE")
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub OpcodeF0Test()
		  Run("F0")
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub OpcodeF1Test()
		  Run("F1")
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub OpcodeF5Test()
		  Run("F5")
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub OpcodeF6Test()
		  Run("F6")
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub OpcodeF8Test()
		  Run("F8")
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub OpcodeF9Test()
		  Run("F9")
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub OpcodeFDTest()
		  Run("FD")
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub OpcodeFETest()
		  Run("FE")
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
		    
		    ' if test.Value("name") = "61 de e9" then
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
