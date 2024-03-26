#tag Class
Protected Class Memory
	#tag Method, Flags = &h0, Description = 437265617465732061206E657720626C6F636B206F66206D656D6F727920776974682074686520737065636966696564206E756D626572206F662062797465732E2044656661756C747320746F203635353335202836344B206279746573292E
		Sub Constructor(size As Integer = 65536)
		  /// Creates a new block of memory with the specified number of bytes.
		  /// Defaults to 65536 (64K bytes).
		  
		  Data.ResizeTo(size - 1)
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0, Description = 416C6C6F7773207468652076616C7565206174206120646972656374206D656D6F7279206164647265737320746F206265206163636573736564207573696E6720737562736372697074206E6F6D656E636C61747572652E
		Function Operator_Subscript(index As Integer) As UInt8
		  /// Allows the value at a direct memory address to be accessed using subscript nomenclature.
		  
		  Return Data(index)
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0, Description = 416C6C6F7773207468652076616C7565206174206120646972656374206D656D6F7279206164647265737320746F206265206163636573736564207573696E6720737562736372697074206E6F6D656E636C61747572652E
		Sub Operator_Subscript(index As Integer, Assigns value As UInt8)
		  /// Allows the value at a direct memory address to be accessed using subscript nomenclature.
		  
		  Data(index) = value
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0, Description = 5265616473207468652062797465206174207468652073706563696669656420616464726573732E
		Function Read(address As UInt16) As UInt8
		  /// Reads the byte at the specified address.
		  
		  Return Data(address)
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0, Description = 577269746573206076616C75656020746F2074686520737065636966696564206061646472657373602E
		Sub Write(address As UInt16, value As UInt8)
		  /// Writes `value` to the specified `address`.
		  
		  Data(address) = value
		  
		End Sub
	#tag EndMethod


	#tag Property, Flags = &h21
		Private Data() As UInt8
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
	#tag EndViewBehavior
End Class
#tag EndClass
