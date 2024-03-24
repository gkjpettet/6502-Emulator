#tag Module
Protected Module MOS6502
	#tag Enum, Name = AddressModes, Type = Integer, Flags = &h1
		Absolute
		  AbsoluteIndirect
		  Accumulator
		  Imediate
		  Implied
		  Relative
		  XIndexedAbsolute
		  XIndexedZeroPage
		  XIndexedZeroPageIndirect
		  YIndexedAbsolute
		  YIndexedZeroPage
		  ZeroPage
		ZeroPageIndirectYIndexed
	#tag EndEnum


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
End Module
#tag EndModule
