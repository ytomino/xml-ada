with Ada.IO_Exceptions;
private with Ada.Finalization;
private with C.libxml.encoding;
private with C.libxml.xmlreader;
private with C.libxml.xmlwriter;
package XML is
	pragma Preelaborate;
	pragma Linker_Options ("-lxml2");
	
	function Version return String;
	
	procedure Check_Version;
	procedure Cleanup; -- do finalization
	
	type Encoding_Type is private;
	
	function No_Encoding return Encoding_Type;
	function Find (Name : String) return Encoding_Type;
	function Name (Encoding : Encoding_Type) return String;
	
	type Standalone_Type is (No_Specific, No, Yes);
	for Standalone_Type use (No_Specific => -1, No => 0, Yes => 1);
	
	package Event_Types is
		type Event_Type is (
			No_Event,
			Element_Start,
			Attribute,
			Text,
			CDATA,
			Entity_Reference,
			Entity_Start,
			Processing_Instruction, -- <?xml-stylesheet ...?>
			Comment,
			Document, -- not used
			Document_Type, -- <!DOCTYPE ...>
			Document_Fragment,
			Notation,
			Whitespace,
			Significant_Whitespace,
			Element_End,
			Entity_End,
			XML_Declaration); -- <?xml ...?>, not used
	private
		use C.libxml.xmlreader;
		for Event_Type use (
			No_Event =>
				xmlReaderTypes'Enum_Rep (XML_READER_TYPE_NONE),
			Element_Start =>
				xmlReaderTypes'Enum_Rep (XML_READER_TYPE_ELEMENT),
			Attribute =>
				xmlReaderTypes'Enum_Rep (XML_READER_TYPE_ATTRIBUTE),
			Text =>
				xmlReaderTypes'Enum_Rep (XML_READER_TYPE_TEXT),
			CDATA =>
				xmlReaderTypes'Enum_Rep (XML_READER_TYPE_CDATA),
			Entity_Reference =>
				xmlReaderTypes'Enum_Rep (XML_READER_TYPE_ENTITY_REFERENCE),
			Entity_Start =>
				xmlReaderTypes'Enum_Rep (XML_READER_TYPE_ENTITY),
			Processing_Instruction =>
				xmlReaderTypes'Enum_Rep (XML_READER_TYPE_PROCESSING_INSTRUCTION),
			Comment =>
				xmlReaderTypes'Enum_Rep (XML_READER_TYPE_COMMENT),
			Document =>
				xmlReaderTypes'Enum_Rep (XML_READER_TYPE_DOCUMENT),
			Document_Type =>
				xmlReaderTypes'Enum_Rep (XML_READER_TYPE_DOCUMENT_TYPE),
			Document_Fragment =>
				xmlReaderTypes'Enum_Rep (XML_READER_TYPE_DOCUMENT_FRAGMENT),
			Notation =>
				xmlReaderTypes'Enum_Rep (XML_READER_TYPE_NOTATION),
			Whitespace =>
				xmlReaderTypes'Enum_Rep (XML_READER_TYPE_WHITESPACE),
			Significant_Whitespace =>
				xmlReaderTypes'Enum_Rep (XML_READER_TYPE_SIGNIFICANT_WHITESPACE),
			Element_End =>
				xmlReaderTypes'Enum_Rep (XML_READER_TYPE_END_ELEMENT),
			Entity_End =>
				xmlReaderTypes'Enum_Rep (XML_READER_TYPE_END_ENTITY),
			XML_Declaration =>
				xmlReaderTypes'Enum_Rep (XML_READER_TYPE_XML_DECLARATION));
	end Event_Types;
	type Event_Type is new Event_Types.Event_Type;
	
	type Event (Event_Type : XML.Event_Type) is record
		case Event_Type is
			when Element_Start | Attribute | Processing_Instruction | Document_Type =>
				Name : not null access constant String;
				case Event_Type is
					when Attribute =>
						Value : not null access constant String;
					when Document_Type =>
						Public_Id : access constant String;
						System_Id : access constant String;
						Subset : access constant String;
					when others =>
						null;
				end case;
			when Text | CDATA | Comment | Whitespace | Significant_Whitespace =>
				Content : not null access constant String;
			when others =>
				null;
		end case;
	end record;
	
	-- reader
	
	type Reader (<>) is limited private;
	
	function Create (
		Input : not null access procedure (Item : out String; Last : out Natural);
		Encoding : Encoding_Type := No_Encoding;
		URI : String := "")
		return Reader;
	
	procedure Set_DTD_Loading (
		Object : in out Reader;
		Value : in Boolean);
	procedure Set_Default_Attributes (
		Object : in out Reader;
		Value : in Boolean);
	procedure Set_Validation (
		Object : in out Reader;
		Value : in Boolean);
	procedure Set_Substitute_Entities (
		Object : in out Reader;
		Value : in Boolean);
	
	function Version (Object : Reader) return access constant String;
	function Encoding (Object : Reader) return Encoding_Type;
	function Standalone (Object : Reader) return Standalone_Type;
	
	function Base_URI (Object : Reader) return String;
	
	procedure Read (
		Object : in out Reader;
		Process : not null access procedure (Event : in XML.Event));
	
	procedure Read_Until_Element_End (
		Object : in out Reader);
	
	-- writer
	
	type Writer (<>) is limited private;
	
	function Create (
		Output : not null access procedure (Item : in String);
		Encoding : Encoding_Type := No_Encoding;
		Version : access constant String := null;
		Standalone : Standalone_Type := No_Specific)
		return Writer;
	
	procedure Set_Indent (Object : in out Writer; Indent : in Natural);
	procedure Set_Indent (Object : in out Writer; Indent : in String);
	
	procedure Write (Object : in out Writer; Event : in XML.Event);
	
	procedure Flush (Object : in out Writer);
	procedure Finish (Object : in out Writer);
	
	-- exceptions
	
	Status_Error : exception
		renames Ada.IO_Exceptions.Status_Error;
	Name_Error : exception
		renames Ada.IO_Exceptions.Name_Error;
	Use_Error : exception
		renames Ada.IO_Exceptions.Use_Error;
	Data_Error : exception
		renames Ada.IO_Exceptions.Data_Error;
	
private
	
	type Encoding_Type is new C.libxml.encoding.xmlCharEncodingHandlerPtr;
	
	type String_Access is access String;
	
	-- reader
	
	type Reader_State is (
		Normal,
		Empty_Element); -- have to supplement Element_End
	pragma Discard_Names (Reader_State);
	
	procedure Next (Object : in out Reader);
	
	package Readers is
		
		type Reader is limited private;
		
		function Raw (X : Reader)
			return not null access C.libxml.xmlreader.xmlTextReaderPtr;
		pragma Inline (Raw);
		
		function State (X : Reader) return not null access Reader_State;
		pragma Inline (State);
		
		function Version (X : Reader) return not null access String_Access;
		pragma Inline (Version);
		
	private
		
		type Reader is new Ada.Finalization.Limited_Controlled with record
			Raw : aliased C.libxml.xmlreader.xmlTextReaderPtr := null;
			State : aliased Reader_State := Normal;
			Version : aliased String_Access := null;
		end record;
		
		overriding procedure Finalize (Object : in out Reader);
	
	end Readers;
	
	type Reader is new Readers.Reader;
	
	-- writer
	
	procedure Write_Document_Start (
		Object : in out Writer;
		Version : access constant String;
		Encoding : Encoding_Type;
		Standalone : Standalone_Type);
	procedure Write_Document_End (
		Object : in out Writer);
	
	package Writers is
		
		type Writer is limited private;
		
		function Raw (X : Writer)
			return not null access C.libxml.xmlwriter.xmlTextWriterPtr;
		pragma Inline (Raw);
		
		function Finished (X : Writer) return not null access Boolean;
		pragma Inline (Finished);
		
	private
		
		type Writer is new Ada.Finalization.Limited_Controlled with record
			Raw : aliased C.libxml.xmlwriter.xmlTextWriterPtr := null;
			Finished : aliased Boolean := False;
		end record;
		
		overriding procedure Finalize (Object : in out Writer);
		
	end Writers;
	
	type Writer is new Writers.Writer;
	
	-- exceptions
	
	procedure Install_Error_Handlers;
	
	procedure Clear_Last_Error;
	procedure Raise_Last_Error;
	pragma No_Return (Raise_Last_Error);
	
	procedure Check_No_Zero (S : in String);
	
end XML;
