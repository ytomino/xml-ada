with Ada.Unchecked_Conversion;
with Ada.Unchecked_Deallocation;
with System;
with C.string;
with C.libxml.globals;
with C.libxml.parser;
with C.libxml.tree;
with C.libxml.xmlerror;
with C.libxml.xmlIO;
with C.libxml.xmlmemory;
with C.libxml.xmlstring;
with C.libxml.xmlversion;
package body XML is
	use type System.Address;
	use type C.char_array;
	use type C.signed_int;
	use type C.libxml.encoding.xmlCharEncodingHandlerPtr;
	use type C.libxml.xmlstring.xmlChar_const_ptr;
	use type C.libxml.tree.xmlOutputBufferPtr;
	use type C.libxml.xmlerror.xmlErrorPtr;
	use type C.libxml.xmlreader.xmlTextReaderPtr;
	use type C.libxml.xmlwriter.xmlTextWriterPtr;
	
	procedure Free is new Ada.Unchecked_Deallocation (String, String_Access);
	
	type xmlChar_array is array (C.size_t range <>) of
		aliased C.libxml.xmlstring.xmlChar;
	pragma Convention (C, xmlChar_array);
	
	function To_char_const_ptr is new Ada.Unchecked_Conversion (
		C.libxml.xmlstring.xmlChar_const_ptr,
		C.char_const_ptr);
	
	function To_String (S : not null access constant C.char) return String is
		Length : constant Natural := Natural (C.string.strlen (S));
		Result : String (1 .. Length);
		for Result'Address use S.all'Address;
	begin
		return Result;
	end To_String;
	
	-- dirty trick
	function Copy_String_Access (
		S : not null String_Access;
		Constraint : not null access String_Constraint)
		return String_Access
	is
		type Fat_Type is record
			Data : System.Address;
			Constraints : System.Address;
		end record;
		Fat : Fat_Type;
		Result : String_Access;
		for Fat'Address use Result'Address;
	begin
		Fat.Data := S.all'Address;
		Fat.Constraints := Constraint.all'Address;
		Constraint.First := S'First;
		Constraint.Last := S'Last;
		return Result;
	end Copy_String_Access;
	
	Version_Checked : Boolean := False;
	
	Error_Handler_Installed : Boolean := False;
	
	procedure Structured_Error_Handler (
		userData : C.void_ptr;
		error : access C.libxml.xmlerror.xmlError);
	pragma Convention (C, Structured_Error_Handler);
	procedure Structured_Error_Handler (
		userData : C.void_ptr;
		error : access C.libxml.xmlerror.xmlError) is
	begin
		null; -- suppress fprintf
	end Structured_Error_Handler;
	
	function Read_Handler (
		context : C.void_ptr;
		buffer : access C.char;
		len : C.signed_int)
		return C.signed_int;
	pragma Convention (C, Read_Handler);
	function Read_Handler (
		context : C.void_ptr;
		buffer : access C.char;
		len : C.signed_int)
		return C.signed_int
	is
		procedure Input (Item : out String; Last : out Natural);
		pragma Import (Ada, Input);
		for Input'Address use System.Address (context);
		Item : String (1 .. Natural (len));
		for Item'Address use buffer.all'Address;
		Last : Natural;
	begin
		Input (Item, Last);
		return C.signed_int (Last);
	end Read_Handler;
	
	function Write_Handler (
		context : C.void_ptr;
		buffer : access constant C.char;
		len : C.signed_int)
		return C.signed_int;
	pragma Convention (C, Write_Handler);
	function Write_Handler (
		context : C.void_ptr;
		buffer : access constant C.char;
		len : C.signed_int)
		return C.signed_int
	is
		procedure Output (Item : in String);
		pragma Import (Ada, Output);
		for Output'Address use System.Address (context);
		Item : String (1 .. Natural (len));
		for Item'Address use buffer.all'Address;
	begin
		Output (Item);
		return len;
	end Write_Handler;
	
	-- for standalone
	No_Image : constant C.char_array := "no" & C.char'Val (0);
	Yes_Image : constant C.char_array := "yes" & C.char'Val (0);
	Standalone_Image : constant
		array (C.signed_int range -1 .. 1) of C.char_const_ptr := (
			Standalone_Type'Enum_Rep (No_Specific) => null,
			Standalone_Type'Enum_Rep (No) => No_Image (No_Image'First)'Access,
			Standalone_Type'Enum_Rep (Yes) => Yes_Image (Yes_Image'First)'Access);
	
	-- reading one event
	
	procedure Read (
		Object : in out Reader;
		Parsed_Data : out Parsed_Data_Type)
	is
		NC_Object : Non_Controlled_Reader
			renames Controlled_Readers.Reference (Object).all;
	begin
		Clear_Last_Error;
		case NC_Object.State is
			when Next | Remaining =>
				if NC_Object.State = Remaining then
					Next (Object);
				end if;
				declare
					Node_Type : constant C.signed_int :=
						C.libxml.xmlreader.xmlTextReaderNodeType (NC_Object.Raw);
				begin
					if Node_Type < 0 then
						Raise_Last_Error;
					else
						case Node_Type is
							when C.libxml.xmlreader.xmlReaderTypes'Enum_Rep (
								C.libxml.xmlreader.XML_READER_TYPE_NONE)
							=>
								Parsed_Data.Event := (Event_Type => No_Event);
								NC_Object.State := Remaining;
							when C.libxml.xmlreader.xmlReaderTypes'Enum_Rep (
								C.libxml.xmlreader.XML_READER_TYPE_ATTRIBUTE)
							=>
								declare
									C_Name : constant C.libxml.xmlstring.xmlChar_const_ptr :=
										C.libxml.xmlreader.xmlTextReaderConstName (
											NC_Object.Raw);
									Name : aliased String (
										1 ..
										Natural (C.string.strlen (To_char_const_ptr (C_Name))));
									for Name'Address use C_Name.all'Address;
									C_Value : constant C.libxml.xmlstring.xmlChar_const_ptr :=
										C.libxml.xmlreader.xmlTextReaderConstValue (
											NC_Object.Raw);
									Value : aliased String (
										1 ..
										Natural (C.string.strlen (To_char_const_ptr (C_Value))));
									for Value'Address use C_Value.all'Address;
								begin
									Parsed_Data.Event := (
										Event_Type => Attribute,
										Name => Copy_String_Access (
											Name'Unrestricted_Access,
											Parsed_Data.Name_Constraint'Access),
										Value => Copy_String_Access (
											Value'Unrestricted_Access,
											Parsed_Data.Value_Constraint'Access));
								end;
								declare
									Moved : constant C.signed_int :=
										C.libxml.xmlreader.xmlTextReaderMoveToNextAttribute (
											NC_Object.Raw);
								begin
									if Moved < 0 then
										Raise_Last_Error;
									elsif Moved > 0 then
										NC_Object.State := Next; -- more attributes
									else
										-- end of attributes
										if C.libxml.xmlreader.xmlTextReaderMoveToElement (
											NC_Object.Raw) < 0
										then
											Raise_Last_Error;
										end if;
										if C.libxml.xmlreader.xmlTextReaderIsEmptyElement (
											NC_Object.Raw) > 0
										then
											NC_Object.State := Empty_Element;
										else -- move to children
											NC_Object.State := Remaining;
										end if;
									end if;
								end;
							when C.libxml.xmlreader.xmlReaderTypes'Enum_Rep (
								C.libxml.xmlreader.XML_READER_TYPE_ELEMENT)
							=>
								declare
									C_Name : constant C.libxml.xmlstring.xmlChar_const_ptr :=
										C.libxml.xmlreader.xmlTextReaderConstName (
											NC_Object.Raw);
									Name : aliased String (
										1 ..
										Natural (C.string.strlen (To_char_const_ptr (C_Name))));
									for Name'Address use C_Name.all'Address;
								begin
									Parsed_Data.Event := (
										Event_Type => Element_Start,
										Name => Copy_String_Access (
											Name'Unrestricted_Access,
											Parsed_Data.Name_Constraint'Access));
								end;
								if C.libxml.xmlreader.xmlTextReaderHasAttributes (
									NC_Object.Raw) > 0
								then
									NC_Object.State := Next;
									if C.libxml.xmlreader.xmlTextReaderMoveToFirstAttribute (
										NC_Object.Raw) < 0
									then
										Raise_Last_Error;
									end if;
								elsif C.libxml.xmlreader.xmlTextReaderIsEmptyElement (
									NC_Object.Raw) > 0
								then
									NC_Object.State := Empty_Element;
								else -- move to children
									NC_Object.State := Remaining;
								end if;
							when C.libxml.xmlreader.xmlReaderTypes'Enum_Rep (
								C.libxml.xmlreader.XML_READER_TYPE_TEXT)
							| C.libxml.xmlreader.xmlReaderTypes'Enum_Rep (
								C.libxml.xmlreader.XML_READER_TYPE_CDATA)
							| C.libxml.xmlreader.xmlReaderTypes'Enum_Rep (
								C.libxml.xmlreader.XML_READER_TYPE_COMMENT)
							| C.libxml.xmlreader.xmlReaderTypes'Enum_Rep (
								C.libxml.xmlreader.XML_READER_TYPE_WHITESPACE)
							| C.libxml.xmlreader.xmlReaderTypes'Enum_Rep (
								C.libxml.xmlreader.XML_READER_TYPE_SIGNIFICANT_WHITESPACE)
							=>
								declare
									C_Content : constant C.libxml.xmlstring.xmlChar_const_ptr :=
										C.libxml.xmlreader.xmlTextReaderConstValue (
											NC_Object.Raw);
									Content : aliased String (
										1 ..
										Natural (C.string.strlen (To_char_const_ptr (C_Content))));
									for Content'Address use C_Content.all'Address;
									Content_Access : constant String_Access := Copy_String_Access (
										Content'Unrestricted_Access,
										Parsed_Data.Content_Constraint'Access);
								begin
									case Node_Type is
										when C.libxml.xmlreader.xmlReaderTypes'Enum_Rep (
											C.libxml.xmlreader.XML_READER_TYPE_TEXT)
										=>
											Parsed_Data.Event := (
												Event_Type => Text,
												Content => Content_Access);
										when C.libxml.xmlreader.xmlReaderTypes'Enum_Rep (
											C.libxml.xmlreader.XML_READER_TYPE_CDATA)
										=>
											Parsed_Data.Event := (
												Event_Type => CDATA,
												Content => Content_Access);
										when C.libxml.xmlreader.xmlReaderTypes'Enum_Rep (
											C.libxml.xmlreader.XML_READER_TYPE_COMMENT)
										=>
											Parsed_Data.Event := (
												Event_Type => Comment,
												Content => Content_Access);
										when C.libxml.xmlreader.xmlReaderTypes'Enum_Rep (
											C.libxml.xmlreader.XML_READER_TYPE_WHITESPACE)
										=>
											Parsed_Data.Event := (
												Event_Type => Whitespace,
												Content => Content_Access);
										when C.libxml.xmlreader.xmlReaderTypes'Enum_Rep (
											C.libxml.xmlreader.XML_READER_TYPE_SIGNIFICANT_WHITESPACE)
										=>
											Parsed_Data.Event := (
												Event_Type => Significant_Whitespace,
												Content => Content_Access);
										when others =>
											pragma Assert (False);
											null;
									end case;
								end;
								NC_Object.State := Remaining;
							when C.libxml.xmlreader.xmlReaderTypes'Enum_Rep (
								C.libxml.xmlreader.XML_READER_TYPE_PROCESSING_INSTRUCTION)
							=>
								declare
									C_Name : constant C.libxml.xmlstring.xmlChar_const_ptr :=
										C.libxml.xmlreader.xmlTextReaderConstName (
											NC_Object.Raw);
									Name : aliased String (
										1 ..
										Natural (C.string.strlen (To_char_const_ptr (C_Name))));
									for Name'Address use C_Name.all'Address;
								begin
									Parsed_Data.Event := (
										Event_Type => Processing_Instruction,
										Name => Copy_String_Access (
											Name'Unrestricted_Access,
											Parsed_Data.Name_Constraint'Access));
									-- it's not able to get attributes info with libxml2 (?)
									pragma Assert (
										C.libxml.xmlreader.xmlTextReaderHasAttributes (
											NC_Object.Raw) = 0);
								end;
								NC_Object.State := Remaining;
							when C.libxml.xmlreader.xmlReaderTypes'Enum_Rep (
								C.libxml.xmlreader.XML_READER_TYPE_DOCUMENT_TYPE)
							=>
								declare
									C_Name : constant C.libxml.xmlstring.xmlChar_const_ptr :=
										C.libxml.xmlreader.xmlTextReaderConstName (
											NC_Object.Raw);
									Name : aliased String (
										1 ..
										Natural (C.string.strlen (To_char_const_ptr (C_Name))));
									for Name'Address use C_Name.all'Address;
								begin
									Parsed_Data.Event := (
										Event_Type => Document_Type,
										Name => Copy_String_Access (
											Name'Unrestricted_Access,
											Parsed_Data.Name_Constraint'Access),
										Public_Id => null,
										System_Id => null,
										Subset => null);
									-- it's not able to get extra DTD info with libxml2 (?)
									pragma Assert (
										C.libxml.xmlreader.xmlTextReaderHasAttributes (
											NC_Object.Raw) = 0);
								end;
								NC_Object.State := Remaining;
							when C.libxml.xmlreader.xmlReaderTypes'Enum_Rep (
								C.libxml.xmlreader.XML_READER_TYPE_END_ELEMENT)
							=>
								Parsed_Data.Event := (Event_Type => Element_End);
								NC_Object.State := Remaining;
							when others =>
								raise Program_Error
									with "in XML.Read, unimplemented" & Node_Type'Img;
						end case;
					end if;
				end;
			when Empty_Element =>
				Parsed_Data.Event := (Event_Type => Element_End);
				NC_Object.State := Remaining;
		end case;
	end Read;
	
	-- implementation
	
	function Version return String is
	begin
		Check_Version;
		return To_String (C.libxml.globals.xmlParserVersion);
	end Version;
	
	procedure Check_Version is
	begin
		if not Version_Checked then
			Version_Checked := True;
			C.libxml.xmlversion.xmlCheckVersion (C.libxml.xmlversion.LIBXML_VERSION);
		end if;
	end Check_Version;
	
	procedure Cleanup is
	begin
		C.libxml.parser.xmlCleanupParser;
		C.libxml.xmlmemory.xmlMemoryDump;
	end Cleanup;
	
	function No_Encoding return Encoding_Type is
	begin
		return null;
	end No_Encoding;
	
	function Find (Name : String) return Encoding_Type is
		Z_Name : String := Name & Character'Val (0);
		C_Name : C.char_array (0 .. Name'Length);
		for C_Name'Address use Z_Name'Address;
		Result : constant C.libxml.encoding.xmlCharEncodingHandlerPtr :=
			C.libxml.encoding.xmlFindCharEncodingHandler (
				C_Name (C_Name'First)'Access);
	begin
		if Result = null then
			raise Name_Error;
		end if;
		return Encoding_Type (Result);
	end Find;
	
	function Name (Encoding : Encoding_Type) return String is
	begin
		if Encoding = null then
			raise Constraint_Error;
		else
			return To_String (Encoding.name);
		end if;
	end Name;
	
	-- reader
	
	function Create (
		Input : not null access procedure (Item : out String; Last : out Natural);
		Encoding : Encoding_Type := No_Encoding;
		URI : String := "")
		return Reader is
	begin
		Check_Version;
		Install_Error_Handlers;
		declare
			P_Encoding : C.char_const_ptr := null;
			P_URI : access constant C.char := null;
			Z_URI : String (1 .. URI'Length + 1);
			C_URI : C.char_array (C.size_t);
			for C_URI'Address use Z_URI'Address;
		begin
			if Encoding /= null then
				P_Encoding := C.char_const_ptr (Encoding.name);
			end if;
			if URI'Length > 0 then
				Z_URI (1 .. URI'Length) := URI;
				Z_URI (Z_URI'Last) := Character'Val (0);
				P_URI := C_URI (C_URI'First)'Access;
			end if;
			return Result : Reader do
				declare
					NC_Result : Non_Controlled_Reader
						renames Controlled_Readers.Reference (Result).all;
				begin
					NC_Result.Raw := C.libxml.xmlreader.xmlReaderForIO (
						Read_Handler'Access,
						null,
						C.void_ptr (Input.all'Address),
						P_URI,
						P_Encoding,
						0);
					if NC_Result.Raw = null then
						raise Use_Error;
					end if;
				end;
				Next (Result);
			end return;
		end;
	end Create;
	
	procedure Set_DTD_Loading (
		Object : in out Reader;
		Value : in Boolean)
	is
		NC_Object : Non_Controlled_Reader
			renames Controlled_Readers.Reference (Object).all;
	begin
		if C.libxml.xmlreader.xmlTextReaderSetParserProp (
			NC_Object.Raw,
			C.libxml.xmlreader.xmlParserProperties'Enum_Rep (
				C.libxml.xmlreader.XML_PARSER_LOADDTD),
			Boolean'Pos (Value)) < 0
		then
			raise Use_Error;
		end if;
	end Set_DTD_Loading;
	
	procedure Set_Default_Attributes (
		Object : in out Reader;
		Value : in Boolean)
	is
		NC_Object : Non_Controlled_Reader
			renames Controlled_Readers.Reference (Object).all;
	begin
		if C.libxml.xmlreader.xmlTextReaderSetParserProp (
			NC_Object.Raw,
			C.libxml.xmlreader.xmlParserProperties'Enum_Rep (
				C.libxml.xmlreader.XML_PARSER_DEFAULTATTRS),
			Boolean'Pos (Value)) < 0
		then
			raise Use_Error;
		end if;
	end Set_Default_Attributes;
	
	procedure Set_Validation (
		Object : in out Reader;
		Value : in Boolean)
	is
		NC_Object : Non_Controlled_Reader
			renames Controlled_Readers.Reference (Object).all;
	begin
		if C.libxml.xmlreader.xmlTextReaderSetParserProp (
			NC_Object.Raw,
			C.libxml.xmlreader.xmlParserProperties'Enum_Rep (
				C.libxml.xmlreader.XML_PARSER_VALIDATE),
			Boolean'Pos (Value)) < 0
		then
			raise Use_Error;
		end if;
	end Set_Validation;
	
	procedure Set_Substitute_Entities (
		Object : in out Reader;
		Value : in Boolean)
	is
		NC_Object : Non_Controlled_Reader
			renames Controlled_Readers.Reference (Object).all;
	begin
		if C.libxml.xmlreader.xmlTextReaderSetParserProp (
			NC_Object.Raw,
			C.libxml.xmlreader.xmlParserProperties'Enum_Rep (
				C.libxml.xmlreader.XML_PARSER_SUBST_ENTITIES),
			Boolean'Pos (Value)) < 0
		then
			raise Use_Error;
		end if;
	end Set_Substitute_Entities;
	
	function Version (Object : Reader) return access constant String is
		Mutable_Object : Reader
			renames Object'Unrestricted_Access.all;
		NC_Object : Non_Controlled_Reader
			renames Controlled_Readers.Reference (Mutable_Object).all;
	begin
		if NC_Object.Version = null then
			declare
				C_Version : constant C.libxml.xmlstring.xmlChar_const_ptr :=
					C.libxml.xmlreader.xmlTextReaderConstXmlVersion (
						NC_Object.Raw);
			begin
				if C_Version /= null then
					declare
						A_Version : String (
							1 ..
							Natural (C.string.strlen (To_char_const_ptr (C_Version))));
						for A_Version'Address use C_Version.all'Address;
					begin
						NC_Object.Version := new String'(A_Version);
					end;
				end if;
			end;
		end if;
		return NC_Object.Version;
	end Version;
	
	function Encoding (Object : Reader) return Encoding_Type is
		NC_Object : Non_Controlled_Reader
			renames Controlled_Readers.Constant_Reference (Object).all;
	begin
		return Encoding_Type (
			C.libxml.encoding.xmlFindCharEncodingHandler (
				To_char_const_ptr (
					C.libxml.xmlreader.xmlTextReaderConstEncoding (
						NC_Object.Raw))));
	end Encoding;
	
	function Standalone (Object : Reader) return Standalone_Type is
		NC_Object : Non_Controlled_Reader
			renames Controlled_Readers.Constant_Reference (Object).all;
		Result : constant C.signed_int :=
			C.libxml.xmlreader.xmlTextReaderStandalone (NC_Object.Raw);
	begin
		return Standalone_Type'Enum_Val (Result);
	end Standalone;
	
	function Base_URI (Object : Reader) return String is
		NC_Object : Non_Controlled_Reader
			renames Controlled_Readers.Constant_Reference (Object).all;
	begin
		return To_String (
			To_char_const_ptr (
				C.libxml.xmlreader.xmlTextReaderConstBaseUri (NC_Object.Raw)));
	end Base_URI;
	
	procedure Read (
		Object : in out Reader;
		Process : not null access procedure (Event : in XML.Event))
	is
		Parsed_Data : Parsed_Data_Type;
	begin
		Read (Object, Parsed_Data);
		Process (Parsed_Data.Event);
	end Read;
	
	procedure Read (
		Object : in out Reader;
		Parsing_Entry : out Parsing_Entry_Type) is
	begin
		Read (Object, Parsing_Entry.Data);
	end Read;
	
	procedure Read_Until_Element_End (
		Object : in out Reader) is
	begin
		loop
			declare
				T : Event_Type;
			begin
				declare
					Parsed_Data : Parsed_Data_Type;
				begin
					Read (Object, Parsed_Data);
					T := Parsed_Data.Event.Event_Type;
				end;
				case T is
					when Element_Start =>
						Read_Until_Element_End (Object);
					when Element_End =>
						exit;
					when others =>
						null;
				end case;
			end;
		end loop;
	end Read_Until_Element_End;
	
	function Value (Parsing_Entry : Parsing_Entry_Type)
		return Event_Reference_Type is
	begin
		return (Element => Parsing_Entry.Data.Event'Access);
	end Value;
	
	procedure Next (Object : in out Reader) is
		NC_Object : Non_Controlled_Reader
			renames Controlled_Readers.Reference (Object).all;
	begin
		Clear_Last_Error;
		if C.libxml.xmlreader.xmlTextReaderRead (NC_Object.Raw) < 0 then
			Raise_Last_Error;
		end if;
	end Next;
	
	package body Controlled_Readers is
		
		function Constant_Reference (Object : XML.Reader)
			return not null access constant Non_Controlled_Reader is
		begin
			return Reader (Object).Data'Unchecked_Access;
		end Constant_Reference;
		
		function Reference (Object : in out XML.Reader)
			return not null access Non_Controlled_Reader is
		begin
			return Reader (Object).Data'Unrestricted_Access;
		end Reference;
		
		overriding procedure Finalize (Object : in out Reader) is
		begin
			C.libxml.xmlreader.xmlFreeTextReader (Object.Data.Raw);
			Free (Object.Data.Version);
		end Finalize;
		
	end Controlled_Readers;
	
	-- writer
	
	function Create (
		Output : not null access procedure (Item : in String);
		Encoding : Encoding_Type := No_Encoding;
		Version : access constant String := null;
		Standalone : Standalone_Type := No_Specific)
		return Writer is
	begin
		Check_Version;
		Install_Error_Handlers;
		declare
			Buffer : constant C.libxml.tree.xmlOutputBufferPtr :=
				C.libxml.xmlIO.xmlOutputBufferCreateIO (
					Write_Handler'Access,
					null,
					C.void_ptr (Output.all'Address),
					Encoding);
		begin
			if Buffer = null then
				raise Use_Error;
			end if;
			return Result : Writer do
				declare
					NC_Result : Non_Controlled_Writer
						renames Controlled_Writers.Reference (Result).all;
				begin
					NC_Result.Raw := C.libxml.xmlwriter.xmlNewTextWriter (Buffer);
					if NC_Result.Raw = null then
						declare
							Dummy : C.signed_int;
						begin
							Dummy := C.libxml.xmlIO.xmlOutputBufferClose (Buffer);
						end;
						raise Use_Error;
					end if;
				end;
				Write_Document_Start (
					Result,
					Version => Version,
					Encoding => Encoding,
					Standalone => Standalone);
			end return;
		end;
	end Create;
	
	procedure Set_Indent (Object : in out Writer; Indent : in Natural) is
		NC_Object : Non_Controlled_Writer
			renames Controlled_Writers.Reference (Object).all;
	begin
		if C.libxml.xmlwriter.xmlTextWriterSetIndent (
			NC_Object.Raw,
			C.signed_int (Indent)) < 0
		then
			raise Use_Error;
		end if;
	end Set_Indent;
	
	procedure Set_Indent (Object : in out Writer; Indent : in String) is
		NC_Object : Non_Controlled_Writer
			renames Controlled_Writers.Reference (Object).all;
		Z_Indent : String := Indent & Character'Val (0);
		C_Indent : xmlChar_array (0 .. Indent'Length);
		for C_Indent'Address use Z_Indent'Address;
	begin
		if C.libxml.xmlwriter.xmlTextWriterSetIndentString (
			NC_Object.Raw,
			C_Indent (C_Indent'First)'Access) < 0
		then
			raise Use_Error;
		end if;
	end Set_Indent;
	
	procedure Write (Object : in out Writer; Event : in XML.Event) is
		NC_Object : Non_Controlled_Writer
			renames Controlled_Writers.Reference (Object).all;
	begin
		if NC_Object.Finished then
			raise Status_Error;
		end if;
		case Event.Event_Type is
			when No_Event =>
				raise Data_Error;
			when Element_Start =>
				Check_No_Zero (Event.Name.all);
				declare
					Z_Name : String := Event.Name.all & Character'Val (0);
					C_Name : aliased xmlChar_array (C.size_t);
					for C_Name'Address use Z_Name'Address;
				begin
					Clear_Last_Error;
					if C.libxml.xmlwriter.xmlTextWriterStartElement (
						NC_Object.Raw,
						C_Name (C_Name'First)'Access) < 0
					then
						Raise_Last_Error;
					end if;
				end;
			when Attribute =>
				Check_No_Zero (Event.Name.all);
				Check_No_Zero (Event.Value.all);
				declare
					Z_Name : String := Event.Name.all & Character'Val (0);
					C_Name : aliased xmlChar_array (C.size_t);
					for C_Name'Address use Z_Name'Address;
					Z_Value : String := Event.Value.all & Character'Val (0);
					C_Value : aliased xmlChar_array (C.size_t);
					for C_Value'Address use Z_Value'Address;
				begin
					Clear_Last_Error;
					if C.libxml.xmlwriter.xmlTextWriterWriteAttribute (
						NC_Object.Raw,
						C_Name (C_Name'First)'Access,
						C_Value (C_Value'First)'Access) < 0
					then
						Raise_Last_Error;
					end if;
				end;
			when Text =>
				Check_No_Zero (Event.Content.all);
				declare
					Z_Content : String := Event.Content.all & Character'Val (0);
					C_Content : aliased xmlChar_array (C.size_t);
					for C_Content'Address use Z_Content'Address;
				begin
					Clear_Last_Error;
					if C.libxml.xmlwriter.xmlTextWriterWriteString (
						NC_Object.Raw,
						C_Content (C_Content'First)'Access) < 0
					then
						Raise_Last_Error;
					end if;
				end;
			when CDATA =>
				Check_No_Zero (Event.Content.all);
				declare
					Z_Content : String := Event.Content.all & Character'Val (0);
					C_Content : aliased xmlChar_array (C.size_t);
					for C_Content'Address use Z_Content'Address;
				begin
					Clear_Last_Error;
					if C.libxml.xmlwriter.xmlTextWriterWriteCDATA (
						NC_Object.Raw,
						C_Content (C_Content'First)'Access) < 0
					then
						Raise_Last_Error;
					end if;
				end;
			when Entity_Reference =>
				raise Program_Error; -- unimplemented
			when Entity_Start =>
				raise Program_Error; -- unimplemented
			when Processing_Instruction =>
				raise Program_Error; -- unimplemented
			when Comment =>
				Check_No_Zero (Event.Content.all);
				declare
					Z_Content : String := Event.Content.all & Character'Val (0);
					C_Content : aliased xmlChar_array (C.size_t);
					for C_Content'Address use Z_Content'Address;
				begin
					Clear_Last_Error;
					if C.libxml.xmlwriter.xmlTextWriterWriteComment (
						NC_Object.Raw,
						C_Content (C_Content'First)'Access) < 0
					then
						Raise_Last_Error;
					end if;
				end;
			when Document =>
				raise Program_Error; -- unimplemented
			when Document_Type =>
				declare
					Public_Id_Length : Natural := 0;
					System_Id_Length : Natural := 0;
					Subset_Length : Natural := 0;
				begin
					Check_No_Zero (Event.Name.all);
					if Event.Public_Id /= null then
						Check_No_Zero (Event.Public_Id.all);
						Public_Id_Length := Event.Public_Id'Length + 1;
					end if;
					if Event.System_Id /= null then
						Check_No_Zero (Event.System_Id.all);
						System_Id_Length := Event.System_Id'Length + 1;
					end if;
					if Event.Subset /= null then
						Check_No_Zero (Event.Subset.all);
						Subset_Length := Event.Subset'Length + 1;
					end if;
					declare
						Z_Name : String := Event.Name.all & Character'Val (0);
						C_Name : aliased xmlChar_array (C.size_t);
						for C_Name'Address use Z_Name'Address;
						Z_Public_Id : String (1 .. Public_Id_Length);
						C_Public_Id : aliased xmlChar_array (C.size_t);
						for C_Public_Id'Address use Z_Public_Id'Address;
						P_Public_Id : access constant C.libxml.xmlstring.xmlChar;
						Z_System_Id : String (1 .. System_Id_Length);
						C_System_Id : aliased xmlChar_array (C.size_t);
						for C_System_Id'Address use Z_System_Id'Address;
						P_System_Id : access constant C.libxml.xmlstring.xmlChar;
						Z_Subset : String (1 .. Subset_Length);
						C_Subset : aliased xmlChar_array (C.size_t);
						for C_Subset'Address use Z_Subset'Address;
						P_Subset : access constant C.libxml.xmlstring.xmlChar;
					begin
						if Event.Public_Id /= null then
							Z_Public_Id (1 .. Public_Id_Length) := Event.Public_Id.all;
							Z_Public_Id (Z_Public_Id'Last) := Character'Val (0);
							P_Public_Id := C_Public_Id (C_Public_Id'First)'Access;
						end if;
						if Event.System_Id /= null then
							Z_System_Id (1 .. System_Id_Length) := Event.System_Id.all;
							Z_System_Id (Z_System_Id'Last) := Character'Val (0);
							P_System_Id := C_System_Id (C_System_Id'First)'Access;
						end if;
						if Event.Subset /= null then
							Z_Subset (1 .. Subset_Length) := Event.Subset.all;
							Z_Subset (Z_Subset'Last) := Character'Val (0);
							P_Subset := C_Subset (C_Subset'First)'Access;
						end if;
						Clear_Last_Error;
						if C.libxml.xmlwriter.xmlTextWriterWriteDocType (
							NC_Object.Raw,
							C_Name (C_Name'First)'Access,
							P_Public_Id,
							P_System_Id,
							P_Subset) < 0
						then
							Raise_Last_Error;
						end if;
					end;
				end;
			when Document_Fragment =>
				raise Program_Error; -- unimplemented
			when Notation =>
				raise Program_Error; -- unimplemented
			when Whitespace =>
				raise Program_Error; -- unimplemented
			when Significant_Whitespace =>
				raise Program_Error; -- unimplemented
			when Element_End =>
				Clear_Last_Error;
				if C.libxml.xmlwriter.xmlTextWriterEndElement (
					NC_Object.Raw) < 0
				then
					Raise_Last_Error;
				end if;
			when Entity_End =>
				raise Program_Error; -- unimplemented
			when XML_Declaration =>
				raise Program_Error; -- unimplemented
		end case;
	end Write;
	
	procedure Flush (Object : in out Writer) is
		NC_Object : Non_Controlled_Writer
			renames Controlled_Writers.Reference (Object).all;
	begin
		if C.libxml.xmlwriter.xmlTextWriterFlush (NC_Object.Raw) < 0 then
			raise Use_Error;
		end if;
	end Flush;
	
	procedure Finish (Object : in out Writer) is
		NC_Object : Non_Controlled_Writer
			renames Controlled_Writers.Reference (Object).all;
	begin
		if NC_Object.Finished then
			raise Status_Error;
		else
			NC_Object.Finished := True;
			Write_Document_End (Object);
		end if;
	end Finish;
	
	procedure Write_Document_Start (
		Object : in out Writer;
		Version : access constant String;
		Encoding : Encoding_Type;
		Standalone : Standalone_Type)
	is
		NC_Object : Non_Controlled_Writer
			renames Controlled_Writers.Reference (Object).all;
		Version_Length : Natural := 0;
	begin
		if Version /= null then
			Check_No_Zero (Version.all);
			Version_Length := Version'Length + 1;
		end if;
		declare
			Z_Version : String (1 .. Version_Length);
			C_Version : aliased C.char_array (C.size_t);
			for C_Version'Address use Z_Version'Address;
			P_Version : access constant C.char := null;
			P_Encoding : access constant C.char := null;
		begin
			if Version /= null then
				Z_Version (1 .. Version'Length) := Version.all;
				Z_Version (Z_Version'Last) := Character'Val (0);
				P_Version := C_Version (C_Version'First)'Access;
			end if;
			if Encoding /= null then
				P_Encoding := Encoding.name;
			end if;
			Clear_Last_Error;
			if C.libxml.xmlwriter.xmlTextWriterStartDocument (
				NC_Object.Raw,
				P_Version,
				P_Encoding,
				Standalone_Image (
					C.signed_int'(Standalone_Type'Enum_Rep (Standalone)))) < 0
			then
				Raise_Last_Error;
			end if;
		end;
	end Write_Document_Start;
	
	procedure Write_Document_End (
		Object : in out Writer)
	is
		NC_Object : Non_Controlled_Writer
			renames Controlled_Writers.Reference (Object).all;
	begin
		Clear_Last_Error;
		if C.libxml.xmlwriter.xmlTextWriterEndDocument (NC_Object.Raw) < 0 then
			Raise_Last_Error;
		end if;
	end Write_Document_End;
	
	package body Controlled_Writers is
		
		function Reference (Object : in out XML.Writer)
			return not null access Non_Controlled_Writer is
		begin
			return Writer (Object).Data'Unrestricted_Access;
		end Reference;
		
		overriding procedure Finalize (Object : in out Writer) is
		begin
			if not Object.Data.Finished then
				Write_Document_End (XML.Writer (Object));
			end if;
			C.libxml.xmlwriter.xmlFreeTextWriter (Object.Data.Raw);
		end Finalize;
		
	end Controlled_Writers;
	
	-- exceptions
	
	procedure Install_Error_Handlers is
	begin
		if not Error_Handler_Installed then
			Error_Handler_Installed := True;
			C.libxml.xmlerror.xmlSetStructuredErrorFunc (
				C.void_ptr (System.Null_Address),
				Structured_Error_Handler'Access);
		end if;
	end Install_Error_Handlers;
	
	procedure Clear_Last_Error is
	begin
		C.libxml.xmlerror.xmlResetLastError;
	end Clear_Last_Error;
	
	procedure Raise_Last_Error is
		Error : constant C.libxml.xmlerror.xmlErrorPtr :=
			C.libxml.xmlerror.xmlGetLastError;
		function Location return String is
		begin
			if Error.line = 0 then
				return "";
			else
				return "line" & C.signed_int'Image (Error.line) & ": ";
			end if;
		end Location;
	begin
		if Error = null then
			raise Use_Error; -- ???
		else
			case Error.code is
				when C.libxml.xmlerror.xmlParserErrors'Enum_Rep (
					C.libxml.xmlerror.XML_ERR_OK)
				=>
					raise Use_Error;
				when C.libxml.xmlerror.xmlParserErrors'Enum_Rep (
					C.libxml.xmlerror.XML_ERR_NO_MEMORY)
				=>
					raise Storage_Error;
				when others =>
					raise Data_Error with Location & To_String (Error.message);
			end case;
		end if;
	end Raise_Last_Error;
	
	procedure Check_No_Zero (S : in String) is
	begin
		-- one of the design problems of libxml2,
		-- it uses zero-terminated strings.
		-- user's data may be lost if the data contains '\0', check it here.
		if System.Address (
			C.string.memchr (C.void_const_ptr (S'Address), 0, S'Length))
			/= System.Null_Address
		then
			raise Constraint_Error;
		end if;
	end Check_No_Zero;
	
end XML;
