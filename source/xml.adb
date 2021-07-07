with Ada.Unchecked_Conversion;
with Ada.Unchecked_Deallocation;
with System.Address_To_Access_Conversions;
with C.string;
with C.libxml.globals;
with C.libxml.parser;
with C.libxml.tree;
with C.libxml.xmlIO;
with C.libxml.xmlmemory;
with C.libxml.xmlstring;
with C.libxml.xmlversion;
package body XML is
	use type System.Address;
	use type C.char_array;
	use type C.signed_int;
	use type C.size_t;
	use type C.libxml.encoding.xmlCharEncodingHandlerPtr;
	use type C.libxml.xmlstring.xmlChar_const_ptr;
	use type C.libxml.tree.xmlOutputBufferPtr;
	use type C.libxml.xmlreader.xmlTextReaderPtr;
	use type C.libxml.xmlwriter.xmlTextWriterPtr;
	
	procedure Free is new Ada.Unchecked_Deallocation (String, String_Access);
	
	procedure memcpy (dst, src : System.Address; n : C.size_t)
		with Import, Convention => Intrinsic, External_Name => "__builtin_memcpy";
	procedure memset (
		s : System.Address;
		c : Standard.C.signed_int;
		n : Standard.C.size_t)
		with Import, Convention => Intrinsic, External_Name => "__builtin_memset";
	
	type xmlChar_array is
		array (C.size_t range <>) of aliased C.libxml.xmlstring.xmlChar
		with Convention => C;
	
	function To_char_const_ptr is
		new Ada.Unchecked_Conversion (
			C.libxml.xmlstring.xmlChar_const_ptr,
			C.char_const_ptr);
	
	function To_Address is
		new Ada.Unchecked_Conversion (C.char_const_ptr, System.Address);
	
	function Length (S : access constant C.char) return Natural is
	begin
		if S = null then
			return 0;
		else
			return Natural (C.string.strlen (S));
		end if;
	end Length;
	
	function To_String (S : access constant C.char) return String is
		Result : String (1 .. Length (S));
		for Result'Address use To_Address (C.char_const_ptr (S));
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
		Result : aliased String_Access;
		for Fat'Address use Result'Address;
	begin
		Fat.Data := S.all'Address;
		Fat.Constraints := Constraint.all'Address;
		Constraint.First := S'First;
		Constraint.Last := S'Last;
		return Result;
	end Copy_String_Access;
	
	Version_Checked : Boolean := False;
	
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
		Name_Length : constant C.size_t := Name'Length;
		C_Name : C.char_array (0 .. Name_Length); -- NUL
		Result : C.libxml.encoding.xmlCharEncodingHandlerPtr;
	begin
		memcpy (C_Name'Address, Name'Address, Name_Length);
		C_Name (Name_Length) := C.char'Val (0);
		Result :=
			C.libxml.encoding.xmlFindCharEncodingHandler (C_Name (C_Name'First)'Access);
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
	
	function Read_Handler (
		context : C.void_ptr;
		buffer : access C.char;
		len : C.signed_int)
		return C.signed_int
		with Convention => C;
	
	function Read_Handler (
		context : C.void_ptr;
		buffer : access C.char;
		len : C.signed_int)
		return C.signed_int
	is
		type I is access procedure (Item : out String; Last : out Natural);
		function To_Input is new Ada.Unchecked_Conversion (C.void_ptr, I);
		Item : String (1 .. Natural (len));
		for Item'Address use buffer.all'Address;
		Last : Natural;
	begin
		To_Input (context) (Item, Last);
		return C.signed_int (Last);
	end Read_Handler;
	
	procedure Reader_Error_Handler (
		userData : C.void_ptr;
		error : access C.libxml.xmlerror.xmlError)
		with Convention => C;
	
	procedure Reader_Error_Handler (
		userData : C.void_ptr;
		error : access C.libxml.xmlerror.xmlError)
	is
		package Conv is
			new System.Address_To_Access_Conversions (Non_Controlled_Reader);
		NC_Object : Non_Controlled_Reader
			renames Conv.To_Pointer (System.Address (userData)).all;
	begin
		if NC_Object.Error then
			C.libxml.xmlerror.xmlResetError (NC_Object.U.Last_Error'Access);
		else
			memset (NC_Object.U.Last_Error'Address, 0, C.libxml.xmlerror.xmlError'Size / Standard'Storage_Unit);
		end if;
		NC_Object.Error :=
			not (
				C.libxml.xmlerror.xmlCopyError (
					from => error,
					to => NC_Object.U.Last_Error'Access) < 0);
	end Reader_Error_Handler;
	
	procedure Read_Start (NC_Object : in out Non_Controlled_Reader) is
	begin
		if NC_Object.State = Start then
			Next (NC_Object);
			NC_Object.State := Next;
		end if;
	end Read_Start;
	
	procedure Read (
		NC_Object : in out Non_Controlled_Reader;
		Parsed_Data : out Parsed_Data_Type) is
	begin
		case NC_Object.State is
			when Next | Start | Remaining =>
				if NC_Object.State /= Next then
					Next (NC_Object);
				end if;
				declare
					Node_Type : constant C.signed_int :=
						C.libxml.xmlreader.xmlTextReaderNodeType (NC_Object.Raw);
				begin
					if Node_Type < 0 then
						Raise_Last_Error (NC_Object);
					else
						case Node_Type is
							when C.libxml.xmlreader.xmlReaderTypes'Enum_Rep (
									C.libxml.xmlreader.XML_READER_TYPE_NONE) =>
								Parsed_Data.Event := (Event_Type => No_Event);
								NC_Object.State := Remaining;
							when C.libxml.xmlreader.xmlReaderTypes'Enum_Rep (
									C.libxml.xmlreader.XML_READER_TYPE_ATTRIBUTE) =>
								declare
									C_Name : constant C.libxml.xmlstring.xmlChar_const_ptr :=
										C.libxml.xmlreader.xmlTextReaderConstName (NC_Object.Raw);
									Name : String (1 .. Natural (C.string.strlen (To_char_const_ptr (C_Name))));
									for Name'Address use C_Name.all'Address;
									C_Value : constant C.libxml.xmlstring.xmlChar_const_ptr :=
										C.libxml.xmlreader.xmlTextReaderConstValue (NC_Object.Raw);
									Value : String (1 .. Natural (C.string.strlen (To_char_const_ptr (C_Value))));
									for Value'Address use C_Value.all'Address;
								begin
									Parsed_Data.Event := (
										Event_Type => Attribute,
										Name =>
											Copy_String_Access (
												Name'Unrestricted_Access,
												Parsed_Data.Name_Constraint'Access),
										Value =>
											Copy_String_Access (
												Value'Unrestricted_Access,
												Parsed_Data.Value_Constraint'Access));
								end;
								Reset_Last_Error (NC_Object);
								declare
									Moved : constant C.signed_int :=
										C.libxml.xmlreader.xmlTextReaderMoveToNextAttribute (NC_Object.Raw);
								begin
									if Moved < 0 then
										Raise_Last_Error (NC_Object);
									elsif Moved > 0 then
										NC_Object.State := Next; -- more attributes
									else
										-- end of attributes
										Reset_Last_Error (NC_Object);
										if C.libxml.xmlreader.xmlTextReaderMoveToElement (NC_Object.Raw) < 0 then
											Raise_Last_Error (NC_Object);
										end if;
										if C.libxml.xmlreader.xmlTextReaderIsEmptyElement (NC_Object.Raw) > 0 then
											NC_Object.State := Empty_Element;
										else -- move to children
											NC_Object.State := Remaining;
										end if;
									end if;
								end;
							when C.libxml.xmlreader.xmlReaderTypes'Enum_Rep (
									C.libxml.xmlreader.XML_READER_TYPE_ELEMENT) =>
								declare
									C_Name : constant C.libxml.xmlstring.xmlChar_const_ptr :=
										C.libxml.xmlreader.xmlTextReaderConstName (NC_Object.Raw);
									Name : String (1 .. Natural (C.string.strlen (To_char_const_ptr (C_Name))));
									for Name'Address use C_Name.all'Address;
								begin
									Parsed_Data.Event := (
										Event_Type => Element_Start,
										Name =>
											Copy_String_Access (
												Name'Unrestricted_Access,
												Parsed_Data.Name_Constraint'Access));
								end;
								if C.libxml.xmlreader.xmlTextReaderHasAttributes (NC_Object.Raw) > 0 then
									NC_Object.State := Next;
									Reset_Last_Error (NC_Object);
									if C.libxml.xmlreader.xmlTextReaderMoveToFirstAttribute (
										NC_Object.Raw) < 0
									then
										Raise_Last_Error (NC_Object);
									end if;
								elsif C.libxml.xmlreader.xmlTextReaderIsEmptyElement (NC_Object.Raw) > 0 then
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
									C.libxml.xmlreader.XML_READER_TYPE_SIGNIFICANT_WHITESPACE) =>
								declare
									C_Content : constant C.libxml.xmlstring.xmlChar_const_ptr :=
										C.libxml.xmlreader.xmlTextReaderConstValue (NC_Object.Raw);
									Content :
										String (1 .. Natural (C.string.strlen (To_char_const_ptr (C_Content))));
									for Content'Address use C_Content.all'Address;
									Content_Access : constant String_Access :=
										Copy_String_Access (
											Content'Unrestricted_Access,
											Parsed_Data.Content_Constraint'Access);
								begin
									case Node_Type is
										when C.libxml.xmlreader.xmlReaderTypes'Enum_Rep (
												C.libxml.xmlreader.XML_READER_TYPE_TEXT) =>
											Parsed_Data.Event := (Event_Type => Text, Content => Content_Access);
										when C.libxml.xmlreader.xmlReaderTypes'Enum_Rep (
												C.libxml.xmlreader.XML_READER_TYPE_CDATA) =>
											Parsed_Data.Event := (Event_Type => CDATA, Content => Content_Access);
										when C.libxml.xmlreader.xmlReaderTypes'Enum_Rep (
												C.libxml.xmlreader.XML_READER_TYPE_COMMENT) =>
											Parsed_Data.Event := (Event_Type => Comment, Content => Content_Access);
										when C.libxml.xmlreader.xmlReaderTypes'Enum_Rep (
												C.libxml.xmlreader.XML_READER_TYPE_WHITESPACE) =>
											Parsed_Data.Event := (Event_Type => Whitespace, Content => Content_Access);
										when C.libxml.xmlreader.xmlReaderTypes'Enum_Rep (
												C.libxml.xmlreader.XML_READER_TYPE_SIGNIFICANT_WHITESPACE) =>
											Parsed_Data.Event :=
												(Event_Type => Significant_Whitespace, Content => Content_Access);
										when others =>
											pragma Assert (False);
											null;
									end case;
								end;
								NC_Object.State := Remaining;
							when C.libxml.xmlreader.xmlReaderTypes'Enum_Rep (
									C.libxml.xmlreader.XML_READER_TYPE_PROCESSING_INSTRUCTION) =>
								declare
									C_Name : constant C.libxml.xmlstring.xmlChar_const_ptr :=
										C.libxml.xmlreader.xmlTextReaderConstName (NC_Object.Raw);
									Name : String (1 .. Natural (C.string.strlen (To_char_const_ptr (C_Name))));
									for Name'Address use C_Name.all'Address;
								begin
									Parsed_Data.Event := (
										Event_Type => Processing_Instruction,
										Name =>
											Copy_String_Access (
												Name'Unrestricted_Access,
												Parsed_Data.Name_Constraint'Access));
									-- it's not able to get attributes info with libxml2 (?)
									pragma Assert (
										C.libxml.xmlreader.xmlTextReaderHasAttributes (NC_Object.Raw) = 0);
								end;
								NC_Object.State := Remaining;
							when C.libxml.xmlreader.xmlReaderTypes'Enum_Rep (
									C.libxml.xmlreader.XML_READER_TYPE_DOCUMENT_TYPE) =>
								declare
									C_Name : constant C.libxml.xmlstring.xmlChar_const_ptr :=
										C.libxml.xmlreader.xmlTextReaderConstName (NC_Object.Raw);
									Name : String (1 .. Natural (C.string.strlen (To_char_const_ptr (C_Name))));
									for Name'Address use C_Name.all'Address;
								begin
									Parsed_Data.Event := (
										Event_Type => Document_Type,
										Name =>
											Copy_String_Access (
												Name'Unrestricted_Access,
												Parsed_Data.Name_Constraint'Access),
										Public_Id => null,
										System_Id => null,
										Subset => null);
									-- it's not able to get extra DTD info with libxml2 (?)
									pragma Assert (
										C.libxml.xmlreader.xmlTextReaderHasAttributes (NC_Object.Raw) = 0);
								end;
								NC_Object.State := Remaining;
							when C.libxml.xmlreader.xmlReaderTypes'Enum_Rep (
									C.libxml.xmlreader.XML_READER_TYPE_END_ELEMENT) =>
								Parsed_Data.Event := (Event_Type => Element_End);
								NC_Object.State := Remaining;
							when others =>
								raise Program_Error with "in XML.Read, unimplemented" & Node_Type'Img;
						end case;
					end if;
				end;
			when Empty_Element =>
				Parsed_Data.Event := (Event_Type => Element_End);
				NC_Object.State := Remaining;
		end case;
	end Read;
	
	procedure Read (Object : in out Reader; Parsed_Data : out Parsed_Data_Type) is
		procedure Process (NC_Object : in out Non_Controlled_Reader) is
		begin
			Read (NC_Object, Parsed_Data);
		end Process;
		procedure Do_Read is new Controlled_Readers.Update (Process);
	begin
		Do_Read (Object);
	end Read;
	
	-- implementation of reader
	
	function Is_Assigned (Parsing_Entry : Parsing_Entry_Type) return Boolean is
	begin
		return Parsing_Entry.Assigned;
	end Is_Assigned;
	
	function Value (Parsing_Entry : aliased Parsing_Entry_Type)
		return Event_Reference_Type
	is
		pragma Check (Pre,
			Check => Is_Assigned (Parsing_Entry) or else raise Status_Error);
	begin
		return (Element => Parsing_Entry.Data.Event'Access);
	end Value;
	
	function Create (
		Input : not null access procedure (Item : out String; Last : out Natural);
		Encoding : Encoding_Type := No_Encoding;
		URI : String := "")
		return Reader
	is
		type I is access procedure (Item : out String; Last : out Natural);
		function To_void_ptr is new Ada.Unchecked_Conversion (I, C.void_ptr);
	begin
		Check_Version;
		declare
			P_Encoding : C.char_const_ptr := null;
			P_URI : access constant C.char := null;
			URI_Length : constant C.size_t := URI'Length;
			C_URI : aliased C.char_array (0 .. URI_Length); -- NUL
		begin
			if Encoding /= null then
				P_Encoding := C.char_const_ptr (Encoding.name);
			end if;
			if URI'Length > 0 then
				memcpy (C_URI'Address, URI'Address, URI_Length);
				C_URI (URI_Length) := C.char'Val (0);
				P_URI := C_URI (C_URI'First)'Access;
			end if;
			return Result : aliased Reader do
				declare
					NC_Result : Non_Controlled_Reader
						renames Controlled_Readers.Reference (Result).all;
				begin
					NC_Result.Raw :=
						C.libxml.xmlreader.xmlReaderForIO (
							Read_Handler'Access,
							null,
							To_void_ptr (Input),
							P_URI,
							P_Encoding,
							0);
					if NC_Result.Raw = null then
						raise Use_Error;
					end if;
					Install_Error_Handler (NC_Result);
				end;
			end return;
		end;
	end Create;
	
	procedure Set_DTD_Loading (
		Object : in out Reader;
		Value : in Boolean)
	is
		procedure Process (NC_Object : in out Non_Controlled_Reader) is
		begin
			if C.libxml.xmlreader.xmlTextReaderSetParserProp (
				NC_Object.Raw,
				C.libxml.xmlreader.xmlParserProperties'Enum_Rep (
					C.libxml.xmlreader.XML_PARSER_LOADDTD),
				Boolean'Pos (Value)) < 0
			then
				raise Use_Error;
			end if;
		end Process;
		procedure Do_Set_DTD_Loading is new Controlled_Readers.Update (Process);
	begin
		Do_Set_DTD_Loading (Object);
	end Set_DTD_Loading;
	
	procedure Set_Default_Attributes (
		Object : in out Reader;
		Value : in Boolean)
	is
		procedure Process (NC_Object : in out Non_Controlled_Reader) is
		begin
			if C.libxml.xmlreader.xmlTextReaderSetParserProp (
				NC_Object.Raw,
				C.libxml.xmlreader.xmlParserProperties'Enum_Rep (
					C.libxml.xmlreader.XML_PARSER_DEFAULTATTRS),
				Boolean'Pos (Value)) < 0
			then
				raise Use_Error;
			end if;
		end Process;
		procedure Do_Set_Default_Attributes is new Controlled_Readers.Update (Process);
	begin
		Do_Set_Default_Attributes (Object);
	end Set_Default_Attributes;
	
	procedure Set_Validation (
		Object : in out Reader;
		Value : in Boolean)
	is
		procedure Process (NC_Object : in out Non_Controlled_Reader) is
		begin
			if C.libxml.xmlreader.xmlTextReaderSetParserProp (
				NC_Object.Raw,
				C.libxml.xmlreader.xmlParserProperties'Enum_Rep (
					C.libxml.xmlreader.XML_PARSER_VALIDATE),
				Boolean'Pos (Value)) < 0
			then
				raise Use_Error;
			end if;
		end Process;
		procedure Do_Set_Validation is new Controlled_Readers.Update (Process);
	begin
		Do_Set_Validation (Object);
	end Set_Validation;
	
	procedure Set_Substitute_Entities (
		Object : in out Reader;
		Value : in Boolean)
	is
		procedure Process (NC_Object : in out Non_Controlled_Reader) is
		begin
			if C.libxml.xmlreader.xmlTextReaderSetParserProp (
				NC_Object.Raw,
				C.libxml.xmlreader.xmlParserProperties'Enum_Rep (
					C.libxml.xmlreader.XML_PARSER_SUBST_ENTITIES),
				Boolean'Pos (Value)) < 0
			then
				raise Use_Error;
			end if;
		end Process;
		procedure Do_Set_Substitute_Entities is
			new Controlled_Readers.Update (Process);
	begin
		Do_Set_Substitute_Entities (Object);
	end Set_Substitute_Entities;
	
	function Version (Object : in out Reader) return access constant String is
		Result : access constant String;
		procedure Process (NC_Object : in out Non_Controlled_Reader) is
		begin
			if NC_Object.Version = null then
				Read_Start (NC_Object);
				declare
					C_Version : constant C.libxml.xmlstring.xmlChar_const_ptr :=
						C.libxml.xmlreader.xmlTextReaderConstXmlVersion (NC_Object.Raw);
				begin
					if C_Version /= null then
						declare
							A_Version :
								String (1 .. Natural (C.string.strlen (To_char_const_ptr (C_Version))));
							for A_Version'Address use C_Version.all'Address;
						begin
							NC_Object.Version := new String'(A_Version);
						end;
					end if;
				end;
			end if;
			Result := NC_Object.Version;
		end Process;
		procedure Do_Version is new Controlled_Readers.Update (Process);
	begin
		Do_Version (Object);
		return Result;
	end Version;
	
	function Encoding (Object : in out Reader) return Encoding_Type is
		Result : Encoding_Type;
		procedure Process (NC_Object : in out Non_Controlled_Reader) is
		begin
			Read_Start (NC_Object);
			Result := Encoding_Type (
				C.libxml.encoding.xmlFindCharEncodingHandler (
					To_char_const_ptr (
						C.libxml.xmlreader.xmlTextReaderConstEncoding (NC_Object.Raw))));
		end Process;
		procedure Do_Encoding is new Controlled_Readers.Update (Process);
	begin
		Do_Encoding (Object);
		return Result;
	end Encoding;
	
	function Standalone (Object : in out Reader) return Standalone_Type is
		Result : Standalone_Type;
		procedure Process (NC_Object : in out Non_Controlled_Reader) is
		begin
			Read_Start (NC_Object);
			declare
				Standalone_Value : constant C.signed_int :=
					C.libxml.xmlreader.xmlTextReaderStandalone (NC_Object.Raw);
			begin
				if Standalone_Value < -1 then
					Result := No_Specific; -- undocumented error
				else
					Result := Standalone_Type'Enum_Val (Standalone_Value);
				end if;
			end;
		end Process;
		procedure Do_Standalone is new Controlled_Readers.Update (Process);
	begin
		Do_Standalone (Object);
		return Result;
	end Standalone;
	
	function Base_URI (Object : in out Reader) return String is
		Result : C.libxml.xmlstring.xmlChar_const_ptr;
		procedure Process (NC_Object : in out Non_Controlled_Reader) is
		begin
			Read_Start (NC_Object);
			Result := C.libxml.xmlreader.xmlTextReaderConstBaseUri (NC_Object.Raw);
		end Process;
		procedure Do_Base_URI is new Controlled_Readers.Update (Process);
	begin
		Do_Base_URI (Object);
		return To_String (To_char_const_ptr (Result));
	end Base_URI;
	
	procedure Get (
		Object : in out Reader;
		Process : not null access procedure (Event : in XML.Event))
	is
		Parsed_Data : Parsed_Data_Type;
	begin
		Read (Object, Parsed_Data);
		Process (Parsed_Data.Event);
	end Get;
	
	procedure Get (
		Object : in out Reader;
		Parsing_Entry : out Parsing_Entry_Type)
	is
		pragma Check (Pre,
			Check => not Is_Assigned (Parsing_Entry) or else raise Status_Error);
	begin
		Read (Object, Parsing_Entry.Data);
		Parsing_Entry.Assigned := True;
	end Get;
	
	procedure Get_Until_Element_End (Object : in out Reader) is
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
						Get_Until_Element_End (Object);
					when Element_End =>
						exit;
					when others =>
						null;
				end case;
			end;
		end loop;
	end Get_Until_Element_End;
	
	procedure Finish (Object : in out Reader) is
		procedure Process (NC_Object : in out Non_Controlled_Reader) is
		begin
			case NC_Object.State is
				when Next =>
					null;
				when Start | Remaining =>
					Next (NC_Object);
				when Empty_Element =>
					raise Data_Error; -- Element_End
			end case;
			declare
				Node_Type : constant C.signed_int :=
					C.libxml.xmlreader.xmlTextReaderNodeType (NC_Object.Raw);
			begin
				if Node_Type /=
					C.libxml.xmlreader.xmlReaderTypes'Enum_Rep (
						C.libxml.xmlreader.XML_READER_TYPE_NONE)
				then
					raise Data_Error;
				end if;
			end;
		end Process;
		procedure Do_Finish is new Controlled_Readers.Update (Process);
	begin
		Do_Finish (Object);
	end Finish;
	
	procedure Next (NC_Object : in out Non_Controlled_Reader) is
	begin
		Reset_Last_Error (NC_Object);
		if C.libxml.xmlreader.xmlTextReaderRead (NC_Object.Raw) < 0 then
			Raise_Last_Error (NC_Object);
		end if;
	end Next;
	
	procedure Install_Error_Handler (
		NC_Object : aliased in out Non_Controlled_Reader)
	is
		package Conv is
			new System.Address_To_Access_Conversions (Non_Controlled_Reader);
	begin
		C.libxml.xmlreader.xmlTextReaderSetStructuredErrorHandler (
			NC_Object.Raw,
			Reader_Error_Handler'Access,
			C.void_ptr (Conv.To_Address (NC_Object'Access)));
	end Install_Error_Handler;
	
	procedure Reset_Last_Error (NC_Object : in out Non_Controlled_Reader) is
	begin
		if NC_Object.Error then
			NC_Object.Error := False;
			C.libxml.xmlerror.xmlResetError (NC_Object.U.Last_Error'Access);
		end if;
	end Reset_Last_Error;
	
	procedure Raise_Last_Error (NC_Object : in Non_Controlled_Reader) is
	begin
		if not NC_Object.Error then
			raise Use_Error; -- API reported a failure, but did not callbacked
		else
			Raise_Error (NC_Object.U.Last_Error'Access);
		end if;
	end Raise_Last_Error;
	
	package body Controlled_Readers is
		
		function Reference (Object : aliased in out Reader)
			return not null access Non_Controlled_Reader;
		pragma Inline (Reference);
		
		function Reference (Object : aliased in out Reader)
			return not null access Non_Controlled_Reader is
		begin
			return Object.Data'Access;
		end Reference;
		
		-- implementation
		
		function Reference (Object : aliased in out XML.Reader)
			return not null access Non_Controlled_Reader is
		begin
			return Reference (Reader (Object));
		end Reference;
		
		function Query (Object : XML.Reader) return Result_Type is
			function Query (Object : Reader) return Result_Type;
			pragma Inline (Query);
			
			function Query (Object : Reader) return Result_Type is
			begin
				return Process (Object.Data);
			end Query;
		begin
			return Query (Reader (Object));
		end Query;
		
		procedure Update (Object : in out XML.Reader) is
			procedure Update (Object : in out Reader);
			pragma Inline (Update);
			
			procedure Update (Object : in out Reader) is
			begin
				Process (Object.Data);
			end Update;
		begin
			Update (Reader (Object));
		end Update;
		
		overriding procedure Finalize (Object : in out Reader) is
		begin
			C.libxml.xmlreader.xmlFreeTextReader (Object.Data.Raw);
			Free (Object.Data.Version);
			if Object.Data.Error then
				C.libxml.xmlerror.xmlResetError (Object.Data.U.Last_Error'Access);
			end if;
		end Finalize;
		
	end Controlled_Readers;
	
	-- writer
	
	function Write_Handler (
		context : C.void_ptr;
		buffer : access constant C.char;
		len : C.signed_int)
		return C.signed_int
		with Convention => C;
	
	function Write_Handler (
		context : C.void_ptr;
		buffer : access constant C.char;
		len : C.signed_int)
		return C.signed_int
	is
		type O is access procedure (Item : in String);
		function To_Output is new Ada.Unchecked_Conversion (C.void_ptr, O);
		Item : String (1 .. Natural (len));
		for Item'Address use buffer.all'Address;
	begin
		To_Output (context) (Item);
		return len;
	end Write_Handler;
	
	procedure Flush (NC_Object : in Non_Controlled_Writer) is
	begin
		if C.libxml.xmlwriter.xmlTextWriterFlush (NC_Object.Raw) < 0 then
			raise Use_Error;
		end if;
	end Flush;
	
	procedure Check_No_Zero (S : in String) is
	begin
		-- one of the design problems of libxml2,
		-- it uses zero-terminated strings.
		-- user's data may be lost if the data contains '\0', check it here.
		if System.Address (
				C.string.memchr (C.void_const_ptr (S'Address), 0, S'Length)) /=
			System.Null_Address
		then
			raise Constraint_Error;
		end if;
	end Check_No_Zero;
	
	-- for standalone
	No_Image : constant C.char_array := "no" & C.char'Val (0);
	Yes_Image : constant C.char_array := "yes" & C.char'Val (0);
	Standalone_Image : constant array (Standalone_Type) of C.char_const_ptr := (
		No_Specific => null,
		No => No_Image (No_Image'First)'Access,
		Yes => Yes_Image (Yes_Image'First)'Access);
	
	-- implementation of writer
	
	function Create (
		Output : not null access procedure (Item : in String);
		Encoding : Encoding_Type := No_Encoding)
		return Writer
	is
		type O is access procedure (Item : in String);
		function To_void_ptr is new Ada.Unchecked_Conversion (O, C.void_ptr);
	begin
		Check_Version;
		declare
			Buffer : constant C.libxml.tree.xmlOutputBufferPtr :=
				C.libxml.xmlIO.xmlOutputBufferCreateIO (
					Write_Handler'Access,
					null,
					To_void_ptr (Output),
					Encoding);
		begin
			if Buffer = null then
				raise Use_Error;
			end if;
			return Result : Writer do
				declare
					procedure Process (NC_Result : in out Non_Controlled_Writer) is
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
					end Process;
					procedure Do_Create is new Controlled_Writers.Update (Process);
				begin
					Do_Create (Result);
				end;
			end return;
		end;
	end Create;
	
	function Finished (Object : Writer) return Boolean is
		function Process (NC_Object : Non_Controlled_Writer) return Boolean is
		begin
			return NC_Object.Finished;
		end Process;
		function Do_Finished is new Controlled_Writers.Query (Boolean, Process);
	begin
		return Do_Finished (Object);
	end Finished;
	
	procedure Flush (Object : in out Writer) is
		procedure Process (NC_Object : in out Non_Controlled_Writer) is
		begin
			Flush (NC_Object);
		end Process;
		procedure Do_Flush is new Controlled_Writers.Update (Process);
	begin
		Do_Flush (Object);
	end Flush;
	
	procedure Set_Indent (Object : in out Writer; Indent : in Natural) is
		procedure Process (NC_Object : in out Non_Controlled_Writer) is
		begin
			if C.libxml.xmlwriter.xmlTextWriterSetIndent (
				NC_Object.Raw,
				C.signed_int (Indent)) < 0
			then
				raise Use_Error;
			end if;
		end Process;
		procedure Do_Set_Indent is new Controlled_Writers.Update (Process);
	begin
		Do_Set_Indent (Object);
	end Set_Indent;
	
	procedure Set_Indent (Object : in out Writer; Indent : in String) is
		procedure Process (NC_Object : in out Non_Controlled_Writer) is
			Indent_Length : constant C.size_t := Indent'Length;
			C_Indent : xmlChar_array (0 .. Indent_Length); -- NUL
		begin
			memcpy (C_Indent'Address, Indent'Address, Indent_Length);
			C_Indent (Indent_Length) := C.libxml.xmlstring.xmlChar'Val (0);
			if C.libxml.xmlwriter.xmlTextWriterSetIndentString (
				NC_Object.Raw,
				C_Indent (C_Indent'First)'Access) < 0
			then
				raise Use_Error;
			end if;
		end Process;
		procedure Do_Set_Indent is new Controlled_Writers.Update (Process);
	begin
		Do_Set_Indent (Object);
	end Set_Indent;
	
	procedure Put (Object : in out Writer; Event : in XML.Event) is
		pragma Check (Pre,
			Check => not Finished (Object) or else raise Status_Error);
		procedure Process (NC_Object : in out Non_Controlled_Writer) is
		begin
			case Event.Event_Type is
				when No_Event =>
					raise Data_Error;
				when Element_Start =>
					Check_No_Zero (Event.Name.all);
					declare
						Name : String renames Event.Name.all;
						Name_Length : constant C.size_t := Name'Length;
						C_Name : xmlChar_array (0 .. Name_Length); -- NUL
					begin
						memcpy (C_Name'Address, Name'Address, Name_Length);
						C_Name (Name_Length) := C.libxml.xmlstring.xmlChar'Val (0);
						C.libxml.xmlerror.xmlResetLastError;
						if C.libxml.xmlwriter.xmlTextWriterStartElement (
							NC_Object.Raw,
							C_Name (C_Name'First)'Access) < 0
						then
							Raise_Error (C.libxml.xmlerror.xmlGetLastError);
						end if;
					end;
				when Attribute =>
					Check_No_Zero (Event.Name.all);
					Check_No_Zero (Event.Value.all);
					declare
						Name : String renames Event.Name.all;
						Name_Length : constant C.size_t := Name'Length;
						C_Name : xmlChar_array (0 .. Name_Length); -- NUL
						Value : String renames Event.Value.all;
						Value_Length : constant C.size_t := Value'Length;
						C_Value : xmlChar_array (0 .. Value_Length); -- NUL
					begin
						memcpy (C_Name'Address, Name'Address, Name_Length);
						C_Name (Name_Length) := C.libxml.xmlstring.xmlChar'Val (0);
						memcpy (C_Value'Address, Value'Address, Value_Length);
						C_Value (Value_Length) := C.libxml.xmlstring.xmlChar'Val (0);
						C.libxml.xmlerror.xmlResetLastError;
						if C.libxml.xmlwriter.xmlTextWriterWriteAttribute (
							NC_Object.Raw,
							C_Name (C_Name'First)'Access,
							C_Value (C_Value'First)'Access) < 0
						then
							Raise_Error (C.libxml.xmlerror.xmlGetLastError);
						end if;
					end;
				when Text =>
					Check_No_Zero (Event.Content.all);
					declare
						Content : String renames Event.Content.all;
						Content_Length : constant C.size_t := Content'Length;
						C_Content : xmlChar_array (0 .. Content_Length); -- NUL
					begin
						memcpy (C_Content'Address, Content'Address, Content_Length);
						C_Content (Content_Length) := C.libxml.xmlstring.xmlChar'Val (0);
						C.libxml.xmlerror.xmlResetLastError;
						if C.libxml.xmlwriter.xmlTextWriterWriteString (
							NC_Object.Raw,
							C_Content (C_Content'First)'Access) < 0
						then
							Raise_Error (C.libxml.xmlerror.xmlGetLastError);
						end if;
					end;
				when CDATA =>
					Check_No_Zero (Event.Content.all);
					declare
						Content : String renames Event.Content.all;
						Content_Length : constant C.size_t := Content'Length;
						C_Content : xmlChar_array (0 .. Content_Length); -- NUL
					begin
						memcpy (C_Content'Address, Content'Address, Content_Length);
						C_Content (Content_Length) := C.libxml.xmlstring.xmlChar'Val (0);
						C.libxml.xmlerror.xmlResetLastError;
						if C.libxml.xmlwriter.xmlTextWriterWriteCDATA (
							NC_Object.Raw,
							C_Content (C_Content'First)'Access) < 0
						then
							Raise_Error (C.libxml.xmlerror.xmlGetLastError);
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
						Content : String renames Event.Content.all;
						Content_Length : constant C.size_t := Content'Length;
						C_Content : xmlChar_array (0 .. Content_Length); -- NUL
					begin
						memcpy (C_Content'Address, Content'Address, Content_Length);
						C_Content (Content_Length) := C.libxml.xmlstring.xmlChar'Val (0);
						C.libxml.xmlerror.xmlResetLastError;
						if C.libxml.xmlwriter.xmlTextWriterWriteComment (
							NC_Object.Raw,
							C_Content (C_Content'First)'Access) < 0
						then
							Raise_Error (C.libxml.xmlerror.xmlGetLastError);
						end if;
					end;
				when Document =>
					raise Program_Error; -- unimplemented
				when Document_Type =>
					declare
						Public_Id_Length : C.size_t := 0;
						System_Id_Length : C.size_t := 0;
						Subset_Length : C.size_t := 0;
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
							Name : String renames Event.Name.all;
							Name_Length : constant C.size_t := Name'Length;
							C_Name : xmlChar_array (0 .. Name_Length); -- NULL
							C_Public_Id : xmlChar_array (0 .. Public_Id_Length); -- NUL
							P_Public_Id : access constant C.libxml.xmlstring.xmlChar;
							C_System_Id : xmlChar_array (0 .. System_Id_Length); -- NUL
							P_System_Id : access constant C.libxml.xmlstring.xmlChar;
							C_Subset : xmlChar_array (0 .. Subset_Length); -- NUL
							P_Subset : access constant C.libxml.xmlstring.xmlChar;
						begin
							memcpy (C_Name'Address, Name'Address, Name_Length);
							C_Name (Name_Length) := C.libxml.xmlstring.xmlChar'Val (0);
							if Event.Public_Id /= null then
								memcpy (C_Public_Id'Address, Event.Public_Id.all'Address, Public_Id_Length);
								C_Public_Id (Public_Id_Length) := C.libxml.xmlstring.xmlChar'Val (0);
								P_Public_Id := C_Public_Id (C_Public_Id'First)'Access;
							end if;
							if Event.System_Id /= null then
								memcpy (C_System_Id'Address, Event.System_Id.all'Address, System_Id_Length);
								C_System_Id (System_Id_Length) := C.libxml.xmlstring.xmlChar'Val (0);
								P_System_Id := C_System_Id (C_System_Id'First)'Access;
							end if;
							if Event.Subset /= null then
								memcpy (C_Subset'Address, Event.Subset.all'Address, Subset_Length);
								C_Subset (Subset_Length) := C.libxml.xmlstring.xmlChar'Val (0);
								P_Subset := C_Subset (C_Subset'First)'Access;
							end if;
							C.libxml.xmlerror.xmlResetLastError;
							if C.libxml.xmlwriter.xmlTextWriterWriteDocType (
								NC_Object.Raw,
								C_Name (C_Name'First)'Access,
								P_Public_Id,
								P_System_Id,
								P_Subset) < 0
							then
								Raise_Error (C.libxml.xmlerror.xmlGetLastError);
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
					C.libxml.xmlerror.xmlResetLastError;
					if C.libxml.xmlwriter.xmlTextWriterEndElement (NC_Object.Raw) < 0 then
						Raise_Error (C.libxml.xmlerror.xmlGetLastError);
					end if;
				when Entity_End =>
					raise Program_Error; -- unimplemented
				when XML_Declaration =>
					raise Program_Error; -- unimplemented
			end case;
		end Process;
		procedure Do_Put is new Controlled_Writers.Update (Process);
	begin
		Do_Put (Object);
	end Put;
	
	procedure Put_Document_Start (
		Object : in out Writer;
		Version : access constant String := null;
		Encoding : Encoding_Type := No_Encoding;
		Standalone : Standalone_Type := No_Specific)
	is
		pragma Check (Pre,
			Check => not Finished (Object) or else raise Status_Error);
		procedure Process (NC_Object : in out Non_Controlled_Writer) is
			Version_Length : C.size_t := 0;
		begin
			if Version /= null then
				Check_No_Zero (Version.all);
				Version_Length := Version'Length + 1;
			end if;
			declare
				C_Version : C.char_array (0 .. Version_Length); -- NUL
				P_Version : access constant C.char := null;
				P_Encoding : access constant C.char := null;
			begin
				if Version /= null then
					memcpy (C_Version'Address, Version.all'Address, Version_Length);
					C_Version (Version_Length) := C.char'Val (0);
					P_Version := C_Version (C_Version'First)'Access;
				end if;
				if Encoding /= null then
					P_Encoding := Encoding.name;
				end if;
				C.libxml.xmlerror.xmlResetLastError;
				if C.libxml.xmlwriter.xmlTextWriterStartDocument (
					NC_Object.Raw,
					P_Version,
					P_Encoding,
					Standalone_Image (Standalone)) < 0
				then
					Raise_Error (C.libxml.xmlerror.xmlGetLastError);
				end if;
			end;
		end Process;
		procedure Do_Put_Document_Start is new Controlled_Writers.Update (Process);
	begin
		Do_Put_Document_Start (Object);
	end Put_Document_Start;
	
	procedure Put_Document_End (Object : in out Writer) is
		pragma Check (Pre,
			Check => not Finished (Object) or else raise Status_Error);
		procedure Process (NC_Object : in out Non_Controlled_Writer) is
		begin
			C.libxml.xmlerror.xmlResetLastError;
			if C.libxml.xmlwriter.xmlTextWriterEndDocument (NC_Object.Raw) < 0 then
				Raise_Error (C.libxml.xmlerror.xmlGetLastError);
			end if;
		end Process;
		procedure Do_Put_Document_End is new Controlled_Writers.Update (Process);
	begin
		Do_Put_Document_End (Object);
	end Put_Document_End;
	
	procedure Finish (Object : in out Writer) is
		pragma Check (Pre,
			Check => not Finished (Object) or else raise Status_Error);
		procedure Process (NC_Object : in out Non_Controlled_Writer) is
		begin
			NC_Object.Finished := True;
			Flush (NC_Object);
		end Process;
		procedure Do_Finish is new Controlled_Writers.Update (Process);
	begin
		Do_Finish (Object);
	end Finish;
	
	package body Controlled_Writers is
		
		function Query (Object : XML.Writer) return Result_Type is
			function Query (Object : Writer) return Result_Type;
			pragma Inline (Query);
			
			function Query (Object : Writer) return Result_Type is
			begin
				return Process (Object.Data);
			end Query;
		begin
			return Query (Writer (Object));
		end Query;
		
		procedure Update (Object : in out XML.Writer) is
			procedure Update (Object : in out Writer);
			pragma Inline (Update);
			
			procedure Update (Object : in out Writer) is
			begin
				Process (Object.Data);
			end Update;
		begin
			Update (Writer (Object));
		end Update;
		
		overriding procedure Finalize (Object : in out Writer) is
		begin
			C.libxml.xmlwriter.xmlFreeTextWriter (Object.Data.Raw);
		end Finalize;
		
	end Controlled_Writers;
	
	-- implementation of exceptions
	
	procedure Raise_Error (Error : access constant C.libxml.xmlerror.xmlError) is
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
						C.libxml.xmlerror.XML_ERR_OK) =>
					raise Use_Error;
				when C.libxml.xmlerror.xmlParserErrors'Enum_Rep (
						C.libxml.xmlerror.XML_ERR_NO_MEMORY) =>
					raise Storage_Error;
				when others =>
					raise Data_Error with Location & To_String (Error.message);
			end case;
		end if;
	end Raise_Error;
	
end XML;
