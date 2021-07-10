with Ada.Unchecked_Deallocation;
package body Serialization.XML is
	use type Ada.Strings.Unbounded.String_Access;
	use type Standard.XML.Event_Type;
	
	Null_String : aliased String := "";
	
	procedure Free_And_Null (X : in out Ada.Strings.Unbounded.String_Access) is
	begin
		if X /= Null_String'Access then
			Ada.Strings.Unbounded.Free (X);
			X := Null_String'Access;
		end if;
	end Free_And_Null;
	
	Sequence_Item_Name : aliased String := "item";
	
	procedure Free is
		new Ada.Unchecked_Deallocation (Serializer, Serializer_Access);
	
	procedure Free is
		new Ada.Unchecked_Deallocation (XML_Reader, XML_Reader_Access);
	procedure Free is
		new Ada.Unchecked_Deallocation (XML_Writer, XML_Writer_Access);
	
	-- private implementation
	
	overriding procedure Finalize (Object : in out Reference_Type) is
	begin
		Free (Object.Serializer_Body);
		if Object.Reader_Body /= null then
			if Object.Reader_Body.Next_Name /= Null_String'Access then
				Ada.Strings.Unbounded.Free (Object.Reader_Body.Next_Name);
			end if;
			if Object.Reader_Body.Next_Value /= Null_String'Access then
				Ada.Strings.Unbounded.Free (Object.Reader_Body.Next_Value);
			end if;
			if Object.Reader_Body.Next_Next_Name /= Sequence_Item_Name'Access then
				Ada.Strings.Unbounded.Free (Object.Reader_Body.Next_Next_Name);
			end if;
			Free (Object.Reader_Body);
		end if;
		if Object.Writer_Body /= null then
			Free (Object.Writer_Body);
		end if;
	end Finalize;
	
	-- reading
	
	procedure Handle_Name (
		Object : not null access XML_Reader;
		Position : in State;
		Event : in Standard.XML.Event;
		Root_Tag : in String) is
	begin
		case Event.Event_Type is
			when Standard.XML.Element_Start =>
				Object.Next_Kind := Value; -- flag to Read_Value
				if Object.Level = 0 then
					if Event.Name.all /= Root_Tag then
						raise Standard.XML.Data_Error;
					end if;
				else
					case Position is
						when In_Mapping =>
							Object.Next_Name := new String'(Event.Name.all);
						when In_Sequence =>
							if Event.Name.all /= Sequence_Item_Name then
								raise Standard.XML.Data_Error;
							end if;
					end case;
				end if;
				Object.Level := Object.Level + 1;
			when Standard.XML.Element_End =>
				Object.Level := Object.Level - 1;
				case Position is
					when In_Mapping =>
						Object.Next_Kind := Leave_Mapping;
					when In_Sequence =>
						Object.Next_Kind := Leave_Sequence;
				end case;
			when Standard.XML.No_Event =>
				Object.Next_Kind := End_Of_Stream;
			when others =>
				raise Standard.XML.Data_Error;
		end case;
	end Handle_Name;
	
	procedure Read_Name (
		Object : not null access XML_Reader;
		Position : in State) is
	begin
		if Object.Next_Next_Name /= null then
			Object.Next_Kind := Value;
			if Object.Next_Next_Name /= Sequence_Item_Name'Access then
				Object.Next_Name := Object.Next_Next_Name;
			end if;
			Object.Next_Next_Name := null;
		else
			declare
				Parsing_Entry : aliased Standard.XML.Parsing_Entry_Type;
			begin
				Standard.XML.Get (Object.Reader.all, Parsing_Entry);
				Handle_Name (
					Object,
					Position,
					Standard.XML.Value (Parsing_Entry).Element.all,
					"");
			end;
		end if;
	end Read_Name;
	
	procedure Read_Name_On_Start (
		Object : not null access XML_Reader;
		Tag : in String)
	is
		procedure Process (Event : in Standard.XML.Event) is
		begin
			case Event.Event_Type is
				when Standard.XML.Document_Type =>
					if Event.Name.all /= Tag then
						raise Standard.XML.Data_Error
							with """" & Event.Name.all & """ is not expected tag (""" & Tag & """) .";
					end if;
				when others =>
					Handle_Name (Object, In_Mapping, Event, Tag);
			end case;
		end Process;
	begin
		declare
			Parsing_Entry : aliased Standard.XML.Parsing_Entry_Type;
		begin
			Standard.XML.Get (Object.Reader.all, Parsing_Entry);
			Process (Standard.XML.Value (Parsing_Entry).Element.all);
		end;
		if Object.Level = 0 then
			declare
				Parsing_Entry : aliased Standard.XML.Parsing_Entry_Type;
			begin
				Standard.XML.Get (Object.Reader.all, Parsing_Entry);
				Process (Standard.XML.Value (Parsing_Entry).Element.all);
			end;
			if Object.Level = 0 then
				raise Standard.XML.Data_Error
					with "expected tag (""" & Tag & """) was not found.";
			end if;
		end if;
	end Read_Name_On_Start;
	
	procedure Read_Value (Object : not null access XML_Reader) is
	begin
		declare
			Parsing_Entry : aliased Standard.XML.Parsing_Entry_Type;
		begin
			Standard.XML.Get (Object.Reader.all, Parsing_Entry);
			case Standard.XML.Value (Parsing_Entry).Element.Event_Type is
				when Standard.XML.Text | Standard.XML.CDATA =>
					Object.Next_Kind := Value;
					Object.Next_Value :=
						new String'(Standard.XML.Value (Parsing_Entry).Element.Content.all);
				when Standard.XML.Element_Start =>
					if Standard.XML.Value (Parsing_Entry).Element.Name.all =
						Sequence_Item_Name
					then
						Object.Next_Kind := Enter_Sequence;
						Object.Next_Next_Name := Sequence_Item_Name'Access;
					else
						Object.Next_Kind := Enter_Mapping;
						Object.Next_Next_Name :=
							new String'(Standard.XML.Value (Parsing_Entry).Element.Name.all);
					end if;
				when others =>
					raise Standard.XML.Data_Error;
			end case;
		end;
		if Object.Next_Kind = Value then
			declare
				Parsing_Entry : aliased Standard.XML.Parsing_Entry_Type;
			begin
				Standard.XML.Get (Object.Reader.all, Parsing_Entry);
				if Standard.XML.Value (Parsing_Entry).Element.Event_Type /=
					Standard.XML.Element_End
				then
					raise Standard.XML.Data_Error;
				end if;
			end;
		end if;
	end Read_Value;
	
	-- implementation of reading
	
	function Reading (Reader : not null access Standard.XML.Reader; Tag : String)
		return Reference_Type
	is
		pragma Suppress (Accessibility_Check);
		R : XML_Reader_Access;
		S : Serializer_Access;
		In_Controlled : Boolean := False;
	begin
		R :=
			new XML_Reader'(
				Reader => Reader,
				Next_Kind => End_Of_Stream,
				Next_Name => Null_String'Access,
				Next_Value => Null_String'Access,
				Next_Next_Name => null,
				Level => 0);
		S := new Serializer'(Direction => Reading, Reader => R);
		return Result : constant Reference_Type :=
			(Ada.Finalization.Limited_Controlled
				with
					Serializer => S,
					Serializer_Body => S,
					Reader_Body => R,
					Writer_Body => null)
		do
			pragma Unreferenced (Result);
			In_Controlled := True;
			Read_Name_On_Start (R, Tag);
			if R.Next_Kind = Value then
				Read_Value (R);
			end if;
		end return;
	exception
		when others =>
			if not In_Controlled then
				if R /= null then
					if R.Next_Name /= Null_String'Access then
						Ada.Strings.Unbounded.Free (R.Next_Name);
					end if;
					if R.Next_Value /= Null_String'Access then
						Ada.Strings.Unbounded.Free (R.Next_Value);
					end if;
					if R.Next_Next_Name /= Sequence_Item_Name'Access then
						Ada.Strings.Unbounded.Free (R.Next_Next_Name);
					end if;
					Free (R);
				end if;
				Free (S);
			end if;
			raise;
	end Reading;
	
	overriding function Next_Kind (Object : not null access XML_Reader)
		return Stream_Element_Kind is
	begin
		return Object.Next_Kind;
	end Next_Kind;
	
	overriding function Next_Name (Object : not null access XML_Reader)
		return not null access constant String is
	begin
		return Object.Next_Name;
	end Next_Name;
	
	overriding function Next_Value (Object : not null access XML_Reader)
		return not null access constant String is
	begin
		return Object.Next_Value;
	end Next_Value;
	
	overriding procedure Advance (
		Object : not null access XML_Reader;
		Position : in State) is
	begin
		Free_And_Null (Object.Next_Name);
		Free_And_Null (Object.Next_Value);
		Read_Name (Object, Position);
		if Object.Next_Kind = Value then
			Read_Value (Object);
		end if;
	end Advance;
	
	-- writing
	
	procedure Write_Element_Start (
		Object : not null access XML_Writer;
		Name : in String) is
	begin
		Standard.XML.Put (
			Object.Writer.all,
			(Event_Type => Standard.XML.Element_Start, Name => Name'Unrestricted_Access));
	end Write_Element_Start;
	
	procedure Write_Element_End (Object : not null access XML_Writer) is
	begin
		Standard.XML.Put (Object.Writer.all, (Event_Type => Standard.XML.Element_End));
	end Write_Element_End;
	
	-- implementation of writing
	
	function Writing (Writer : not null access Standard.XML.Writer; Tag : String)
		return Reference_Type
	is
		pragma Suppress (Accessibility_Check);
		W : XML_Writer_Access;
		S : Serializer_Access;
		In_Controlled : Boolean := False;
	begin
		W := new XML_Writer'(Writer => Writer, Level => 0);
		S := new Serializer'(Direction => Writing, Writer => W);
		return Result : constant Reference_Type :=
			(Ada.Finalization.Limited_Controlled
				with
					Serializer => S,
					Serializer_Body => S,
					Reader_Body => null,
					Writer_Body => W)
		do
			pragma Unreferenced (Result);
			In_Controlled := True;
			Standard.XML.Put_Document_Start (Writer.all);
			Standard.XML.Put (
				Writer.all,
				(Event_Type => Standard.XML.Document_Type,
					Name => Tag'Unrestricted_Access,
					Public_Id => null,
					System_Id => null,
					Subset => null));
			Write_Element_Start (W, Tag);
		end return;
	exception
		when others =>
			if not In_Controlled then
				Free (W);
				Free (S);
			end if;
			raise;
	end Writing;
	
	overriding procedure Put (
		Object : not null access XML_Writer;
		Name : in String;
		Item : in String) is
	begin
		if Name /= "" then
			Write_Element_Start (Object, Name);
		end if;
		Standard.XML.Put (
			Object.Writer.all,
			(Event_Type => Standard.XML.Text, Content => Item'Unrestricted_Access));
		if Name /= "" then
			Write_Element_End (Object);
		end if;
		if Object.Level = 0 then
			Write_Element_End (Object);
			Standard.XML.Put_Document_End (Object.Writer.all);
		end if;
	end Put;
	
	overriding procedure Enter_Mapping (
		Object : not null access XML_Writer;
		Name : in String) is
	begin
		if Object.Level > 0 then
			if Name /= "" then
				Write_Element_Start (Object, Name);
			else
				Write_Element_Start (Object, Sequence_Item_Name);
			end if;
		end if;
		Object.Level := Object.Level + 1;
	end Enter_Mapping;
	
	overriding procedure Leave_Mapping (Object : not null access XML_Writer) is
	begin
		Write_Element_End (Object);
		Object.Level := Object.Level - 1;
		if Object.Level = 0 then
			Standard.XML.Put_Document_End (Object.Writer.all);
		end if;
	end Leave_Mapping;
	
	overriding procedure Enter_Sequence (
		Object : not null access XML_Writer;
		Name : in String)
		renames Enter_Mapping;
	
	overriding procedure Leave_Sequence (Object : not null access XML_Writer)
		renames Leave_Mapping;
	
end Serialization.XML;
