with Ada.Characters.Latin_1;
with Ada.Command_Line;
with Ada.Exceptions;
with Ada.Integer_Text_IO;
with Ada.Streams.Stream_IO;
with Ada.Text_IO;
with XML.Streams;
procedure dump_event is
	use type XML.Encoding_Type;
	use type XML.Event_Type;
	use type XML.Standalone_Type;
	procedure Dump_One_Event (Event : in XML.Event) is
		use Ada.Characters.Latin_1;
		use Ada.Integer_Text_IO;
		use Ada.Text_IO;
	begin
		Put_Line (XML.Event_Type'Image (Event.Event_Type));
		case Event.Event_Type is
			when XML.Element_Start | XML.Attribute | XML.Processing_Instruction
				| XML.Document_Type =>
				Put (HT);
				Put (Event.Name.all);
				case Event.Event_Type is
					when XML.Attribute =>
						Put (" = """);
						Put (Event.Value.all);
						Put ('"');
					when XML.Document_Type =>
						if Event.Public_Id /= null then
							Put (" public id = ");
							Put (Event.Public_Id.all);
						end if;
						if Event.System_Id /= null then
							Put (" system id = ");
							Put (Event.System_Id.all);
						end if;
						if Event.Subset /= null then
							Put (" subset = ");
							Put (Event.Subset.all);
						end if;
					when others =>
						null;
				end case;
				New_Line;
			when XML.Text | XML.CDATA | XML.Comment =>
				Put (HT & '"');
				Put (Event.Content.all);
				Put ('"');
				New_Line;
			when XML.Whitespace | XML.Significant_Whitespace =>
				Put (HT);
				Put (Event.Content'Length, Width => 1);
				Put (" whitespaces");
				New_Line;
			when others =>
				null;
		end case;
	end Dump_One_Event;
	procedure Dump_XML (Reader : in out XML.Reader) is
		use Ada.Text_IO;
	begin
		Put_Line ("version = " & XML.Version (Reader).all);
		declare
			Encoding : constant XML.Encoding_Type := XML.Encoding (Reader);
		begin
			Put ("encoding = ");
			if Encoding = XML.No_Encoding then
				Put ("none");
			else
				Put (XML.Name (Encoding));
			end if;
			New_Line;
		end;
		declare
			Standalone : constant XML.Standalone_Type := XML.Standalone (Reader);
		begin
			Put ("standalone = ");
			if Standalone = XML.No_Specific then
				Put ("none");
			else
				Put (XML.Standalone_Type'Image (Standalone));
			end if;
			New_Line;
		end;
		Put_Line ("base URI = " & XML.Base_URI (Reader));
		loop
			declare
				Parsing_Entry : aliased XML.Parsing_Entry_Type;
			begin
				XML.Read (Reader, Parsing_Entry);
				declare
					Event : XML.Event renames XML.Value (Parsing_Entry);
				begin
					exit when Event.Event_Type = XML.No_Event;
					Dump_One_Event (Event);
				end;
			end;
		end loop;
	end Dump_XML;
	procedure Read_From_File (Name : in String) is
		File : Ada.Streams.Stream_IO.File_Type;
	begin
		Ada.Streams.Stream_IO.Open (File, Ada.Streams.Stream_IO.In_File, Name => Name);
		declare
			Reader : XML.Reader :=
				XML.Streams.Create (Ada.Streams.Stream_IO.Stream (File),
					URI => "file://" & Name);
		begin
			Dump_XML (Reader);
		exception
			when E : XML.Data_Error =>
				Ada.Text_IO.Put_Line (
					Ada.Text_IO.Current_Error.all,
					"error: " & Ada.Exceptions.Exception_Message (E));
		end;
		Ada.Streams.Stream_IO.Close (File);
	end Read_From_File;
begin
	if Ada.Command_Line.Argument_Count = 0 then
		Ada.Text_IO.Put_Line ("please tell XML file name.");
	else
		for I in 1 .. Ada.Command_Line.Argument_Count loop
			declare
				Name : constant String := Ada.Command_Line.Argument (I);
			begin
				if Ada.Command_Line.Argument_Count > 1 then
					Ada.Text_IO.Put_Line (Name & ":");
				end if;
				Read_From_File (Name);
			end;
		end loop;
	end if;
end dump_event;
