with Ada.Characters.Latin_1;
with Ada.Command_Line;
with Ada.Directories;
with Ada.Environment_Variables;
with Ada.Streams.Stream_IO;
with Ada.Text_IO;
with XML.Streams;
procedure test_xml is
	Verbose : Boolean := False;
	procedure Put (Item : in String) is
	begin
		if Verbose then
			Ada.Text_IO.Put (Item);
		end if;
	end Put;
	procedure New_Line is
	begin
		if Verbose then
			Ada.Text_IO.New_Line;
		end if;
	end New_Line;
	use type XML.Encoding_Type;
	use type XML.Event_Type;
	use type XML.Standalone_Type;
	package Latin_1 renames Ada.Characters.Latin_1;
	function "=" (Left, Right : XML.Event) return Boolean is
	begin
		if Left.Event_Type /= Right.Event_Type then
			return False;
		else
			case Left.Event_Type is
				when XML.Element_Start | XML.Attribute | XML.Processing_Instruction
					| XML.Document_Type =>
					if Left.Name.all /= Right.Name.all then
						return False;
					else
						case Left.Event_Type is
							when XML.Attribute =>
								return Left.Value.all = Right.Value.all;
							when XML.Document_Type =>
								if (Left.Public_Id /= null or else Right.Public_Id /= null)
									and then (
										Left.Public_Id = null
											or else Right.Public_Id = null
											or else Left.Public_Id.all /= Right.Public_Id.all)
								then
									return False;
								end if;
								if (Left.System_Id /= null or else Right.System_Id /= null)
									and then (
										Left.System_Id = null
											or else Right.System_Id = null
											or else Left.System_Id.all /= Right.System_Id.all)
								then
									return False;
								end if;
								if (Left.Subset /= null or else Right.Subset /= null)
									and then (
										Left.Subset = null
											or else Right.Subset = null
											or else Left.Subset.all /= Right.Subset.all)
								then
									return False;
								end if;
								return True;
							when others =>
								return True;
						end case;
					end if;
				when XML.Text | XML.CDATA | XML.Comment | XML.Whitespace
					| XML.Significant_Whitespace =>
					return Left.Content.all = Right.Content.all;
				when others =>
					return True;
			end case;
		end if;
	end "=";
	Test_File_Name : constant String :=
		Ada.Directories.Compose (
			Ada.Environment_Variables.Value ("TMPDIR", Default => "/tmp"),
			"test_xml.xml");
	type Event_Constant is access constant XML.Event;
	The_Name_1 : aliased constant String := "root";
	The_Content_1 : aliased constant String := "Hello,";
	The_Name_2 : aliased constant String := "sub";
	The_Name_3 : aliased constant String := "attr";
	The_Value_1 : aliased constant String := "<>&" & Latin_1.HT & Latin_1.LF;
	The_Content_2 : aliased constant String := "XML!";
	The_Content_3 : aliased constant String := " comment ";
	Data : constant array (Positive range <>) of not null Event_Constant := (
		new XML.Event'(
			Event_Type => XML.Document_Type,
			Name => The_Name_1'Unchecked_Access,
			Public_Id => null,
			System_Id => null,
			Subset => null),
		new XML.Event'(
			Event_Type => XML.Element_Start,
			Name => The_Name_1'Unchecked_Access),
		new XML.Event'(
			Event_Type => XML.Text,
			Content => The_Content_1'Unchecked_Access),
		new XML.Event'(
			Event_Type => XML.Element_Start,
			Name => The_Name_2'Unchecked_Access),
		new XML.Event'(
			Event_Type => XML.Attribute,
			Name => The_Name_3'Unchecked_Access,
			Value => The_Value_1'Unchecked_Access),
		new XML.Event'(
			Event_Type => XML.Element_End),
		new XML.Event'(
			Event_Type => XML.Text,
			Content => The_Content_2'Unchecked_Access),
		new XML.Event'(
			Event_Type => XML.Element_End),
		new XML.Event'(
			Event_Type => XML.Comment,
			Content => The_Content_3'Unchecked_Access));
	Version_1_0 : aliased constant String := "1.0";
	UTF_8 : constant XML.Encoding_Type := XML.Find ("utf-8");
begin
	-- options
	for I in 1 .. Ada.Command_Line.Argument_Count loop
		declare
			A : constant String := Ada.Command_Line.Argument (I);
		begin
			if A = "--verbose" then
				Verbose := True;
			else
				Ada.Text_IO.Put_Line (Ada.Text_IO.Standard_Error.all, "unknown option: " & A);
				Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
				return;
			end if;
		end;
	end loop;
	-- writer
	declare
		W : XML.Writer := XML.Create (Put'Access, UTF_8);
	begin
		XML.Set_Indent (W, (1 => Latin_1.HT));
		XML.Put_Document_Start (W, Version_1_0'Access, UTF_8, XML.Yes);
		for I in Data'Range loop
			XML.Put (W, Data (I).all);
		end loop;
		XML.Put_Document_End (W);
		XML.Finish (W);
	end;
	declare
		File : Ada.Streams.Stream_IO.File_Type;
	begin
		Ada.Streams.Stream_IO.Create (File, Name => Test_File_Name);
		declare
			W : XML.Writer :=
				XML.Streams.Create (Ada.Streams.Stream_IO.Stream (File), UTF_8);
		begin
			Put ("Writing...");
			XML.Put_Document_Start (W, Version_1_0'Access, UTF_8, XML.Yes);
			for I in Data'Range loop
				Put (I'Img);
				XML.Put (W, Data (I).all);
			end loop;
			XML.Put_Document_End (W);
			XML.Finish (W);
			Put (" ok");
			New_Line;
		end;
		Ada.Streams.Stream_IO.Close (File);
	end;
	-- reader
	declare
		File : Ada.Streams.Stream_IO.File_Type;
	begin
		Ada.Streams.Stream_IO.Open (File, Ada.Streams.Stream_IO.In_File,
			Name => Test_File_Name);
		declare
			R : XML.Reader :=
				XML.Streams.Create (Ada.Streams.Stream_IO.Stream (File), UTF_8);
		begin
			if XML.Version (R).all /= Version_1_0 then
				raise Program_Error;
			end if;
			if XML.Encoding (R) /= UTF_8 then
				raise Program_Error;
			end if;
			if XML.Standalone (R) /= XML.Yes then
				raise Program_Error;
			end if;
			Put ("Reading...");
			for I in Data'Range loop
				Put (I'Img);
				declare
					procedure Process (Event : in XML.Event) is
					begin
						if Event /= Data (I).all then
							raise Program_Error;
						end if;
					end Process;
				begin
					XML.Get (R, Process'Access);
				end;
			end loop;
			Put (" ok");
			New_Line;
		end;
		Ada.Streams.Stream_IO.Close (File);
	end;
	-- finish
	Ada.Text_IO.Put_Line (Ada.Text_IO.Standard_Error.all, "ok");
end test_xml;
