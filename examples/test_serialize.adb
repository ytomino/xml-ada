with Ada.Command_Line;
with Ada.Directories;
with Ada.Environment_Variables;
with Ada.Streams.Stream_IO;
with Ada.Strings.Unbounded;
with Ada.Text_IO;
with Serialization.XML;
with XML.Streams;
procedure test_serialize is
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
	Test_File_Name : constant String :=
		Ada.Directories.Compose (
			Ada.Environment_Variables.Value ("TMPDIR", Default => "/tmp"),
			"test_serialize.xml");
	type Nested_Map is record
		A : Integer;
	end record;
	procedure IO (
		S : not null access Serialization.Serializer;
		Name : String;
		Var : in out Nested_Map)
	is
		procedure Process is
		begin
			Serialization.IO (S, "A", Var.A);
		end Process;
	begin
		Serialization.IO (S, Name, Process'Access);
	end IO;
	type T is record
		X : Ada.Strings.Unbounded.Unbounded_String;
		Y : Boolean;
		Z : Nested_Map;
	end record;
	procedure IO (S : not null access Serialization.Serializer; Var : in out T) is
		procedure Process is
		begin
			Serialization.IO (S, "X", Var.X);
			Serialization.IO (S, "Y", Var.Y);
			IO (S, "Z", Var.Z);
		end Process;
	begin
		Serialization.IO (S, Process'Access);
	end IO;
	Root_Tag : constant String := "ROOT-TAG";
	Data : T := (
		X => Ada.Strings.Unbounded.To_Unbounded_String ("XYZ"),
		Y => True,
		Z => (A => 100));
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
		W : aliased XML.Writer := XML.Create (Put'Access);
	begin
		IO (Serialization.XML.Writing (W'Access, Root_Tag).Serializer, Data);
		XML.Flush (W);
	end;
	declare
		File : Ada.Streams.Stream_IO.File_Type;
	begin
		Ada.Streams.Stream_IO.Create (File, Name => Test_File_Name);
		declare
			W : aliased XML.Writer :=
				XML.Streams.Create (Ada.Streams.Stream_IO.Stream (File));
		begin
			Put ("Writing...");
			IO (Serialization.XML.Writing (W'Access, Root_Tag).Serializer, Data);
			XML.Flush (W);
			Put (" ok");
			New_Line;
		end;
		Ada.Streams.Stream_IO.Close (File);
	end;
	-- reader
	declare
		File : Ada.Streams.Stream_IO.File_Type;
		Data2 : T := (
			X => Ada.Strings.Unbounded.Null_Unbounded_String,
			Y => False,
			Z => (A => 0));
	begin
		Ada.Streams.Stream_IO.Open (File, Ada.Streams.Stream_IO.In_File,
			Name => Test_File_Name);
		declare
			R : aliased XML.Reader :=
				XML.Streams.Create (Ada.Streams.Stream_IO.Stream (File));
		begin
			Put ("Reading...");
			IO (Serialization.XML.Reading (R'Access, Root_Tag).Serializer, Data2);
			Put (" ok");
			New_Line;
		end;
		Ada.Streams.Stream_IO.Close (File);
		declare
			W : aliased XML.Writer := XML.Create (Put'Access);
		begin
			IO (Serialization.XML.Writing (W'Access, Root_Tag).Serializer, Data2);
			XML.Flush (W);
		end;
		if Data2 /= Data then
			raise Program_Error;
		end if;
	end;
	-- finish
	Ada.Text_IO.Put_Line (Ada.Text_IO.Standard_Error.all, "ok");
end test_serialize;
