with Ada.Streams.Stream_IO;
with Ada.Strings.Unbounded;
with Ada.Text_IO;
with Serialization.XML;
with XML.Streams;
procedure test_serialize is
	Test_File_Name : constant String := "test_serialize.xml";
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
	declare
		W : aliased XML.Writer := XML.Create (Ada.Text_IO.Put'Access);
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
			Ada.Text_IO.Put ("Writing...");
			IO (Serialization.XML.Writing (W'Access, Root_Tag).Serializer, Data);
			XML.Flush (W);
			Ada.Text_IO.Put_Line (" ok");
		end;
		Ada.Streams.Stream_IO.Close (File);
	end;
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
			Ada.Text_IO.Put ("Reading...");
			IO (Serialization.XML.Reading (R'Access, Root_Tag).Serializer, Data2);
			Ada.Text_IO.Put_Line (" ok");
		end;
		Ada.Streams.Stream_IO.Close (File);
		declare
			W : aliased XML.Writer := XML.Create (Ada.Text_IO.Put'Access);
		begin
			IO (Serialization.XML.Writing (W'Access, Root_Tag).Serializer, Data2);
			XML.Flush (W);
		end;
		if Data2 /= Data then
			raise Program_Error;
		end if;
	end;
end test_serialize;
