with Ada.Characters.Latin_1;
with Ada.Command_Line;
with Ada.Text_IO;
with XML;
procedure test_reader_error is
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
	use type XML.Event_Type;
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
	-- error
	declare
		use Ada.Characters.Latin_1;
		Source : constant String :=
			"<root>" & LF
			& " <tag>" & LF
			& " </tagu>" & LF -- error
			& "</root>" & LF;
		Source_First : Positive := Source'First;
		procedure Read (Item : out String; Last : out Natural) is
			R : constant Integer :=
				Integer'Min (Item'Length - 1, Source'Last - Source_First);
			Source_Last : constant Natural := Source_First + R;
		begin
			Last := Source'First + R;
			Item (Item'First .. Last) := Source (Source_First .. Source_Last);
			Source_First := Source_Last + 1;
		end Read;
		R : aliased XML.Reader := XML.Create (Read'Access);
	begin
		declare
			Line : constant Natural := XML.Last_Error_Line (R);
			Message : constant String := XML.Last_Error_Message (R);
		begin
			if Line /= 0 or else Message /= "" then
				raise Program_Error;
			end if;
		end;
		loop
			declare
				E : aliased XML.Parsing_Entry_Type;
			begin
				XML.Get (R, E);
				declare
					V : XML.Event renames XML.Value (E);
				begin
					Put (XML.Event_Type'Image (V.Event_Type));
					New_Line;
					exit when V.Event_Type = XML.No_Event;
				end;
			end;
		end loop;
		raise Program_Error;
	exception
		when XML.Data_Error =>
			declare
				Line : constant Natural := XML.Last_Error_Line (R);
			begin
				Put (Integer'Image (Line));
				Put (": ");
				Put (XML.Last_Error_Message (R));
					-- "Opening and ending tag mismatch: tag line 1 and tagu"
				New_Line;
				if Line /= 2 then
					raise Program_Error;
				end if;
			end;
	end;
	-- finish
	Ada.Text_IO.Put_Line (Ada.Text_IO.Standard_Error.all, "ok");
end test_reader_error;
