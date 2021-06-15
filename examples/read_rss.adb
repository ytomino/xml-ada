with Ada.Characters.Latin_1;
with Ada.Command_Line;
with Ada.Streams.Stream_IO;
with Ada.Text_IO;
with XML.Streams;
procedure read_rss is
	package Latin_1 renames Ada.Characters.Latin_1;
	procedure Read_RSS (Reader : in out XML.Reader) is
		type RSS_Version_Type is (RSS_1, RSS_2);
		RSS_Version : RSS_Version_Type;
	begin
		loop
			declare
				type T1 is (RSS_1, RSS_2, Processing_Instruction);
				S1 : T1;
				procedure Process (Event : in XML.Event) is
				begin
					case Event.Event_Type is
						when XML.Element_Start =>
							if Event.Name.all = "rdf:RDF" then
								S1 := RSS_1;
							elsif Event.Name.all = "rss" then
								S1 := RSS_2;
							else
								raise XML.Data_Error;
							end if;
						when XML.Processing_Instruction =>
							S1 := Processing_Instruction;
						when others =>
							raise XML.Data_Error;
					end case;
				end Process;
			begin
				XML.Read (Reader, Process'Access);
				case S1 is
					when RSS_1 =>
						Ada.Text_IO.Put_Line (XML.Base_URI (Reader) & " is RSS 1.0.");
						RSS_Version := RSS_1;
						exit;
					when RSS_2 =>
						Ada.Text_IO.Put_Line (XML.Base_URI (Reader) & " is RSS 2.0.");
						RSS_Version := RSS_2;
						exit;
					when Processing_Instruction =>
						null; -- read one more event
				end case;
			end;
		end loop;
		case RSS_Version is
			when RSS_1 =>
				declare
					type T is
						(Root, Channel, Title, Link, Creator, Item, Item_Title, Item_Link, Unknown);
					S : array (1 .. 10) of T := (1 => Root, others => Unknown);
					Top : Natural := S'First;
					procedure Push (N : T) is
					begin
						Top := Top + 1;
						S (Top) := N;
					end Push;
					procedure Process (Event : in XML.Event) is
					begin
						case S (Top) is
							when Root =>
								case Event.Event_Type is
									when XML.Attribute =>
										null; -- skip xmlns
									when XML.Element_Start =>
										if Event.Name.all = "channel" then
											Push (Channel);
										elsif Event.Name.all = "item" then
											Push (Item);
										else
											Push (Unknown);
										end if;
									when XML.Significant_Whitespace =>
										null;
									when XML.Element_End =>
										Top := Top - 1;
									when others =>
										raise XML.Data_Error;
								end case;
							when Channel =>
								case Event.Event_Type is
									when XML.Attribute =>
										if Event.Name.all = "rdf:about" then
											Ada.Text_IO.Put_Line (Latin_1.HT & "about: " & Event.Value.all);
										end if;
									when XML.Element_Start =>
										if Event.Name.all = "title" then
											Push (Title);
										elsif Event.Name.all = "link" then
											Push (Link);
										elsif Event.Name.all = "dc:creator" then
											Push (Creator);
										else
											Push (Unknown);
										end if;
									when XML.Significant_Whitespace =>
										null;
									when XML.Element_End =>
										Top := Top - 1;
									when others =>
										raise XML.Data_Error;
								end case;
							when Title =>
								case Event.Event_Type is
									when XML.Text =>
										Ada.Text_IO.Put_Line (Latin_1.HT & "title: " & Event.Content.all);
									when XML.Element_End =>
										Top := Top - 1;
									when others =>
										raise XML.Data_Error;
								end case;
							when Link =>
								case Event.Event_Type is
									when XML.Text =>
										Ada.Text_IO.Put_Line (Latin_1.HT & "link: " & Event.Content.all);
									when XML.Element_End =>
										Top := Top - 1;
									when others =>
										raise XML.Data_Error;
								end case;
							when Creator =>
								case Event.Event_Type is
									when XML.Text =>
										Ada.Text_IO.Put_Line (Latin_1.HT & "creator: " & Event.Content.all);
									when XML.Element_End =>
										Top := Top - 1;
									when others =>
										raise XML.Data_Error;
								end case;
							when Item =>
								case Event.Event_Type is
									when XML.Attribute =>
										if Event.Name.all = "rdf:about" then
											Ada.Text_IO.Put_Line (Latin_1.HT & "item about: " & Event.Value.all);
										end if;
									when XML.Element_Start =>
										if Event.Name.all = "title" then
											Push (Item_Title);
										elsif Event.Name.all = "link" then
											Push (Item_Link);
										else
											Push (Unknown);
										end if;
									when XML.Element_End =>
										Top := Top - 1;
									when XML.Significant_Whitespace =>
										null;
									when others =>
										raise XML.Data_Error;
								end case;
							when Item_Title =>
								case Event.Event_Type is
									when XML.Text =>
										Ada.Text_IO.Put_Line (Latin_1.HT & "item title: " & Event.Content.all);
									when XML.Element_End =>
										Top := Top - 1;
									when others =>
										raise XML.Data_Error;
								end case;
							when Item_Link =>
								case Event.Event_Type is
									when XML.Text =>
										Ada.Text_IO.Put_Line (Latin_1.HT & "item link: " & Event.Content.all);
									when XML.Element_End =>
										Top := Top - 1;
									when others =>
										raise XML.Data_Error;
								end case;
							when Unknown =>
								raise Program_Error;
						end case;
					end Process;
				begin
					loop
						if S (Top) = Unknown then
							XML.Read_Until_Element_End (Reader);
							Top := Top - 1;
						else
							XML.Read (Reader, Process'Access);
						end if;
						exit when Top < S'First;
					end loop;
				end;
			when RSS_2 =>
				declare
					type T is
						(Root, Channel, Title, Link, Creator, Item, Item_Title, Item_Link, Unknown);
					S : array (1 .. 10) of T := (1 => Root, others => Unknown);
					Top : Natural := S'First;
					procedure Push (N : T) is
					begin
						Top := Top + 1;
						S (Top) := N;
					end Push;
					procedure Process (Event : in XML.Event) is
					begin
						case S (Top) is
							when Root =>
								case Event.Event_Type is
									when XML.Attribute =>
										null; -- skip xmlns
									when XML.Element_Start =>
										if Event.Name.all = "channel" then
											Push (Channel);
										else
											Push (Unknown);
										end if;
									when XML.Significant_Whitespace =>
										null;
									when XML.Element_End =>
										Top := Top - 1;
									when others =>
										raise XML.Data_Error;
								end case;
							when Channel =>
								case Event.Event_Type is
									when XML.Element_Start =>
										if Event.Name.all = "title" then
											Push (Title);
										elsif Event.Name.all = "link" then
											Push (Link);
										elsif Event.Name.all = "dc:creator" then
											Push (Creator);
										elsif Event.Name.all = "item" then
											Push (Item);
										else
											Push (Unknown);
										end if;
									when XML.Significant_Whitespace =>
										null;
									when XML.Element_End =>
										Top := Top - 1;
									when others =>
										raise XML.Data_Error;
								end case;
							when Title =>
								case Event.Event_Type is
									when XML.Text =>
										Ada.Text_IO.Put_Line (Latin_1.HT & "title: " & Event.Content.all);
									when XML.Element_End =>
										Top := Top - 1;
									when others =>
										raise XML.Data_Error;
								end case;
							when Link =>
								case Event.Event_Type is
									when XML.Text =>
										Ada.Text_IO.Put_Line (Latin_1.HT & "link: " & Event.Content.all);
									when XML.Element_End =>
										Top := Top - 1;
									when others =>
										raise XML.Data_Error;
								end case;
							when Creator =>
								case Event.Event_Type is
									when XML.Text =>
										Ada.Text_IO.Put_Line (Latin_1.HT & "creator: " & Event.Content.all);
									when XML.Element_End =>
										Top := Top - 1;
									when others =>
										raise XML.Data_Error;
								end case;
							when Item =>
								case Event.Event_Type is
									when XML.Element_Start =>
										if Event.Name.all = "title" then
											Push (Item_Title);
										elsif Event.Name.all = "link" then
											Push (Item_Link);
										else
											Push (Unknown);
										end if;
									when XML.Element_End =>
										Top := Top - 1;
									when XML.Significant_Whitespace =>
										null;
									when others =>
										raise XML.Data_Error;
								end case;
							when Item_Title =>
								case Event.Event_Type is
									when XML.Text =>
										Ada.Text_IO.Put_Line (Latin_1.HT & "item title: " & Event.Content.all);
									when XML.Element_End =>
										Top := Top - 1;
									when others =>
										raise XML.Data_Error;
								end case;
							when Item_Link =>
								case Event.Event_Type is
									when XML.Text =>
										Ada.Text_IO.Put_Line (Latin_1.HT & "item link: " & Event.Content.all);
									when XML.Element_End =>
										Top := Top - 1;
									when others =>
										raise XML.Data_Error;
								end case;
							when Unknown =>
								raise Program_Error;
						end case;
					end Process;
				begin
					loop
						if S (Top) = Unknown then
							XML.Read_Until_Element_End (Reader);
							Top := Top - 1;
						else
							XML.Read (Reader, Process'Access);
						end if;
						exit when Top < S'First;
					end loop;
				end;
		end case;
	end Read_RSS;
	procedure Read_RSS_From_File (Name : in String) is
		File : Ada.Streams.Stream_IO.File_Type;
	begin
		Ada.Streams.Stream_IO.Open (File, Ada.Streams.Stream_IO.In_File, Name => Name);
		declare
			R : XML.Reader :=
				XML.Streams.Create (Ada.Streams.Stream_IO.Stream (File),
					URI => "file://" & Name);
		begin
			Read_RSS (R);
		end;
		Ada.Streams.Stream_IO.Close (File);
	end Read_RSS_From_File;
begin
	if Ada.Command_Line.Argument_Count = 0 then
		Ada.Text_IO.Put_Line ("please tell RSS file name.");
	else
		for I in 1 .. Ada.Command_Line.Argument_Count loop
			Read_RSS_From_File (Ada.Command_Line.Argument (I));
		end loop;
	end if;
end read_rss;
