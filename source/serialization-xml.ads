with XML;
private with Ada.Finalization;
package Serialization.XML is
	pragma Preelaborate;
	
	type Reference_Type (
		Serializer : not null access Serialization.Serializer) is limited private;
	
	function Reading (
		Reader : not null access Standard.XML.Reader;
		Tag : String)
		return Reference_Type;
	
	function Writing (
		Writer : not null access Standard.XML.Writer;
		Tag : String)
		return Reference_Type;
	
private
	
	type Serializer_Access is access Serializer;
	
	type XML_Reader;
	type XML_Reader_Access is access XML_Reader;
	type XML_Writer;
	type XML_Writer_Access is access XML_Writer;
	
	type Reference_Type (
		Serializer : not null access Serializer) is
		limited new Ada.Finalization.Limited_Controlled
		with record
			Serializer_Body : Serializer_Access;
			Reader_Body : XML_Reader_Access;
			Writer_Body : XML_Writer_Access;
		end record;
	
	overriding procedure Finalize (Object : in out Reference_Type);
	
	-- reading
	
	type XML_Reader is limited new Serialization.Reader
		with record
			Reader : not null access Standard.XML.Reader;
			Next_Kind : Stream_Element_Kind;
			Next_Name : Ada.Strings.Unbounded.String_Access;
			Next_Value : Ada.Strings.Unbounded.String_Access;
			Next_Next_Name : Ada.Strings.Unbounded.String_Access;
			Level : Natural;
		end record;
	
	overriding function Next_Kind (Object : not null access XML_Reader)
		return Stream_Element_Kind;
	overriding function Next_Name (Object : not null access XML_Reader)
		return not null access constant String;
	overriding function Next_Value (Object : not null access XML_Reader)
		return not null access constant String;
	overriding procedure Advance (
		Object : not null access XML_Reader;
		Position : in State);
	
	-- writing
	
	type XML_Writer is limited new Serialization.Writer
		with record
			Writer : not null access Standard.XML.Writer;
			Level : Natural;
		end record;
	
	overriding procedure Put (
		Object : not null access XML_Writer;
		Name : in String;
		Item : in String);
	overriding procedure Enter_Mapping (
		Object : not null access XML_Writer;
		Name : in String);
	overriding procedure Leave_Mapping (
		Object : not null access XML_Writer);
	overriding procedure Enter_Sequence (
		Object : not null access XML_Writer;
		Name : in String);
	overriding procedure Leave_Sequence (
		Object : not null access XML_Writer);
	
end Serialization.XML;
