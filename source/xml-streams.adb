with Ada.IO_Exceptions;
with System.Address_To_Access_Conversions;
with C.libxml.tree;
with C.libxml.xmlIO;
package body XML.Streams is
	use type C.libxml.tree.xmlOutputBufferPtr;
	use type C.libxml.xmlreader.xmlTextReaderPtr;
	use type C.libxml.xmlwriter.xmlTextWriterPtr;
	
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
		package Conv is
			new System.Address_To_Access_Conversions (
				Ada.Streams.Root_Stream_Type'Class);
		Stream : constant Conv.Object_Pointer :=
			Conv.To_Pointer (System.Address (context));
		Item : Ada.Streams.Stream_Element_Array (
			1 ..
			Ada.Streams.Stream_Element_Offset (len));
		for Item'Address use buffer.all'Address;
		Last : Ada.Streams.Stream_Element_Offset;
	begin
		begin
			Ada.Streams.Read (Stream.all, Item, Last);
		exception
			when Ada.IO_Exceptions.End_Error => Last := 0;
		end;
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
		package Conv is
			new System.Address_To_Access_Conversions (
				Ada.Streams.Root_Stream_Type'Class);
		Stream : constant Conv.Object_Pointer :=
			Conv.To_Pointer (System.Address (context));
		Item : Ada.Streams.Stream_Element_Array (
			1 ..
			Ada.Streams.Stream_Element_Offset (len));
		for Item'Address use buffer.all'Address;
	begin
		Ada.Streams.Write (Stream.all, Item);
		return len;
	end Write_Handler;
	
	-- implementation
	
	function Create (
		Stream : not null access Ada.Streams.Root_Stream_Type'Class;
		Encoding : Encoding_Type := No_Encoding;
		URI : String := "")
		return Reader
	is
		package Conv is
			new System.Address_To_Access_Conversions (
				Ada.Streams.Root_Stream_Type'Class);
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
						C.void_ptr (Conv.To_Address (Conv.Object_Pointer (Stream))),
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
	
	function Create (
		Stream : not null access Ada.Streams.Root_Stream_Type'Class;
		Encoding : Encoding_Type := No_Encoding;
		Version : access constant String := null;
		Standalone : Standalone_Type := No_Specific)
		return Writer
	is
		package Conv is
			new System.Address_To_Access_Conversions (
				Ada.Streams.Root_Stream_Type'Class);
	begin
		Check_Version;
		Install_Error_Handlers;
		declare
			Buffer : constant C.libxml.tree.xmlOutputBufferPtr :=
				C.libxml.xmlIO.xmlOutputBufferCreateIO (
					Write_Handler'Access,
					null,
					C.void_ptr (Conv.To_Address (Conv.Object_Pointer (Stream))),
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
	
end XML.Streams;
