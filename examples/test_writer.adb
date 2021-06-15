-- testWriter.c
--
-- section: xmlWriter
-- synopsis: use various APIs for the xmlWriter
-- purpose: tests a number of APIs for the xmlWriter, especially
--          the various methods to write to a filename, to a memory
--          buffer, to a new document, or to a subtree. It shows how to
--          do encoding string conversions too. The resulting
--          documents are then serialized.
-- usage: testWriter
-- test: testWriter ;
--       for i in 1 2 3 4 ; do diff writer.xml writer$$i.res ; done ;
--       rm writer*.res
-- author: Alfred Mickautsch
-- copy: see Copyright for the status of this software.
--
-- Ada version by yt
-- This file is encoded as Latin-1
--
with Ada.Text_IO;
with Ada.Unchecked_Conversion;
with System;
with C.stdio;
with C.string;
with C.libxml.encoding;
with C.libxml.xmlwriter;
with C.libxml.globals;
with C.libxml.parser;
with C.libxml.tree;
with C.libxml.xmlmemory;
with C.libxml.xmlstring;
with C.libxml.xmlversion;
--pragma Compile_Time_Error (
--	not C.libxml.xmlversion.LIBXML_WRITER_ENABLED
--		or else not C.libxml.xmlversion.LIBXML_OUTPUT_ENABLED,
--	"Writer or output support not compiled in");
procedure test_writer is
	pragma Linker_Options ("-lxml2");
	use type C.char;
	use type C.char_array;
	use type C.signed_int;
	use type C.size_t;
	use type C.libxml.encoding.xmlCharEncodingHandlerPtr;
	use type C.libxml.tree.xmlBufferPtr;
	use type C.libxml.tree.xmlDocPtr;
	use type C.libxml.tree.xmlNodePtr;
	use type C.libxml.xmlstring.xmlChar_ptr;
	use type C.libxml.xmlwriter.xmlTextWriterPtr;
	function To_void_ptr is
		new Ada.Unchecked_Conversion (
			C.libxml.xmlstring.xmlChar_ptr,
			C.void_ptr);
	function To_char_const_ptr is
		new Ada.Unchecked_Conversion (
			C.libxml.xmlstring.xmlChar_ptr,
			C.char_const_ptr);
	function To_unsigned_char_const_ptr is
		new Ada.Unchecked_Conversion (
			C.char_const_ptr,
			C.unsigned_char_const_ptr);
	function To_xmlChar_ptr is
		new Ada.Unchecked_Conversion (
			C.void_ptr,
			C.libxml.xmlstring.xmlChar_ptr);
	function To_xmlChar_const_ptr is
		new Ada.Unchecked_Conversion (
			C.char_const_ptr,
			C.libxml.xmlstring.xmlChar_ptr);
	MY_ENCODING : constant C.char_array := "ISO-8859-1" & C.char'Val (0);
	-- ConvertInput:
	-- @in: string in a given encoding
	-- @encoding: the encoding used
	--
	-- Converts @in into UTF-8 for processing with libxml2 APIs
	--
	-- Returns the converted UTF-8 string, or NULL in case of error.
	function ConvertInput (A_in : C.char_array; encoding : C.char_array)
		return C.libxml.xmlstring.xmlChar_ptr
	is
		pragma Assert (A_in'Length > 0 and then A_in (A_in'Last) = C.char'Val (0));
		pragma Assert (
			encoding'Length > 0 and then encoding (encoding'Last) = C.char'Val (0));
		L_out : C.libxml.xmlstring.xmlChar_ptr;
		ret : C.signed_int;
		size : C.signed_int;
		out_size : aliased C.signed_int;
		temp : aliased C.signed_int;
		handler : C.libxml.encoding.xmlCharEncodingHandlerPtr;
	begin
		handler :=
			C.libxml.encoding.xmlFindCharEncodingHandler (
				encoding (encoding'First)'Access);
		if handler = null then
			declare
				encoding_String : String (1 .. encoding'Length - 1);
				for encoding_String'Address use encoding'Address;
			begin
				raise Program_Error
					with "ConvertInput: no encoding handler found for '" & encoding_String & "'";
			end;
		end if;
		size := C.signed_int (C.string.strlen (A_in (A_in'First)'Access)) + 1;
		out_size := size * 2 - 1;
		L_out := To_xmlChar_ptr (C.libxml.globals.xmlMalloc (C.size_t (out_size)));
		if L_out /= null then
			temp := size - 1;
			ret :=
				handler.input (
					L_out,
					out_size'Access,
					To_unsigned_char_const_ptr (A_in (A_in'First)'Unchecked_Access),
					temp'Access);
			if ret < 0 or else temp - size + 1 /= 0 then
				C.libxml.globals.xmlFree (To_void_ptr (L_out));
				L_out := null;
				if ret < 0 then
					raise Program_Error with "ConvertInput: conversion wasn't successful.";
				else
					raise Program_Error
						with "ConvertInput: conversion wasn't successful. converted:"
							& C.signed_int'Image (temp) & " octets.";
				end if;
			else
				L_out :=
					To_xmlChar_ptr (
						C.libxml.globals.xmlRealloc (To_void_ptr (L_out), C.size_t (out_size) + 1));
				declare
					out_Array : array (0 .. C.size_t (out_size)) of C.libxml.xmlstring.xmlChar
						with Convention => C;
					for out_Array'Address use System.Address (To_void_ptr (L_out));
				begin
					out_Array (C.size_t (out_size)) := C.libxml.xmlstring.xmlChar'Val (0);
					-- null terminating out
				end;
			end if;
		else
			raise Program_Error with "ConvertInput: no mem";
		end if;
		return L_out;
	end ConvertInput;
	-- shared
	procedure Write_Sample_XML (writer : C.libxml.xmlwriter.xmlTextWriterPtr) is
		rc : C.signed_int;
		tmp : C.libxml.xmlstring.xmlChar_ptr;
	begin
		-- Write a comment as child of EXAMPLE.
		-- Please observe, that the input to the xmlTextWriter functions
		-- HAS to be in UTF-8, even if the output XML is encoded
		-- in iso-8859-1
		tmp :=
			ConvertInput (
				"This is a comment with special chars: <" & C.char'Val (16#E4#)
					& C.char'Val (16#F6#) & C.char'Val (16#FC#) & ">" & C.char'Val (0),
				MY_ENCODING);
		rc := C.libxml.xmlwriter.xmlTextWriterWriteComment (writer, tmp);
		if rc < 0 then
			raise Program_Error
				with "testXmlwriterTree: Error at xmlTextWriterWriteComment";
		end if;
		if tmp /= null then
			C.libxml.globals.xmlFree (To_void_ptr (tmp));
		end if;
		-- Start an element named "ORDER" as child of EXAMPLE.
		declare
			Name : constant C.char_array := "ORDER" & C.char'Val (0);
		begin
			rc :=
				C.libxml.xmlwriter.xmlTextWriterStartElement (
					writer,
					To_xmlChar_const_ptr (Name (Name'First)'Unchecked_Access));
		end;
		if rc < 0 then
			raise Program_Error
				with "testXmlwriterTree: Error at xmlTextWriterStartElement";
		end if;
		-- Add an attribute with name "version" and value "1.0" to ORDER.
		declare
			Name : constant C.char_array := "version" & C.char'Val (0);
			Format : constant C.char_array := "1.0" & C.char'Val (0);
		begin
			rc :=
				C.libxml.xmlwriter.xmlTextWriterWriteAttribute (
					writer,
					To_xmlChar_const_ptr (Name (Name'First)'Unchecked_Access),
					To_xmlChar_const_ptr (Format (Format'First)'Unchecked_Access));
		end;
		if rc < 0 then
			raise Program_Error
				with "testXmlwriterTree: Error at xmlTextWriterWriteAttribute";
		end if;
		-- Add an attribute with name "xml:lang" and value "de" to ORDER.
		declare
			Name : constant C.char_array := "xml:lang" & C.char'Val (0);
			Format : constant C.char_array := "de" & C.char'Val (0);
		begin
			rc :=
				C.libxml.xmlwriter.xmlTextWriterWriteAttribute (
					writer,
					To_xmlChar_const_ptr (Name (Name'First)'Unchecked_Access),
					To_xmlChar_const_ptr (Format (Format'First)'Unchecked_Access));
		end;
		if rc < 0 then
			raise Program_Error
				with "testXmlwriterTree: Error at xmlTextWriterWriteAttribute";
		end if;
		-- Write a comment as child of ORDER
		tmp :=
			ConvertInput (
				"<" & C.char'Val (16#E4#) & C.char'Val (16#F6#) & C.char'Val (16#FC#) & ">"
					& C.char'Val (0),
				MY_ENCODING);
		declare
			Format : constant C.char_array :=
				"This is another comment with special chars: " & C.char'Val (0);
			Value : C.char_array (0 .. 255);
			Dummy_char_ptr : C.char_ptr;
		begin
			Dummy_char_ptr :=
				C.string.strcpy (
					Value (Value'First)'Access,
					Format (Format'First)'Access);
			Dummy_char_ptr :=
				C.string.strcat (
					Value (Value'First)'Access,
					To_char_const_ptr (tmp));
			rc :=
				C.libxml.xmlwriter.xmlTextWriterWriteComment (
					writer,
					To_xmlChar_const_ptr (Value (Value'First)'Unchecked_Access));
		end;
		if rc < 0 then
			raise Program_Error
				with "testXmlwriterTree: Error at xmlTextWriterWriteFormatComment";
		end if;
		if tmp /= null then
			C.libxml.globals.xmlFree (To_void_ptr (tmp));
		end if;
		-- Start an element named "HEADER" as child of ORDER.
		declare
			Name : constant C.char_array := "HEADER" & C.char'Val (0);
		begin
			rc :=
				C.libxml.xmlwriter.xmlTextWriterStartElement (
					writer,
					To_xmlChar_const_ptr (Name (Name'First)'Unchecked_Access));
		end;
		if rc < 0 then
			raise Program_Error
				with "testXmlwriterTree: Error at xmlTextWriterStartElement";
		end if;
		-- Write an element named "X_ORDER_ID" as child of HEADER.
		declare
			Name : constant C.char_array := "X_ORDER_ID" & C.char'Val (0);
			Format : constant C.char_array := "0000053535" & C.char'Val (0);
		begin
			rc :=
				C.libxml.xmlwriter.xmlTextWriterWriteElement (
					writer,
					To_xmlChar_const_ptr (Name (Name'First)'Unchecked_Access),
					To_xmlChar_const_ptr (Format (Format'First)'Unchecked_Access));
		end;
		if rc < 0 then
			raise Program_Error
				with "testXmlwriterTree: Error at xmlTextWriterWriteFormatElement";
		end if;
		-- Write an element named "CUSTOMER_ID" as child of HEADER.
		declare
			Name : constant C.char_array := "CUSTOMER_ID" & C.char'Val (0);
			Format : constant C.char_array := "1010" & C.char'Val (0);
		begin
			rc :=
				C.libxml.xmlwriter.xmlTextWriterWriteElement (
					writer,
					To_xmlChar_const_ptr (Name (Name'First)'Unchecked_Access),
					To_xmlChar_const_ptr (Format (Format'First)'Unchecked_Access));
		end;
		if rc < 0 then
			raise Program_Error
				with "testXmlwriterTree: Error at xmlTextWriterWriteFormatElement";
		end if;
		-- Write an element named "NAME_1" as child of HEADER.
		tmp :=
			ConvertInput (
				"M" & C.char'Val (16#FC#) & "ller" & C.char'Val (0),
				MY_ENCODING);
		declare
			Name : constant C.char_array := "NAME_1" & C.char'Val (0);
		begin
			rc :=
				C.libxml.xmlwriter.xmlTextWriterWriteElement (
					writer,
					To_xmlChar_const_ptr (Name (Name'First)'Unchecked_Access),
					tmp);
		end;
		if rc < 0 then
			raise Program_Error
				with "testXmlwriterTree: Error at xmlTextWriterWriteElement";
		end if;
		if tmp /= null then
			C.libxml.globals.xmlFree (To_void_ptr (tmp));
		end if;
		-- Write an element named "NAME_2" as child of HEADER.
		tmp :=
			ConvertInput (
				"J" & C.char'Val (16#F6#) & "rg" & C.char'Val (0),
				MY_ENCODING);
		declare
			Name : constant C.char_array := "NAME_2" & C.char'Val (0);
		begin
			rc :=
				C.libxml.xmlwriter.xmlTextWriterWriteElement (
					writer,
					To_xmlChar_const_ptr (Name (Name'First)'Unchecked_Access),
					tmp);
		end;
		if rc < 0 then
			raise Program_Error
				with "testXmlwriterTree: Error at xmlTextWriterWriteElement";
		end if;
		if tmp /= null then
			C.libxml.globals.xmlFree (To_void_ptr (tmp));
		end if;
		-- Close the element named HEADER.
		rc := C.libxml.xmlwriter.xmlTextWriterEndElement (writer);
		if rc < 0 then
			raise Program_Error with "testXmlwriterTree: Error at xmlTextWriterEndElement";
		end if;
		-- Start an element named "ENTRIES" as child of ORDER.
		declare
			Name : constant C.char_array := "ENTRIES" & C.char'Val (0);
		begin
			rc :=
				C.libxml.xmlwriter.xmlTextWriterStartElement (
					writer,
					To_xmlChar_const_ptr (Name (Name'First)'Unchecked_Access));
		end;
		if rc < 0 then
			raise Program_Error
				with "testXmlwriterTree: Error at xmlTextWriterStartElement";
		end if;
		-- Start an element named "ENTRY" as child of ENTRIES.
		declare
			Name : constant C.char_array := "ENTRY" & C.char'Val (0);
		begin
			rc :=
				C.libxml.xmlwriter.xmlTextWriterStartElement (
					writer,
					To_xmlChar_const_ptr (Name (Name'First)'Unchecked_Access));
		end;
		if rc < 0 then
			raise Program_Error
				with "testXmlwriterTree: Error at xmlTextWriterStartElement";
		end if;
		-- Write an element named "ARTICLE" as child of ENTRY.
		declare
			Name : constant C.char_array := "ARTICLE" & C.char'Val (0);
			Format : constant C.char_array := "<Test>" & C.char'Val (0);
		begin
			rc :=
				C.libxml.xmlwriter.xmlTextWriterWriteElement (
					writer,
					To_xmlChar_const_ptr (Name (Name'First)'Unchecked_Access),
					To_xmlChar_const_ptr (Format (Format'First)'Unchecked_Access));
		end;
		if rc < 0 then
			raise Program_Error
				with "testXmlwriterTree: Error at xmlTextWriterWriteElement";
		end if;
		-- Write an element named "ENTRY_NO" as child of ENTRY.
		declare
			Name : constant C.char_array := "ENTRY_NO" & C.char'Val (0);
			Format : constant C.char_array := "10" & C.char'Val (0);
		begin
			rc :=
				C.libxml.xmlwriter.xmlTextWriterWriteElement (
					writer,
					To_xmlChar_const_ptr (Name (Name'First)'Unchecked_Access),
					To_xmlChar_const_ptr (Format (Format'First)'Unchecked_Access));
		end;
		if rc < 0 then
			raise Program_Error
				with "testXmlwriterTree: Error at xmlTextWriterWriteFormatElement";
		end if;
		-- Close the element named ENTRY.
		rc := C.libxml.xmlwriter.xmlTextWriterEndElement (writer);
		if rc < 0 then
			raise Program_Error with "testXmlwriterTree: Error at xmlTextWriterEndElement";
		end if;
		-- Start an element named "ENTRY" as child of ENTRIES.
		declare
			Name : constant C.char_array := "ENTRY" & C.char'Val (0);
		begin
			rc :=
				C.libxml.xmlwriter.xmlTextWriterStartElement (
					writer,
					To_xmlChar_const_ptr (Name (Name'First)'Unchecked_Access));
		end;
		if rc < 0 then
			raise Program_Error
				with "testXmlwriterTree: Error at xmlTextWriterStartElement";
		end if;
		-- Write an element named "ARTICLE" as child of ENTRY.
		declare
			Name : constant C.char_array := "ARTICLE" & C.char'Val (0);
			Format : constant C.char_array := "<Test 2>" & C.char'Val (0);
		begin
			rc :=
				C.libxml.xmlwriter.xmlTextWriterWriteElement (
					writer,
					To_xmlChar_const_ptr (Name (Name'First)'Unchecked_Access),
					To_xmlChar_const_ptr (Format (Format'First)'Unchecked_Access));
		end;
		if rc < 0 then
			raise Program_Error
				with "testXmlwriterTree: Error at xmlTextWriterWriteElement";
		end if;
		-- Write an element named "ENTRY_NO" as child of ENTRY.
		declare
			Name : constant C.char_array := "ENTRY_NO" & C.char'Val (0);
			Format : constant C.char_array := "20" & C.char'Val (0);
		begin
			rc :=
				C.libxml.xmlwriter.xmlTextWriterWriteElement (
					writer,
					To_xmlChar_const_ptr (Name (Name'First)'Unchecked_Access),
					To_xmlChar_const_ptr (Format (Format'First)'Unchecked_Access));
		end;
		if rc < 0 then
			raise Program_Error
				with "testXmlwriterTree: Error at xmlTextWriterWriteFormatElement";
		end if;
		-- Close the element named ENTRY.
		rc := C.libxml.xmlwriter.xmlTextWriterEndElement (writer);
		if rc < 0 then
			raise Program_Error with "testXmlwriterTree: Error at xmlTextWriterEndElement";
		end if;
		-- Close the element named ENTRIES.
		rc := C.libxml.xmlwriter.xmlTextWriterEndElement (writer);
		if rc < 0 then
			raise Program_Error with "testXmlwriterTree: Error at xmlTextWriterEndElement";
		end if;
		-- Start an element named "FOOTER" as child of ORDER.
		declare
			Name : constant C.char_array := "FOOTER" & C.char'Val (0);
		begin
			rc :=
				C.libxml.xmlwriter.xmlTextWriterStartElement (
					writer,
					To_xmlChar_const_ptr (Name (Name'First)'Unchecked_Access));
		end;
		if rc < 0 then
			raise Program_Error
				with "testXmlwriterTree: Error at xmlTextWriterStartElement";
		end if;
		-- Write an element named "TEXT" as child of FOOTER.
		declare
			Name : constant C.char_array := "TEXT" & C.char'Val (0);
			Format : constant C.char_array := "This is a text." & C.char'Val (0);
		begin
			rc :=
				C.libxml.xmlwriter.xmlTextWriterWriteElement (
					writer,
					To_xmlChar_const_ptr (Name (Name'First)'Unchecked_Access),
					To_xmlChar_const_ptr (Format (Format'First)'Unchecked_Access));
		end;
		if rc < 0 then
			raise Program_Error
				with "testXmlwriterTree: Error at xmlTextWriterWriteElement";
		end if;
		-- Close the element named FOOTER.
		rc := C.libxml.xmlwriter.xmlTextWriterEndElement (writer);
		if rc < 0 then
			raise Program_Error with "testXmlwriterTree: Error at xmlTextWriterEndElement";
		end if;
	end Write_Sample_XML;
	-- testXmlwriterFilename:
	-- @uri: the output URI
	--
	-- test the xmlWriter interface when writing to a new file
	procedure testXmlwriterFilename (uri : in C.char_array) is
		pragma Assert (uri (uri'Last) = C.char'Val (0));
		rc : C.signed_int;
		writer : C.libxml.xmlwriter.xmlTextWriterPtr;
	begin
		-- Create a new XmlWriter for uri, with no compression.
		writer :=
			C.libxml.xmlwriter.xmlNewTextWriterFilename (uri (uri'First)'Access, 0);
		if writer = null then
			raise Program_Error
				with "testXmlwriterFilename: Error creating the xml writer";
		end if;
		-- Start the document with the xml default for the version,
		-- encoding ISO 8859-1 and the default for the standalone
		-- declaration.
		rc :=
			C.libxml.xmlwriter.xmlTextWriterStartDocument (
				writer,
				null,
				MY_ENCODING (MY_ENCODING'First)'Access,
				null);
		if rc < 0 then
			raise Program_Error
				with "testXmlwriterTree: Error at xmlTextWriterStartDocument";
		end if;
		-- Start an element named "EXAMPLE". Since thist is the first
		-- element, this will be the root element of the document.
		declare
			Name : constant C.char_array := "EXAMPLE" & C.char'Val (0);
		begin
			rc :=
				C.libxml.xmlwriter.xmlTextWriterStartElement (
					writer,
					To_xmlChar_const_ptr (Name (Name'First)'Unchecked_Access));
		end;
		if rc < 0 then
			raise Program_Error
				with "testXmlwriterTree: Error at xmlTextWriterStartElement";
		end if;
		Writing : begin
			Write_Sample_XML (writer);
		end Writing;
		-- Here we could close the elements ORDER and EXAMPLE using the
		-- function xmlTextWriterEndElement, but since we do not want to
		-- write any other elements, we simply call xmlTextWriterEndDocument,
		-- which will do all the work.
		rc := C.libxml.xmlwriter.xmlTextWriterEndDocument (writer);
		if rc < 0 then
			raise Program_Error
				with "testXmlwriterTree: Error at xmlTextWriterEndDocument";
		end if;
		C.libxml.xmlwriter.xmlFreeTextWriter (writer);
	end testXmlwriterFilename;
	-- testXmlwriterMemory:
	-- @file: the output file
	--
	-- test the xmlWriter interface when writing to memory
	procedure testXmlwriterMemory (file : in C.char_array) is
		pragma Assert (file (file'Last) = C.char'Val (0));
		rc : C.signed_int;
		writer : C.libxml.xmlwriter.xmlTextWriterPtr;
		buf : C.libxml.tree.xmlBufferPtr;
		fp : access C.stdio.FILE;
		Dummy_signed_int : C.signed_int;
	begin
		-- Create a new XML buffer, to which the XML document will be written
		buf := C.libxml.tree.xmlBufferCreate;
		if buf = null then
			raise Program_Error with "testXmlwriterMemory: Error creating the xml buffer";
		end if;
		-- Create a new XmlWriter for memory, with no compression.
		-- Remark: there is no compression for this kind of xmlTextWriter
		writer := C.libxml.xmlwriter.xmlNewTextWriterMemory (buf, 0);
		if writer = null then
			raise Program_Error with "testXmlwriterMemory: Error creating the xml writer";
		end if;
		-- Start the document with the xml default for the version,
		-- encoding ISO 8859-1 and the default for the standalone
		-- declaration.
		rc :=
			C.libxml.xmlwriter.xmlTextWriterStartDocument (
				writer,
				null,
				MY_ENCODING (MY_ENCODING'First)'Access,
				null);
		if rc < 0 then
			raise Program_Error
				with "testXmlwriterTree: Error at xmlTextWriterStartDocument";
		end if;
		-- Start an element named "EXAMPLE". Since thist is the first
		-- element, this will be the root element of the document.
		declare
			Name : constant C.char_array := "EXAMPLE" & C.char'Val (0);
		begin
			rc :=
				C.libxml.xmlwriter.xmlTextWriterStartElement (
					writer,
					To_xmlChar_const_ptr (Name (Name'First)'Unchecked_Access));
		end;
		if rc < 0 then
			raise Program_Error
				with "testXmlwriterTree: Error at xmlTextWriterStartElement";
		end if;
		Writing : begin
			Write_Sample_XML (writer);
		end Writing;
		-- Here we could close the elements ORDER and EXAMPLE using the
		-- function xmlTextWriterEndElement, but since we do not want to
		-- write any other elements, we simply call xmlTextWriterEndDocument,
		-- which will do all the work.
		rc := C.libxml.xmlwriter.xmlTextWriterEndDocument (writer);
		if rc < 0 then
			raise Program_Error
				with "testXmlwriterTree: Error at xmlTextWriterEndDocument";
		end if;
		C.libxml.xmlwriter.xmlFreeTextWriter (writer);
		declare
			Mode : constant C.char_array := "w" & C.char'Val (0);
		begin
			fp := C.stdio.fopen (file (file'First)'Access, Mode (Mode'First)'Access);
		end;
		if fp = null then
			raise Program_Error with "testXmlwriterMemory: Error at fopen";
		end if;
		Dummy_signed_int := C.stdio.fputs (To_char_const_ptr (buf.content), fp);
		Dummy_signed_int := C.stdio.fclose (fp);
		C.libxml.tree.xmlBufferFree (buf);
	end testXmlwriterMemory;
	-- testXmlwriterDoc:
	-- @file: the output file
	--
	-- test the xmlWriter interface when creating a new document
	procedure testXmlwriterDoc (file : in C.char_array) is
		pragma Assert (file (file'Last) = C.char'Val (0));
		rc : C.signed_int;
		writer : C.libxml.xmlwriter.xmlTextWriterPtr;
		doc : aliased C.libxml.tree.xmlDocPtr;
		Dummy_signed_int : C.signed_int;
	begin
		-- Create a new XmlWriter for DOM, with no compression.
		writer := C.libxml.xmlwriter.xmlNewTextWriterDoc (doc'Access, 0);
		if writer = null then
			raise Program_Error with "testXmlwriterDoc: Error creating the xml writer";
		end if;
		-- Start the document with the xml default for the version,
		-- encoding ISO 8859-1 and the default for the standalone
		-- declaration.
		rc :=
			C.libxml.xmlwriter.xmlTextWriterStartDocument (
				writer,
				null,
				MY_ENCODING (MY_ENCODING'First)'Access,
				null);
		if rc < 0 then
			raise Program_Error
				with "testXmlwriterTree: Error at xmlTextWriterStartDocument";
		end if;
		-- Start an element named "EXAMPLE". Since thist is the first
		-- element, this will be the root element of the document.
		declare
			Name : constant C.char_array := "EXAMPLE" & C.char'Val (0);
		begin
			rc :=
				C.libxml.xmlwriter.xmlTextWriterStartElement (
					writer,
					To_xmlChar_const_ptr (Name (Name'First)'Unchecked_Access));
		end;
		if rc < 0 then
			raise Program_Error
				with "testXmlwriterTree: Error at xmlTextWriterStartElement";
		end if;
		Writing : begin
			Write_Sample_XML (writer);
		end Writing;
		-- Here we could close the elements ORDER and EXAMPLE using the
		-- function xmlTextWriterEndElement, but since we do not want to
		-- write any other elements, we simply call xmlTextWriterEndDocument,
		-- which will do all the work.
		rc := C.libxml.xmlwriter.xmlTextWriterEndDocument (writer);
		if rc < 0 then
			raise Program_Error
				with "testXmlwriterTree: Error at xmlTextWriterEndDocument";
		end if;
		C.libxml.xmlwriter.xmlFreeTextWriter (writer);
		Dummy_signed_int :=
			C.libxml.tree.xmlSaveFileEnc (
				file (file'First)'Access,
				doc,
				MY_ENCODING (MY_ENCODING'First)'Access);
		C.libxml.tree.xmlFreeDoc (doc);
	end testXmlwriterDoc;
	-- testXmlwriterTree:
	-- @file: the output file
	--
	-- test the xmlWriter interface when writing to a subtree
	procedure testXmlwriterTree (file : in C.char_array) is
		pragma Assert (file (file'Last) = C.char'Val (0));
		rc : C.signed_int;
		writer : C.libxml.xmlwriter.xmlTextWriterPtr;
		doc : C.libxml.tree.xmlDocPtr;
		node : C.libxml.tree.xmlNodePtr;
		Dummy_xmlNodePtr : C.libxml.tree.xmlNodePtr;
		Dummy_signed_int : C.signed_int;
	begin
		-- Create a new XML DOM tree, to which the XML document will be written
		doc :=
			C.libxml.tree.xmlNewDoc (
				To_xmlChar_const_ptr (
					C.libxml.parser.XML_DEFAULT_VERSION (
						C.libxml.parser.XML_DEFAULT_VERSION'First)'Access));
		if doc = null then
			raise Program_Error
				with "testXmlwriterTree: Error creating the xml document tree";
		end if;
		-- Create a new XML node, to which the XML document will be appended
		declare
			Name : constant C.char_array := "EXAMPLE" & C.char'Val (0);
		begin
			node :=
				C.libxml.tree.xmlNewDocNode (
					doc,
					null,
					To_xmlChar_const_ptr (Name (Name'First)'Unchecked_Access),
					null);
		end;
		if node = null then
			raise Program_Error with "testXmlwriterTree: Error creating the xml node";
		end if;
		-- Make ELEMENT the root node of the tree
		Dummy_xmlNodePtr := C.libxml.tree.xmlDocSetRootElement (doc, node);
		-- Create a new XmlWriter for DOM tree, with no compression.
		writer := C.libxml.xmlwriter.xmlNewTextWriterTree (doc, node, 0);
		if writer = null then
			raise Program_Error with "testXmlwriterTree: Error creating the xml writer";
		end if;
		-- Start the document with the xml default for the version,
		-- encoding ISO 8859-1 and the default for the standalone
		-- declaration.
		rc :=
			C.libxml.xmlwriter.xmlTextWriterStartDocument (
				writer,
				null,
				MY_ENCODING (MY_ENCODING'First)'Access,
				null);
		if rc < 0 then
			raise Program_Error
				with "testXmlwriterTree: Error at xmlTextWriterStartDocument";
		end if;
		Writing : begin
			Write_Sample_XML (writer);
		end Writing;
		-- Here we could close the elements ORDER and EXAMPLE using the
		-- function xmlTextWriterEndElement, but since we do not want to
		-- write any other elements, we simply call xmlTextWriterEndDocument,
		-- which will do all the work.
		rc := C.libxml.xmlwriter.xmlTextWriterEndDocument (writer);
		if rc < 0 then
			raise Program_Error
				with "testXmlwriterTree: Error at xmlTextWriterEndDocument";
		end if;
		C.libxml.xmlwriter.xmlFreeTextWriter (writer);
		Dummy_signed_int :=
			C.libxml.tree.xmlSaveFileEnc (
				file (file'First)'Access,
				doc,
				MY_ENCODING (MY_ENCODING'First)'Access);
		C.libxml.tree.xmlFreeDoc (doc);
	end testXmlwriterTree;
begin
	-- this initialize the library and check potential ABI mismatches
	-- between the version it was compiled for and the actual shared
	-- library used.
	C.libxml.xmlversion.xmlCheckVersion (C.libxml.xmlversion.LIBXML_VERSION);
	-- first, the file version
	testXmlwriterFilename ("writer1.res" & C.char'Val (0));
	-- next, the memory version
	testXmlwriterMemory ("writer2.res" & C.char'Val (0));
	-- next, the DOM version
	testXmlwriterDoc ("writer3.res" & C.char'Val (0));
	-- next, the tree version
	testXmlwriterTree ("writer4.res" & C.char'Val (0));
	-- Cleanup function for the XML library.
	C.libxml.parser.xmlCleanupParser;
	-- this is to debug memory for regression tests
	C.libxml.xmlmemory.xmlMemoryDump;
	-- finish
	Ada.Text_IO.Put_Line (Ada.Text_IO.Standard_Error.all, "ok");
end test_writer;
