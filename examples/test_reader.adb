-- reader1.c
--
-- section: xmlReader
-- synopsis: Parse an XML file with an xmlReader
-- purpose: Demonstrate the use of xmlReaderForFile() to parse an XML file
--          and dump the informations about the nodes found in the process.
--          (Note that the XMLReader functions require libxml2 version later
--          than 2.6.)
-- usage: reader1 <filename>
-- test: reader1 test2.xml > reader1.tmp ;
--       diff reader1.tmp reader1.res ;
--       rm reader1.tmp
-- author: Daniel Veillard
-- copy: see Copyright for the status of this software.
--
-- reader2.c
--
-- section: xmlReader
-- synopsis: Parse and validate an XML file with an xmlReader
-- purpose: Demonstrate the use of xmlReaderForFile() to parse an XML file
--          validating the content in the process and activating options
--          like entities substitution, and DTD attributes defaulting.
--          (Note that the XMLReader functions require libxml2 version later
--          than 2.6.)
-- usage: reader2 <valid_xml_filename>
-- test: reader2 test2.xml > reader1.tmp ;
--       diff reader1.tmp reader1.res ;
--       rm reader1.tmp
-- author: Daniel Veillard
-- copy: see Copyright for the status of this software.
--
-- reader3.c
--
-- section: xmlReader
-- synopsis: Show how to extract subdocuments with xmlReader
-- purpose: Demonstrate the use of xmlTextReaderPreservePattern() 
--          to parse an XML file with the xmlReader while collecting
--          only some subparts of the document.
--          (Note that the XMLReader functions require libxml2 version later
--          than 2.6.)
-- usage: reader3
-- test: reader3 > reader3.tmp ;
--       diff reader3.tmp reader3.res ;
--       rm reader3.tmp
-- author: Daniel Veillard
-- copy: see Copyright for the status of this software.
--
-- reader4.c
--
-- section: xmlReader
-- synopsis: Parse multiple XML files reusing an xmlReader
-- purpose: Demonstrate the use of xmlReaderForFile() and
-- xmlReaderNewFile to parse XML files while reusing the reader object
-- and parser context.  (Note that the XMLReader functions require
-- libxml2 version later than 2.6.)
-- usage: reader4 <filename> [ filename ... ]
-- test: reader4 test1.xml test2.xml test3.xml > reader4.tmp ;
--       diff reader4.tmp reader4.res ;
--       rm reader4.tmp
-- author: Graham Bennett
-- copy: see Copyright for the status of this software.
--
-- Ada version by yt
--
with Ada.Unchecked_Conversion;
with C.stdio;
with C.libxml.xmlreader;
with C.libxml.parser;
with C.libxml.xmlmemory;
with C.libxml.xmlstring;
with C.libxml.tree;
with C.libxml.xmlversion;
--pragma Compile_Time_Error (
--	not C.libxml.xmlversion.LIBXML_READER_ENABLED
--	or else not C.libxml.xmlversion.LIBXML_PATTERN_ENABLED
--	or else not C.libxml.xmlversion.LIBXML_OUTPUT_ENABLED,
--	"Reader, Pattern or output support not compiled in");
procedure test_reader is
	pragma Linker_Options ("-lxml2");
	use type C.char_array;
	use type C.signed_int;
	use type C.size_t;
	use type C.unsigned_int;
	use type C.libxml.tree.xmlDocPtr;
	use type C.libxml.xmlreader.xmlTextReaderPtr;
	use type C.libxml.xmlstring.xmlChar_const_ptr;
	function To_char_const_ptr is new Ada.Unchecked_Conversion (
		C.libxml.xmlstring.xmlChar_const_ptr,
		C.char_const_ptr);
	function To_xmlChar_const_ptr is new Ada.Unchecked_Conversion (
		C.char_const_ptr,
		C.libxml.xmlstring.xmlChar_const_ptr);
	Write_Mode : constant C.char_array := "w" & C.char'Val (0);
	stdout : C.stdio.FILE_ptr;
	procedure fwrite (
		ptr : not null access constant C.char;
		size : in C.size_t;
		nitems : in C.size_t;
		stream : in C.stdio.FILE_ptr)
	is
		Dummy_size_t : C.size_t;
		pragma Unreferenced (Dummy_size_t);
	begin
		Dummy_size_t := C.stdio.fwrite (ptr.all'Address, size, nitems, stream);
	end fwrite;
	procedure fputs (
		s : not null access constant C.char;
		stream : in C.stdio.FILE_ptr)
	is
		Dummy_signed_int : C.signed_int;
		pragma Unreferenced (Dummy_signed_int);
	begin
		Dummy_signed_int := C.stdio.fputs (s, stream);
	end fputs;
	procedure fputs (
		s : in C.char_array;
		stream : in C.stdio.FILE_ptr) is
	begin
		fputs (s (s'First)'Access, stream);
	end fputs;
	procedure fputd (
		d : in C.signed_int;
		stream : in C.stdio.FILE_ptr)
	is
		s : C.char_array (0 .. 10);
		i : C.size_t := s'Last;
		r : C.signed_int := d;
	begin
		if r < 0 then
			fputs (' ' & C.char'Val (0), stream);
			r := -r;
		end if;
		s (i) := C.char'Val (0);
		loop
			i := i - 1;
			s (i) := C.char'Val (C.char'Pos ('0') + r rem 10);
			r := r / 10;
			exit when r = 0;
		end loop;
		fputs (s (i)'Access, stream);
	end fputd;
	procedure processNode_For_1_and_2 (
		reader : C.libxml.xmlreader.xmlTextReaderPtr)
	is
		name, value : C.libxml.xmlstring.xmlChar_const_ptr;
		S1 : constant C.char_array := "--" & C.char'Val (0);
	begin
		name := C.libxml.xmlreader.xmlTextReaderConstName (reader);
		if name = null then
			name := To_xmlChar_const_ptr (S1 (S1'First)'Unchecked_Access);
		end if;
		value := C.libxml.xmlreader.xmlTextReaderConstValue (reader);
		fputd (C.libxml.xmlreader.xmlTextReaderDepth (reader), stdout);
		fputs (' ' & C.char'Val (0), stdout);
		fputd (C.libxml.xmlreader.xmlTextReaderNodeType (reader), stdout);
		fputs (' ' & C.char'Val (0), stdout);
		fputs (To_char_const_ptr (name), stdout);
		fputs (' ' & C.char'Val (0), stdout);
		fputd (C.libxml.xmlreader.xmlTextReaderIsEmptyElement (reader), stdout);
		fputs (' ' & C.char'Val (0), stdout);
		fputd (C.libxml.xmlreader.xmlTextReaderHasValue (reader), stdout);
		if value = null then
			fputs (C.char'Val (10) & C.char'Val (0), stdout);
		else
			if C.libxml.xmlstring.xmlStrlen (value) > 40 then
				fputs (' ' & C.char'Val (0), stdout);
				fwrite (To_char_const_ptr (value), 1, 40, stdout);
				fputs ("..." & C.char'Val (10) & C.char'Val (0), stdout);
			else
				fputs (' ' & C.char'Val (0), stdout);
				fputs (To_char_const_ptr (value), stdout);
				fputs (C.char'Val (10) & C.char'Val (0), stdout);
			end if;
		end if;
	end processNode_For_1_and_2;
	-- do as reader1.c
	procedure reader1 (argv1, output : in C.char_array) is
		-- processNode:
		-- @reader: the xmlReader
		--
		-- Dump information about the current node
		procedure processNode (reader : C.libxml.xmlreader.xmlTextReaderPtr)
			renames processNode_For_1_and_2;
		-- streamFile:
		-- @filename: the file name to parse
		--
		-- Parse and print information about an XML file.
		procedure streamFile (filename : access constant C.char) is
			reader : C.libxml.xmlreader.xmlTextReaderPtr;
			ret : C.signed_int;
		begin
			reader := C.libxml.xmlreader.xmlReaderForFile (filename, null, 0);
			if reader /= null then
				ret := C.libxml.xmlreader.xmlTextReaderRead (reader);
				while ret = 1 loop
					processNode (reader);
					ret := C.libxml.xmlreader.xmlTextReaderRead (reader);
				end loop;
				C.libxml.xmlreader.xmlFreeTextReader (reader);
				if ret /= 0 then
					fputs (filename, C.stdio.stderr);
					fputs (" : failed to parse" & C.char'Val (10) & C.char'Val (0),
						C.stdio.stderr);
				end if;
			else
				fputs ("Unable to open " & C.char'Val (0), C.stdio.stderr);
				fputs (filename, C.stdio.stderr);
				fputs (C.char'Val (10) & C.char'Val (0), C.stdio.stderr);
			end if;
		end streamFile;
		Dummy_signed_int : C.signed_int;
		pragma Unreferenced (Dummy_signed_int);
	begin
		stdout := C.stdio.fopen (
			output (output'First)'Access,
			Write_Mode (Write_Mode'First)'Access);
		streamFile (argv1 (argv1'First)'Access);
		Dummy_signed_int := C.stdio.fclose (stdout);
	end reader1;
	-- do as reader2.c
	procedure reader2 (argv1, output : in C.char_array) is
		-- processNode:
		-- @reader: the xmlReader
		--
		-- Dump information about the current node
		procedure processNode (reader : C.libxml.xmlreader.xmlTextReaderPtr)
			renames processNode_For_1_and_2;
		-- streamFile:
		-- @filename: the file name to parse
		--
		-- Parse, validate and print information about an XML file.
		procedure streamFile (filename : access constant C.char) is
			reader : C.libxml.xmlreader.xmlTextReaderPtr;
			ret : C.signed_int;
		begin
			-- Pass some special parsing options to activate DTD attribute defaulting,
			-- entities substitution and DTD validation
			reader := C.libxml.xmlreader.xmlReaderForFile (
				filename,
				null,
				C.signed_int (C.unsigned_int'(
					C.libxml.parser.xmlParserOption'Enum_Rep (
						C.libxml.parser.XML_PARSE_DTDATTR) -- default DTD attributes
					or C.libxml.parser.xmlParserOption'Enum_Rep (
						C.libxml.parser.XML_PARSE_NOENT) -- substitute entities
					or C.libxml.parser.xmlParserOption'Enum_Rep (
						C.libxml.parser.XML_PARSE_DTDVALID)))); -- validate with the DTD
			if reader /= null then
				ret := C.libxml.xmlreader.xmlTextReaderRead (reader);
				while ret = 1 loop
					processNode (reader);
					ret := C.libxml.xmlreader.xmlTextReaderRead (reader);
				end loop;
				-- Once the document has been fully parsed check the validation results
				if C.libxml.xmlreader.xmlTextReaderIsValid (reader) /= 1 then
					fputs ("Document " & C.char'Val (0), C.stdio.stderr);
					fputs (filename, C.stdio.stderr);
					fputs (" does not validate" & C.char'Val (10) & C.char'Val (0),
						C.stdio.stderr);
				end if;
				C.libxml.xmlreader.xmlFreeTextReader (reader);
				if ret /= 0 then
					fputs (filename, C.stdio.stderr);
					fputs (" : failed to parse" & C.char'Val (10) & C.char'Val (0),
						C.stdio.stderr);
				end if;
			else
				fputs ("Unable to open " & C.char'Val (0), C.stdio.stderr);
				fputs (filename, C.stdio.stderr);
				fputs (C.char'Val (10) & C.char'Val (0), C.stdio.stderr);
			end if;
		end streamFile;
		Dummy_signed_int : C.signed_int;
		pragma Unreferenced (Dummy_signed_int);
	begin
		stdout := C.stdio.fopen (
			output (output'First)'Access,
			Write_Mode (Write_Mode'First)'Access);
		streamFile (argv1 (argv1'First)'Access);
		Dummy_signed_int := C.stdio.fclose (stdout);
	end reader2;
	-- do as reader3.c
	procedure reader3 (output : in C.char_array) is
		-- streamFile:
		-- @filename: the file name to parse
		--
		-- Parse and print information about an XML file.
		--
		-- Returns the resulting doc with just the elements preserved.
		function extractFile (
			filename : access constant C.char;
			pattern : C.libxml.xmlstring.xmlChar_const_ptr)
			return C.libxml.tree.xmlDocPtr
		is
			doc : C.libxml.tree.xmlDocPtr;
			reader : C.libxml.xmlreader.xmlTextReaderPtr;
			ret : C.signed_int;
		begin
			-- build an xmlReader for that file
			reader := C.libxml.xmlreader.xmlReaderForFile (filename, null, 0);
			if reader /= null then
				-- add the pattern to preserve
				if C.libxml.xmlreader.xmlTextReaderPreservePattern (
					reader,
					pattern,
					null) < 0
				then
					fputs (filename, C.stdio.stderr);
					fputs (" : failed add preserve pattern " & C.char'Val (0),
						C.stdio.stderr);
					fputs (To_char_const_ptr (pattern), C.stdio.stderr);
					fputs (C.char'Val (10) & C.char'Val (0), C.stdio.stderr);
				end if;
				-- Parse and traverse the tree, collecting the nodes in the process
				ret := C.libxml.xmlreader.xmlTextReaderRead (reader);
				while ret = 1 loop
					ret := C.libxml.xmlreader.xmlTextReaderRead (reader);
				end loop;
				if ret /= 0 then
					fputs (filename, C.stdio.stderr);
					fputs (" : failed to parse" & C.char'Val (10) & C.char'Val (0),
						C.stdio.stderr);
					C.libxml.xmlreader.xmlFreeTextReader (reader);
					return null;
				end if;
				-- get the resulting nodes
				doc := C.libxml.xmlreader.xmlTextReaderCurrentDoc (reader);
				-- Free up the reader
				C.libxml.xmlreader.xmlFreeTextReader (reader);
			else
				fputs ("Unable to open " & C.char'Val (0), C.stdio.stderr);
				fputs (filename, C.stdio.stderr);
				fputs (C.char'Val (10) & C.char'Val (0), C.stdio.stderr);
				return null;
			end if;
			return doc;
		end extractFile;
		filename : constant C.char_array := "test3.xml" & C.char'Val (0);
		pattern : constant C.char_array := "preserved" & C.char'Val (0);
		doc : C.libxml.tree.xmlDocPtr;
		Dummy_signed_int : C.signed_int;
		pragma Unreferenced (Dummy_signed_int);
	begin
		stdout := C.stdio.fopen (
			output (output'First)'Access,
			Write_Mode (Write_Mode'First)'Access);
		doc := extractFile (
			filename (filename'First)'Access,
			To_xmlChar_const_ptr (pattern (pattern'First)'Unchecked_Access));
		if doc /= null then
			-- ouptut the result.
			Dummy_signed_int := C.libxml.tree.xmlDocDump (stdout, doc);
			-- don't forget to free up the doc
			C.libxml.tree.xmlFreeDoc (doc);
		end if;
		Dummy_signed_int := C.stdio.fclose (stdout);
	end reader3;
	-- do as reader4.c
	procedure reader4 (argv1, argv2, argv3, output : in C.char_array) is
		procedure processDoc (readerPtr : C.libxml.xmlreader.xmlTextReaderPtr) is
			ret : C.signed_int;
			docPtr : C.libxml.tree.xmlDocPtr;
			URL : C.libxml.xmlstring.xmlChar_const_ptr;
		begin
			ret := C.libxml.xmlreader.xmlTextReaderRead (readerPtr);
			while ret = 1 loop
				ret := C.libxml.xmlreader.xmlTextReaderRead (readerPtr);
			end loop;
			-- One can obtain the document pointer to get insteresting
			-- information about the document like the URL, but one must also
			-- be sure to clean it up at the end (see below).
			docPtr := C.libxml.xmlreader.xmlTextReaderCurrentDoc (readerPtr);
			if null = docPtr then
				fputs ("failed to obtain document" & C.char'Val (10) & C.char'Val (0),
					C.stdio.stderr);
				return;
			end if;
			URL := docPtr.URL;
			if null = URL then
				fputs (To_char_const_ptr (URL), C.stdio.stderr);
				fputs (": Failed to obtain URL" & C.char'Val (10) & C.char'Val (0),
					C.stdio.stderr);
			end if;
			if ret /= 0 then
				fputs (To_char_const_ptr (URL), C.stdio.stderr);
				fputs (" : failed to parse" & C.char'Val (10) & C.char'Val (0),
					C.stdio.stderr);
				return;
			end if;
			fputs (To_char_const_ptr (URL), stdout);
			fputs (": Processed ok" & C.char'Val (10) & C.char'Val (0), stdout);
		end processDoc;
		readerPtr : C.libxml.xmlreader.xmlTextReaderPtr;
		docPtr : C.libxml.tree.xmlDocPtr;
		Dummy_signed_int : C.signed_int;
		pragma Unreferenced (Dummy_signed_int);
	begin
		stdout := C.stdio.fopen (
			output (output'First)'Access,
			Write_Mode (Write_Mode'First)'Access);
		-- Create a new reader for the first file and process the document.
		readerPtr := C.libxml.xmlreader.xmlReaderForFile (
			argv1 (argv1'First)'Access,
			null,
			0);
		if null = readerPtr then
			fputs (argv1, C.stdio.stderr);
			fputs (": failed to create reader" & C.char'Val (10) & C.char'Val (0),
				C.stdio.stderr);
			raise Program_Error;
		end if;
		processDoc (readerPtr);
		-- The reader can be reused for subsequent files.
		for i in 2 .. 3 loop
			declare
				argv_i : access constant C.char;
			begin
				case i is
					when 2 => argv_i := argv2 (argv2'First)'Access;
					when 3 => argv_i := argv3 (argv3'First)'Access;
				end case;
				readerPtr := C.libxml.xmlreader.xmlReaderForFile (
					argv_i,
					null,
					0);
				if null = readerPtr then
					fputs (argv_i, C.stdio.stderr);
					fputs (": failed to create reader" & C.char'Val (10) & C.char'Val (0),
						C.stdio.stderr);
					raise Program_Error;
				end if;
				processDoc (readerPtr);
			end;
		end loop;
		-- Since we've called xmlTextReaderCurrentDoc, we now have to
		-- clean up after ourselves.  We only have to do this the last
		-- time, because xmlReaderNewFile calls xmlCtxtReset which takes
		-- care of it.
		docPtr := C.libxml.xmlreader.xmlTextReaderCurrentDoc (readerPtr);
		if docPtr /= null then
			C.libxml.tree.xmlFreeDoc (docPtr);
		end if;
		-- Clean up the reader.
		C.libxml.xmlreader.xmlFreeTextReader (readerPtr);
		Dummy_signed_int := C.stdio.fclose (stdout);
	end reader4;
begin
	-- this initialize the library and check potential ABI mismatches
	-- between the version it was compiled for and the actual shared
	-- library used.
	C.libxml.xmlversion.xmlCheckVersion (C.libxml.xmlversion.LIBXML_VERSION);
	Tests : begin
		reader1 (
			"test2.xml" & C.char'Val (0),
			"reader1.tmp" & C.char'Val (0));
		reader2 (
			"test2.xml" & C.char'Val (0),
			"reader2.tmp" & C.char'Val (0));
		reader3 (
			"reader3.tmp" & C.char'Val (0));
		reader4 (
			"test1.xml" & C.char'Val (0),
			"test2.xml" & C.char'Val (0),
			"test3.xml" & C.char'Val (0),
			"reader4.tmp" & C.char'Val (0));
	end Tests;
	-- Cleanup function for the XML library.
	C.libxml.parser.xmlCleanupParser;
	-- this is to debug memory for regression tests
	C.libxml.xmlmemory.xmlMemoryDump;
end test_reader;
