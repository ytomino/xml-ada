with Ada.Streams;
package XML.Streams is
	pragma Preelaborate;
	
	function Create (
		Stream : not null access Ada.Streams.Root_Stream_Type'Class;
		Encoding : Encoding_Type := No_Encoding;
		URI : String := "")
		return Reader;
	
	function Create (
		Stream : not null access Ada.Streams.Root_Stream_Type'Class;
		Encoding : Encoding_Type := No_Encoding;
		Version : access constant String := null;
		Standalone : Standalone_Type := No_Specific)
		return Writer;
	
end XML.Streams;
