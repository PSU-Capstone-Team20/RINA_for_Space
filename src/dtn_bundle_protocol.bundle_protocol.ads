with DTN_Bundle_Protocol;

package DTN_Bundle_Protocol.Bundle_Protocol is 
	type Bundle_Type is record 
		ID : Integer;
		Source : DTN_Bundle_Protocol.Node_ID_Type;
		Timestamp : DTN_Bundle_Protocol.Timestamp_Type;
		Priority : DTN_Bundle_Protocol.Priority_Type;
		DIF_ID : DIF_ID_Type;
		Payload : String(1 .. 1024); -- size 1 KB
		Custody_Flag : Boolean := False;
	end record;

	procedure Create_Bundle (Source : in DTN_Bundle_Protocol.Node_ID_Type;
							 Destination : in DTN_Bundle_Protocol.Node_ID_Type;
							 Priority : in DTN_Bundle_Protocol.Priority_Type;
							 Payload : in String;
							 DIF_ID : in DIF_ID_Type;
							 Bundle : out Bundle_Type;)
end DTN_Bundle_Protocol.Bundle_Protocol;