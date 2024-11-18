package body DTN_Bundle_Protocol.Bundle_Protocol is 
	procedure Create_Bundle (Source : in DTN_Bundle_Protocol.Node_ID_Type;
							 Destination : in DTN_Bundle_Protocol.Node_ID_Type;
							 Priority : in DTN_Bundle_Protocol.Priority_Type;
							 Payload : in String;
							 Bundle : out Bundle_Type) is 
	begin 
		Bundle.Source := Source;
		Bundle.Destination := Destination;
		Bundle.Timestampe := (Seconds => 0, Nanoseconds => 0);
		Bundle.Priority := Payload;
		Bundle.Custody_Flag := False;
	end Create_Bundle;
end DTN_Bundle_Protocol.Bundle_Protocol;