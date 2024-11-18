with DTN_Bundle_Protocol;
with DTN_Bundle_Protocol.Bundle_Protocol;

package DTN_Bundle_Protocol.DTN is 
	type DTN_Status_Type is (Pending, In_Transit, Delivered, Failed);

	type DTN_Bundle_Type is new DTN_Bundle_Protocol.Bundle_Protocol.Bundle_Type with record
		Status : DTN_Status_Type := Pending;
		Retry_Count : Integer := 0;
		Last Attempt : DTN_Bundle_Protocol.Timestamp_Type;
	end record;

	type Buffer_Type is array (1 .. 100)  of DTN_Bundle_Type; 

	procedure Store_Bundle (Bundle : in DTN_Bundle_Protocol; 
							Node_Buffer : in out Buffer_Type);
	procedure Forward_Bundle (Bundle : in out DTN_Bundle_Type; 
							  DIF_ID : in DTN_Bundle_Protocol.Bundle_Protocol.DIF_ID_Type; 
							  Next_Node : in DTN_Bundle_Protocol.Node_ID_Type);
	procedure Retry_Bundles(Node_Buffer : in out Buffer_Type;
							DIF_ID : in DTN_Bundle_Protocol.Bundle_Protocol.DIF_ID_Type);
end DTn_Bundle_Protocol.DTN;