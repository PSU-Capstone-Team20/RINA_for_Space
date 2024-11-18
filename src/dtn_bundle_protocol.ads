package DTN_Bundle_Protocol is 
	type Node_ID_Type is new Integer; 
	type Timestamp_Type is record
		Seconds : Integer;
		Nanoseconds : Integer;
	end record;

	type Priority_Type is (Low, Normal, High);

	procedure Initialize_Network;
end DTN_Bundle_Protocol;