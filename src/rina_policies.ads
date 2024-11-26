


package RINA_Policies is 
	type Flow_ID is new Integer; 
	type Error_Code is new Integer;
	type QoS_Prameter is record 
		Priority : Priority_Level;
		Latency : Integer;
		Throughput : Integer; 
	end record; 

	procedure Define_Qos (
		Flow : in Flow_ID;
		QoS : in QoS_Parameter
	);

	procedure Get_Flow_Priority (
		Flow : in Flow_ID;
		Priority : out Priority_Level
	);


	procedure Handle_Error (
		Flow : in Flow_ID;
		Code : in Error_Code
	);

	procedure Flow_Control (
		Flow : in Flow_ID;
		Max_Packets : in Positive
	);

	procedure Relay_And_Foward (
		Source_Flow : in Flow_ID;
		Destination_Flow : in Flow_ID
	);

end RINA_Policiess;
