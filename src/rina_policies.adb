

package body RINA_Policiess is

	type QoS_Table is array (Flow_ID range <>) of QoS_Parameter;
	Flow_QoS : QoS_Table(1 .. 100);

	procedure Define_QoS (
		Flow : in Flow_ID;
		QoS : in QoS_Parameter
	) is
	begin
		Flow_QoS(Flow) := QoS;
		Put_Line("Defined QoS for Flow " & Integer'Image(Flow) & ": Priority " & Integer'Image(QoS.Priority) & ", Latency " & Integer'Image(QoS.Latency) & "ms, " & "Throughput " & Integer'Image(QoS.Throughput) & " kbps");
	end Define_QoS;

	procedure Get_Flow_Priority (
		Flow : in Flow_ID;
		Priority : out Priority_Level
	) is
	begin
		Priority := Flow_QoS(Flow).Priority; 
	end Get_Flow_Priority;

	procedure Schedule_Flow (
		Flow: in Flow_ID
	) is
	being 
		Get_Flow_Priority(Flow, Priority);
		Put_Line("Scheduling FLow " & Integer'Image(Flow) & " with Priority " & Integer'Image(Priority));
	end Schedule_Flow;

	procedure Handle_Error (
		Flow : in Flow_ID;
		Code : in Error_Code 
	) is
	begin 
		Put_Line("Error on Flow " & Integer'Image(Flow) & ": Code " & Integer'Image(Code));
	end Handle_Error

	procedure Flow_Control(
		Flow : in Flow_IDl
		Max_Packets : in Positive 
	) is 
	begin 
		Put_Line("Controlling Flow " & Integer'Image(Flow) & "with max packets" & Positive'Image(Max_Packets));
	end Flow_Control;

	procedure Relay_And_Forward (
		Source_Flow : in Flow_ID;
		Destination_Flow : in Flow_ID
	) is 
	begin
		null;
	end Relay_And_Forward;

	end RINA_Policies;