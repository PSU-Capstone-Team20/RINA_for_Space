with Ada.Text_IO; use Ada.Text_IO;
with Rina;
package body RINA_Policies is

type QoS_Table is array (Flow_ID range <>) of QoS_Parameter;
Flow_QoS : QoS_Table(1 .. 100);

procedure Define_QoS (
Flow : in Flow_ID;
QoS : in QoS_Parameter
) is
begin
Flow_QoS(Flow) := QoS;
Put_Line("Defining QoS for Flow " & Integer'Image(Flow) & ": Priority " & Integer'Image(QoS.Priority) & ", Latency " & Integer'Image(QoS.Latency) & "ms, " & "Throughput " & Integer'Image(QoS.Throughput) & " kbps");
end Define_QoS;

procedure Schedule_Flow (
Flow: in Flow_ID
) is
begin
Put_Line("Scheduling FLow " & Integer'Image(Flow) & " with Priority " & Integer'Image(Priority));
end Schedule_Flow;

procedure Handle_Error (
Flow : in Flow_ID;
Code : in Error_Code
) is
begin
Put_Line("Error on Flow " & Integer'Image(Flow) & ": Code " & Integer'Image(Code));
end Handle_Error;

procedure Flow_Control(
Flow : in Flow_IDl	Max_Packets : in Positive 
) is
begin
Put_Line("Controlling Flow " & Integer'Image(Flow) & "with max packets" & Positive'Image(Max_Packets));
end Flow_Control;

procedure Relay_And_Forward (
Source_Flow : in Flow_ID;
Destination_Flow : in Flow_ID
) is
begin
Put_Line("Relaying packets from Flow "  & Integer'Image(Source_Flow) & " to Flow " & Integer'Image(Destination_Flow));
end Relay_And_Forward;

end RINA_Policies;