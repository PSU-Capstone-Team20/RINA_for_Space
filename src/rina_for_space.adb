with Rina;
with Ada.Text_IO; use Ada.Text_IO;
with RINA_Policies;

procedure Rina_For_Space is
   Flow : RINA_Policies.Flow_ID := 1;
   QoS : RINA_Policies.QoS_Parameter := (
   Priority => 1,
   Latency => 100,
   Throughput => 500,
   QoS_ID => 42);
begin
   Put_Line ("Initializing test");
   RINA_Policies.Define_QoS (Flow, QoS);
   RINA_Policies.Schedule_Flow (Flow);
   RINA_Policies.Relay_And_Forward (
   Source_Flow => Flow, Destination_Flow => 2);
   RINA_Policies.Handle_Error (Flow, Error_Code => 404);
   Put_Line ("RINA Policies test");
end Rina_For_Space;
