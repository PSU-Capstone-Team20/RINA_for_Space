with Rina;
with Ada.Text_IO; use Ada.Text_IO;
with RINA_Policies;


procedure Rina_For_Space is

   Flow : Flow_ID := 1;
   Qos : Qos_Paramenter := (Priority => 1, Latency => 100, Throughput => 500);

begin

	Define_Qos(Flow, QoS);

	Schedule_Flow(Flow);

	Put_Line("RINA Policies test");

end Rina_For_Space;
