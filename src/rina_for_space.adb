with Rina;
with Ada.Text_IO; use Ada.Text_IO;
with RINA_Policies; use RINA_Policies;
with RINA_Policies.DTN_Bundle_Protocol; 

procedure Rina_For_Space is
   Flow		: RINA_Policies.Flow_ID := -1;
   Bundle   : RINA_Policies.DTN_Bundle_Protocol.Bundle_ID := 1000;
begin

   Put_Line("Initializing test");
   RINA_Policies.DTN_Bundle_Protocol.Create_Bundle(Flow, Bundle);
   RINA_Policies.DTN_Bundle_Protocol.Send_Bundle(Flow, Bundle);
   RINA_Policies.DTN_Bundle_Protocol.Receive_Bundle(Flow, Bundle);
   RINA_Policies.DTN_Bundle_Protocol.Handle_Custody(Bundle,RINA_Policies.DTN_Bundle_Protocol.Pending);
 
   Put_Line("RINA Policies test complete");
end Rina_For_Space;

