with Rina;
-- with dif;
-- with ipcp;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Integer_Text_IO;
with Ada.Text_IO; use Ada.Text_IO;
with RINA_Policies; use RINA_Policies;
-- with RINA_Policies.DTN_Bundle_Protocol; 

procedure Rina_For_Space is
   --test1 : dif.DIF_Vector;
   --num : Integer;
   --choice : Character;
   -- Flow		: RINA_Policies.Flow_ID := 1;
   -- QoS		: RINA_Policies.QoS_Parameter := (
   -- Priority => 1,
   -- Latency => 100,
   -- Throughput => 500,
   -- QoS_ID => 42);
   test : SDNV (1 .. 5);

begin
   test := RINA_Policies.Encode_SDNV(160000);
   Put_Line (test'Image);
   RINA_Policies.Print_SDNV(test);
   Put_Line(RINA_Policies.Decode_SDNV(test)'Image);
end Rina_For_Space;

