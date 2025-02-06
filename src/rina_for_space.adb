with Rina;
-- with dif;
-- with ipcp;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Integer_Text_IO;
with Ada.Text_IO; use Ada.Text_IO;
with RINA_Policies; use RINA_Policies;
-- with RINA_Policies.DTN_Bundle_Protocol; 
with dif_manager; use dif_manager;

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

   -- DIF_Manager Tests
   Manager : DIF_manager.DIF_MANAGER;
   Name : Unbounded_String := To_Unbounded_String("Test DIF");

begin
   test := RINA_Policies.Encode_SDNV(1420);
   Put_Line (test'Image);
   Put_Line(RINA_Policies.Decode_SDNV(test)'Image);


   -- DIF_Manager Tests
   Put_Line("Creating DIF with ID 1");
   Create_DIF(1, Manager);
   Put_Line("Creating Named DIF with ID 2 and Name 'Test DIF'");
   Create_Named_DIF(2, Name, Manager);
   Put_Line("Listing all DIFs:");
   List_DIFs(Manager);
   Put_Line("Disconnecting DIF with ID 1");
   Disconnect_DIF(1, Manager);
   Put_Line("Listing all DIFs after disconnection:");
   List_DIFs(Manager);

end Rina_For_Space;
