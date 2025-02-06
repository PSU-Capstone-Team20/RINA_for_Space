-- with Rina;
-- with dif;
-- with ipcp;
with Ada.Text_IO; use Ada.Text_IO;
--with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Integer_Text_IO;
with Ada.Text_IO; use Ada.Text_IO;
with Rina_BP_Bundle; use Rina_BP_Bundle;
with Ada.Streams.Stream_IO;
with Ada.Calendar; use Ada.Calendar;
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

   --create the bundle 
   B := Rina_BP_Bundle.Create_Bundle(Version => 6, Processing_Flag => 2, Block_Length => 512, Src_EID => "Mars Observer", Dst_EID => "Ground Station 1", Payload => "Water found in region 5");


   --serialize bundle
   Send_Bundle(B);

   --receive the bundle
   B := Receive_Bundle;

   --details of bundle printout 
   Put_Line("Received Data from Bundle: ");
   Put_Line("Version: " & Natural'Image(B.Header.Version));
   Put_Line("Processing Flag: " & Natural'Image(B.Header.Processing_Flag));
   Put_Line("Block Length: " & Natural'Image(B.Header.Block_Length));
   Put_Line("Source: " & B.Src_EID);
   Put_Line("Destination: " & B.Dst_EID);
   Put_Line("Payload: " & B.Payload);

   Put_Line("Bundle has been processed successfully");
   

   
   --test := RINA_Policies.Encode_SDNV(1420);
   --Put_Line (test'Image);
   --Put_Line(RINA_Policies.Decode_SDNV(test)'Image);


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
