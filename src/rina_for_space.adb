-- with Rina;
-- with dif;
-- with ipcp;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Integer_Text_IO;
with Ada.Text_IO; use Ada.Text_IO;
with Rina_BP_Bundle; use Rina_BP_Bundle;
with Ada.Streams.Stream_IO;
with Ada.Calendar; use Ada.Calendar;
with DIF_Manager; use DIF_Manager;
with IPC_Manager; use IPC_Manager;


procedure Rina_For_Space is

   DIF_M : DIF_MANAGER_T;
   IPC_M : IPCP_Manager_T;


   --test1 : dif.DIF_Vector;
   --num : Integer;
   --choice : Character;
   -- Flow		: RINA_Policies.Flow_ID := 1;
   -- QoS		: RINA_Policies.QoS_Parameter := (
   -- Priority => 1,
   -- Latency => 100,
   -- Throughput => 500,
   -- QoS_ID => 42);
   --test : SDNV (1 .. 5);

   --testing for bundle 
   B : Bundle;

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

   Create_Named_DIF (1, To_Unbounded_String("DIF_1"), DIF_M);
   Create_Named_DIF (2, To_Unbounded_String("DIF_2"), DIF_M);

   Create_IPCP(To_Unbounded_String("IPCP_A"), To_Unbounded_String("IPCP_A_for_DIF_1"), IPC_M);
   Create_IPCP(To_Unbounded_String("IPCP_B"), To_Unbounded_String("IPCP_B_for_DIF_1"), IPC_M);
   Create_IPCP(To_Unbounded_String("IPCP_C"), To_Unbounded_String("IPCP_C_for_DIF_2"), IPC_M);
   
   -- List all existing DIFs and IPCPs
   Put_Line("Existing DIFs:");
   List_DIFs(DIF_M);
   Put_Line("");

   Put_Line("Existing IPCPs:");
   List_IPCPs(IPC_M);
   Put_Line("");

   -- Connect IPCPs to their respective DIFs
   --  Connect_IPCP_to_DIF(To_Unbounded_String("IPCP_A_for_DIF_1"), DIF_M.DIFs.Element(1).all, IPC_M);
   --  Connect_IPCP_to_DIF(To_Unbounded_String("IPCP_B_for_DIF_1"), DIF_M.DIFs.Element(1).all, IPC_M);
   --  Connect_IPCP_to_DIF(To_Unbounded_String("IPCP_C_for_DIF_2"), DIF_M.DIFs.Element(2).all, IPC_M);


   Put_Line("Test completed successfully.");
   
   --test := RINA_Policies.Encode_SDNV(1420);
   --Put_Line (test'Image);
   --Put_Line(RINA_Policies.Decode_SDNV(test)'Image);
end Rina_For_Space;

