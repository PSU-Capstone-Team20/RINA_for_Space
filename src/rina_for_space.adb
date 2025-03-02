-- with Rina;
-- with dif;
-- with ipcp;
with cdap;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Integer_Text_IO;
with Ada.Text_IO; use Ada.Text_IO;
with Rina_BP_Bundle; use Rina_BP_Bundle;
with Ada.Streams.Stream_IO;
with Ada.Calendar; use Ada.Calendar;
with DIF_Manager; use DIF_Manager;
with IPC_Manager; use IPC_Manager;
with IPCP; use IPCP;
-- with IPC_API; use IPC_API;
with RIB; use RIB;
with fakeComp;
with Ada.Containers.Vectors;


procedure Rina_For_Space is

   DIF_M : DIF_MANAGER_T;
   IPC_M : IPCP_Manager_T;

   IPC_M_Joe : IPCP_Manager_T;
   --  task Joe_Comp;
   --  task body Joe_Comp is
   --     begin
   --     null;
   --  end Joe_Comp;



   IPC_M_Steve : IPCP_Manager_T;
   --  task Steve_Comp;
   --  task body Steve_Comp is
   --     begin
   --     null;
   --  end Steve_Comp;
   
   IPC_M_Chad : IPCP_Manager_T;
   --  task Chad_Comp;
   --  task body Chad_Comp is
   --     begin
   --     while true loop
   --        Put_Line ("Operating IPC Manager");
   --     end loop;
   --  end Chad_Comp;

   package DIF_Vectors is new Ada.Containers.Vectors
      (Index_Type => Natural, Element_Type => DIF_Access);
   
   type Task_Comp_Access is access all fakeComp.fake_comp;

   package Task_Comp_Vectors is new Ada.Containers.Vectors
      (Index_Type => Natural, Element_Type => Task_Comp_Access);

   TC_V : Task_Comp_Vectors.Vector;

   killflag : Integer := 0;


   -- ****NEW****
   Task running;
   task body running is
   begin
      while killflag = 0 loop
      for i in TC_V.First_Index .. TC_V.Last_Index loop
         TC_V.Reference(i).operate;
      end loop;
      end loop;
   end running;

   procedure newfakecomp is
      New_Task : constant Task_Comp_Access := new fakeComp.fake_comp;
   begin
      TC_V.Append (New_Task);
   end;
   -- ****NEW****


   -- IPC API Test 
   --  Port : IPC_API.Port_ID;
   --  Received_SDU : String(1 .. 1024);

   -- Testing PDU operations
   Test_IPCP : IPCP.IPCP_Access;
   Test_PDU  : IPCP.PDU_T;
   PCI_Data  : IPCP.PCI_T := (Seq_Num => 1, DRF_Flag => True, ECN_Flag => False, QoS_ID => 3);

   --testing for bundle 
   B : Bundle;

   

begin

   newfakecomp;
   TC_V.Reference(0).change_name(To_Unbounded_String("Joe"));

   delay 1.0;

   RIB.Display_Map;

   

   

   --create the bundle 
   --  B := Rina_BP_Bundle.Create_Bundle(Version => 6, 
   --                                    Processing_Flag => 2, 
   --                                    Block_Length => 512, 
   --                                    Src_EID => "Mars Observer", 
   --                                    Dst_EID => "Ground Station 1", 
   --                                    Payload => "Water found in region 5");


   --  --serialize bundle
   --  Send_Bundle(B);

   --  --receive the bundle
   --  --B := Receive_Bundle;

   --  --details of bundle printout 
   --  Put_Line("Received Data from Bundle: ");
   --  Put_Line("Version: " & Natural'Image(B.Header.Version));
   --  Put_Line("Processing Flag: " & Natural'Image(B.Header.Processing_Flag));
   --  Put_Line("Block Length: " & Natural'Image(B.Header.Block_Length));
   --  Put_Line("Source: " & B.Src_EID);
   --  Put_Line("Destination: " & B.Dst_EID);
   --  Put_Line("Payload: " & B.Payload);

   --  Put_Line("Bundle has been processed successfully");

   -- Testing DIF & IPCP creation without connection 
   --  Create_Named_DIF (1, To_Unbounded_String("DIF_1"), DIF_M);
   --  Create_Named_DIF (2, To_Unbounded_String("DIF_2"), DIF_M);

   --  Create_IPCP(To_Unbounded_String("IPCP_A"), To_Unbounded_String("IPCP_A_for_DIF_1"), IPC_M);
   --  Create_IPCP(To_Unbounded_String("IPCP_B"), To_Unbounded_String("IPCP_B_for_DIF_1"), IPC_M);
   --  Create_IPCP(To_Unbounded_String("IPCP_C"), To_Unbounded_String("IPCP_C_for_DIF_2"), IPC_M);
   
   --  -- List all existing DIFs and IPCPs
   --  Put_Line("Existing DIFs:");
   --  List_DIFs(DIF_M);
   --  Put_Line("");

   --  Put_Line("Existing IPCPs:");
   --  List_IPCPs(IPC_M);
   --  Put_Line("");

      -- Allocate a flow between source and destination applications
   --  Port := Allocate(To_Unbounded_String("IPCP_A"), 
   --                   To_Unbounded_String("IPCP_B"), 
   --                   1, 
   --                   DIF_M, 
   --                   IPC_M); -- Priority Level 1

   --  -- Check if allocation was successful
   --  if Port /= 0 then
   --     Put_Line("Flow allocated with Port ID: " & Port'Image);

   --     -- Send an SDU to the destination application process
   --     Send(Port, "Hello from Source to Destination!", IPC_M);
   --     Put_Line("Sent SDU to port " & Port'Image);

   --     -- Receive an SDU from the destination application process
   --     Received_SDU := Receive(Port, IPC_M);
   --     Put_Line("Received SDU: " & Received_SDU);

   --     -- Deallocate the flow and release resources
   --     Deallocate(Port, IPC_M);
   --     Put_Line("Flow deallocated for Port ID: " & Port'Image);
   --  end if;

   Put_Line(" ");

   Create_IPCP(To_Unbounded_String("IPCP_Test"), To_Unbounded_String("IPCP_Test_ID"), IPC_M);
   Test_IPCP := Find_IPCP(IPC_M, To_Unbounded_String("IPCP_Test_ID"));
   Test_PDU := Create_PDU(ID        => "PDU_001",
                           P_Type    => DT,
                           Src_Addr  => "192.168.1.1",
                           Dst_Addr  => "192.168.1.2",
                           PCI       => PCI_Data,
                           SDU       => "This is a test payload." & (1 .. 1024 - 23 => ' '));

   Put_Line("Created PDU Details:");
   Put_Line("PDU ID: " & Test_PDU.ID);
   Put_Line("PDU Type: " & PDU_Type'Image(Test_PDU.P_Type));
   Put_Line("Source Address: " & Test_PDU.Src_Addr);
   Put_Line("Destination Address: " & Test_PDU.Dst_Addr);
   Put_Line("Payload: " & Test_PDU.SDU);
   Put_Line("Processing the PDU...");
   Process_PDU(Test_IPCP.all, Test_PDU);
   Put_Line("Stored PDUs in IPCP:");
   for P of Test_IPCP.PDUs loop
      Put_Line("PDU ID: " & P.ID & ", Src: " & P.Src_Addr & ", Dst: " & P.Dst_Addr);
   end loop;



   






   
   --test := RINA_Policies.Encode_SDNV(1420);
   --Put_Line (test'Image);
   --Put_Line(RINA_Policies.Decode_SDNV(test)'Image);
   killflag := 1;
   delay 1.0;
end Rina_For_Space;

