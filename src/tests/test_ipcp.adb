with Ada.Assertions; use Ada.Assertions;
with Ada.Exceptions; use Ada.Exceptions;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;
with IPC_Manager.IPCP; use IPC_Manager.IPCP;
with IPCP_Types; use IPCP_Types;
with Transport_Types; use Transport_Types;

procedure Test_IPCP is
   pragma Assertion_Policy (Check);

   -- Test Make_IPCP to create an IPCP instance
   procedure Test_Make_IPCP is
      IPCP1 : IPCP_T;
      Name  : constant Unbounded_String := To_Unbounded_String("TestIPCP");
   begin
      IPCP1 := Make_IPCP(Name);
      Assert(IPCP1.Name = Name, "IPCP Name should be " & To_String(Name));
      Assert(IPCP1.State = Initialized, "IPCP State should be Initialized");
      Assert(IPCP1.Outgoing_PDUs.Is_Empty, "Outgoing PDUs should be empty");
      Assert(IPCP1.Incoming_PDUs.Is_Empty, "Incoming PDUs should be empty");
      Assert(IPCP1.Active_Flows.Is_Empty, "Active Flows should be empty");
   exception
      when E : Assertion_Error =>
         Put_Line("Assertion failed in Test_Make_IPCP: " & Exception_Message(E));
   end Test_Make_IPCP;

   -- Test Add_Flow and Remove_Flow procedures
   procedure Test_Flow_Management is
      IPCP1 : IPCP_T := Make_IPCP(To_Unbounded_String("FlowTestIPCP"));
      Flow  : Flow_Info_T := (
         Flow_ID       => 1,
         Port_ID       => 1001,
         QoS_ID        => 2,
         Remote_CEP_ID => To_Unbounded_String("RemoteCEP")
      );
   begin
      -- Add a flow
      Add_Flow(IPCP1, Flow);
      Assert(Flow_Exists(IPCP1, 1), "Flow with ID 1 should exist");

      -- Remove the flow
      Remove_Flow(IPCP1, 1);
      Assert(not Flow_Exists(IPCP1, 1), "Flow with ID 1 should not exist after removal");
   exception
      when E : Assertion_Error =>
         Put_Line("Assertion failed in Test_Flow_Management: " & Exception_Message(E));
   end Test_Flow_Management;

   procedure Test_PDU_Management is
   IPCP1 : IPCP_T := Make_IPCP(To_Unbounded_String("PDUTestIPCP"));
   PDU   : PDU_T := (
      ID   => "PDU0001",                                     
      PCI  => (
         Src_CEP_ID  => To_Unbounded_String("SrcCEP"),
         Dst_CEP_ID  => To_Unbounded_String("DstCEP"),
         Seq_Num     => 1,
         DRF_Flag    => False,
         ECN_Flag    => False,
         QoS_ID      => 1,
         TTL         => 64
      ),
      Data => (others => ' ')
   );
   begin
      -- Assign a PDU to outgoing buffer
      Assign_PDU(IPCP1, PDU, True);
      Assert(not IPCP1.Outgoing_PDUs.Is_Empty, "Outgoing PDUs should not be empty");

      -- Get the PDU from outgoing buffer
      declare
         Retrieved_PDU : PDU_T := Get_PDU(IPCP1, True);
      begin
         Assert(Retrieved_PDU.ID = PDU.ID, "Retrieved PDU ID should match the assigned PDU ID");
         Assert(Retrieved_PDU.PCI.Src_CEP_ID = PDU.PCI.Src_CEP_ID, "Retrieved PDU Src_CEP_ID should match the assigned PDU Src_CEP_ID");
         Assert(Retrieved_PDU.PCI.Dst_CEP_ID = PDU.PCI.Dst_CEP_ID, "Retrieved PDU Dst_CEP_ID should match the assigned PDU Dst_CEP_ID");
         Assert(Retrieved_PDU.PCI.Seq_Num = PDU.PCI.Seq_Num, "Retrieved PDU Seq_Num should match the assigned PDU Seq_Num");
         Assert(Retrieved_PDU.PCI.DRF_Flag = PDU.PCI.DRF_Flag, "Retrieved PDU DRF_Flag should match the assigned PDU DRF_Flag");
         Assert(Retrieved_PDU.PCI.ECN_Flag = PDU.PCI.ECN_Flag, "Retrieved PDU ECN_Flag should match the assigned PDU ECN_Flag");
         Assert(Retrieved_PDU.PCI.QoS_ID = PDU.PCI.QoS_ID, "Retrieved PDU QoS_ID should match the assigned PDU QoS_ID");
         Assert(Retrieved_PDU.PCI.TTL = PDU.PCI.TTL, "Retrieved PDU TTL should match the assigned PDU TTL");
         Assert(Retrieved_PDU.Data = PDU.Data, "Retrieved PDU Data should match the assigned PDU Data");
      end;

      -- Pop the PDU from outgoing buffer
      declare
         Popped_PDU : PDU_T := Pop_PDU(IPCP1, True);
      begin
         Assert(Popped_PDU.ID = PDU.ID, "Popped PDU ID should match the assigned PDU ID");
         Assert(Popped_PDU.PCI.Src_CEP_ID = PDU.PCI.Src_CEP_ID, "Popped PDU Src_CEP_ID should match the assigned PDU Src_CEP_ID");
         Assert(Popped_PDU.PCI.Dst_CEP_ID = PDU.PCI.Dst_CEP_ID, "Popped PDU Dst_CEP_ID should match the assigned PDU Dst_CEP_ID");
         Assert(Popped_PDU.PCI.Seq_Num = PDU.PCI.Seq_Num, "Popped PDU Seq_Num should match the assigned PDU Seq_Num");
         Assert(Popped_PDU.PCI.DRF_Flag = PDU.PCI.DRF_Flag, "Popped PDU DRF_Flag should match the assigned PDU DRF_Flag");
         Assert(Popped_PDU.PCI.ECN_Flag = PDU.PCI.ECN_Flag, "Popped PDU ECN_Flag should match the assigned PDU ECN_Flag");
         Assert(Popped_PDU.PCI.QoS_ID = PDU.PCI.QoS_ID, "Popped PDU QoS_ID should match the assigned PDU QoS_ID");
         Assert(Popped_PDU.PCI.TTL = PDU.PCI.TTL, "Popped PDU TTL should match the assigned PDU TTL");
         Assert(Popped_PDU.Data = PDU.Data, "Popped PDU Data should match the assigned PDU Data");
         Assert(IPCP1.Outgoing_PDUs.Is_Empty, "Outgoing PDUs should be empty after pop");
      end;

      -- Clear PDU buffers
      Clear_PDU_Buffer(IPCP1);
      Assert(IPCP1.Incoming_PDUs.Is_Empty, "Incoming PDUs should be empty after clear");
      Assert(IPCP1.Outgoing_PDUs.Is_Empty, "Outgoing PDUs should be empty after clear");
   exception
      when E : Assertion_Error =>
         Put_Line("Assertion failed in Test_PDU_Management: " & Exception_Message(E));
   end Test_PDU_Management;


begin
   Test_Make_IPCP;
   Test_Flow_Management;
   Test_PDU_Management;
end Test_IPCP;
