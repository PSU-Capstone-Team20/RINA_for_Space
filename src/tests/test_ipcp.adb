with Ada.Assertions;      use Ada.Assertions;
with Ada.Exceptions;      use Ada.Exceptions;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO;         use Ada.Text_IO;
with Test_Utils;          use Test_Utils;

with IPC_Manager.IPCP;
with IPCP_Types;
with Transport_Types; use Transport_Types;

procedure Test_IPCP is
   pragma Assertion_Policy (Assert => Ignore);

   procedure Test_Make_IPCP is
      Name     : constant Unbounded_String := To_Unbounded_String("Mock_IPCP");
      IPCP     : IPCP_Types.IPCP_T := IPC_Manager.IPCP.Make_IPCP(Name);
      Mock_PDU : Transport_Types.PDU_T := (
         ID        => "TEST123",
         PCI       => (
                      Src_CEP_ID => +"SRC_CEP001",
                      Dst_CEP_ID => +"DST_CEP001",
                      Seq_Num    => 1,
                      DRF_Flag   => False,
                      ECN_Flag   => False,
                      QoS_ID     => 0,
                      TTL        => 64
                      ),
         Data      => (others => 'X')
      );

   begin
      Assert(To_String(IPCP.Name) = To_String(Name), "IPCP name mismatch");

      IPC_Manager.IPCP.Assign_PDU(IPCP, Mock_PDU, True);
      declare
         Fetched : Transport_Types.PDU_T := IPC_Manager.IPCP.Get_PDU(IPCP, True);
      begin
         Assert(Fetched = Mock_PDU, "Outgoing PDU mismatch after assignment");

         Put_Line("Fetched PDU PCI Info:");
         Put_Line("Source CEP ID: " & To_String(Fetched.PCI.Src_CEP_ID));
         Put_Line("Destination CEP ID: " & To_String(Fetched.PCI.Dst_CEP_ID));
         Put_Line("Sequence Number: " & Integer'Image(Fetched.PCI.Seq_Num));
         Put_Line("DRF Flag: " & Boolean'Image(Fetched.PCI.DRF_Flag));
         Put_Line("ECN Flag: " & Boolean'Image(Fetched.PCI.ECN_Flag));
         Put_Line("QoS ID: " & Integer'Image(Fetched.PCI.QoS_ID));
         Put_Line("TTL: " & Integer'Image(Fetched.PCI.TTL));
      end;

      declare
         Fetched : Transport_Types.PDU_T := IPC_Manager.IPCP.Pop_PDU(IPCP, True);
      begin
         null;
      end;

      IPC_Manager.IPCP.Clear_PDU_Buffer(IPCP);
   end Test_Make_IPCP;

begin
   Put_Line("Running Test_Make_IPCP...");
   Test_Make_IPCP;
   Put_Line("Test_IPCP completed successfully.");
end Test_IPCP;
