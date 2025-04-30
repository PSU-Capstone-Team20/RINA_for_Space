with Ada.Text_IO; use Ada.Text_IO;
with Ada.Assertions; use Ada.Assertions;
with Ada.Calendar; use Ada.Calendar;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with EFCP; use EFCP;
with Transport_Types; use Transport_Types;
with IPCP_Types; use IPCP_Types;
with IPC_Manager.IPCP; use IPC_Manager.IPCP;
with IPC_Data_Transfer; use IPC_Data_Transfer;
with Test_Utils; use Test_Utils;
with RINA; use RINA;

procedure Test_IPCP_Ack_Buffer is
   PDU : PDU_T;
   IPCP : IPCP_T;
   Retrieved : PDU_T;
begin
   Put_Line("Acknowledgement and IPCP Buffering...");

   -- Create mock IPCP
   IPCP := Make_IPCP(+"Test.IPCP");

   -- Create PDU with expired TTL and ACK requested
   PDU := (
      ID => "ACK0001",
      PCI => (
         Src_CEP_ID  => +"1",
         Dst_CEP_ID  => +"2",
         Src_Address => Address_Vectors.Empty_Vector,
         Dst_Address => Address_Vectors.Empty_Vector,
         Seq_Num     => 42,
         DRF_Flag    => False,
         ECN_Flag    => False,
         QoS_ID      => 1,
         TTL         => 0.5, -- Short TTL
         Ack_Req     => True,
         Retransmit  => False,
         Timestamp   => Clock - 60.0  -- Simulate old PDU
      ),
      Data => +"Payload needing ACK"
   );

   -- Buffer the PDU in IPCP
   -- Simulate deferred transmission
   Assign_PDU(IPCP, PDU, To_Outgoing => False);
   Assert(Natural(IPCP.Incoming_PDUs.Length) = 1, "PDU should be buffered in Incoming_PDUs");

   -- Trigger retransmission logic (DTCP)
   DTCP(PDU);
   Assert(PDU.PCI.Ack_Req, "ACK flag should still be set after DTCP");
   Assert(PDU.PCI.Retransmit, "PDU should be marked for retransmission due to TTL expiry");

   -- Retrieve and verify stored PDU
   Retrieved := Get_PDU(IPCP, From_Outgoing => False);
   Assert(To_String(Retrieved.Data) = To_String(PDU.Data), "Buffered PDU data must match original");

   Put_Line("Acknowledgement handling and IPCP buffering validated.");
end Test_IPCP_Ack_Buffer;
