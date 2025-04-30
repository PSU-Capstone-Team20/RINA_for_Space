with Ada.Text_IO; use Ada.Text_IO;
with Ada.Assertions; use Ada.Assertions;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Calendar; use Ada.Calendar;
with EFCP; use EFCP;
with IPC_Data_Transfer; use IPC_Data_Transfer;
with RINA;

procedure Test_IPC_Data_Transfer is

   -- Create mock SDU
   SDU : Byte_Array(1 .. 300);
   PDU_Fragments : PDU_List(1 .. 10);

   -- Mock an expired PDU
   Test_PDU : EFCP.PDU_S_T := (
      ID  => "0000001",
      PCI => (
         Src_CEP_ID => To_Unbounded_String("1"),
         Dst_CEP_ID => To_Unbounded_String("2"),
         Src_Address => RINA.Address_Vectors.Empty_Vector,
         Dst_Address => RINA.Address_Vectors.Empty_Vector,
         Seq_Num     => 1,
         DRF_Flag    => False,
         ECN_Flag    => False,
         QoS_ID      => 1,
         TTL         => 1.0,  -- 1 second
         Ack_Req     => False,
         Retransmit  => False,
         Timestamp   => Ada.Calendar.Clock - 120.0  -- Simulate 2 minutes ago
      ),
      Data => To_Unbounded_String("Example Data")
   );

   -- Mock a valid (non-expired) PDU
   Valid_PDU : EFCP.PDU_S_T := (
      ID  => "0000001",
      PCI => (
         Src_CEP_ID => To_Unbounded_String("1"),
         Dst_CEP_ID => To_Unbounded_String("2"),
         Src_Address => RINA.Address_Vectors.Empty_Vector,
         Dst_Address => RINA.Address_Vectors.Empty_Vector,
         Seq_Num     => 1,
         DRF_Flag    => False,
         ECN_Flag    => False,
         QoS_ID      => 1,
         TTL         => 1.0,
         Ack_Req     => False,
         Retransmit  => False,
         Timestamp   => Ada.Calendar.Clock
      ),
      Data => To_Unbounded_String("Example Data")
   );

   Flow : Flow_ID;

begin
   Put_Line("Running Test_IPC_Data_Transfer...");

   -- Fill SDU with mock data
   for I in SDU'Range loop
      SDU(I) := Byte(I mod 256);
   end loop;

   -- Test DTP
   DTP(SDU, PDU_Fragments);
   Assert(PDU_Fragments'Length > 0, "DTP failed: No fragments created");
   Put_Line("DTP produced " & PDU_Fragments'Length'Image & " fragments.");

   -- Test DTCP with expired PDU
   DTCP(Test_PDU);
   Assert(Test_PDU.PCI.Ack_Req = True, "DTCP Ack_Req not set for expired PDU");
   Assert(Test_PDU.PCI.Retransmit = True, "DTCP Retransmit not set for expired PDU");

   -- Test DTCP with valid PDU
   DTCP(Valid_PDU);
   Assert(Valid_PDU.PCI.Ack_Req = True, "DTCP Ack_Req not set for valid PDU");
   Assert(Valid_PDU.PCI.Retransmit = False, "DTCP Retransmit incorrectly set for valid PDU");

   -- Test Relay_PDU (just prints)
   Put_Line("Expecting Relay_PDU to print:");
   Relay_PDU(Test_PDU);

   -- Test Multiplex_PDU
   Multiplex_PDU(Test_PDU, Flow);
   Assert(Flow = Flow_ID(1 mod 10), "Multiplex_PDU produced incorrect flow");

end Test_IPC_Data_Transfer;
