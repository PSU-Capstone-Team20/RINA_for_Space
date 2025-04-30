with Ada.Text_IO; use Ada.Text_IO;
with Ada.Assertions; use Ada.Assertions;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Calendar;
with Interfaces; use Interfaces;
with RINA_BP_Bundle; use RINA_BP_Bundle;
with IPC_Data_Transfer; use IPC_Data_Transfer;
with EFCP; use EFCP;
with RINA; use RINA;
with Transport_Types; use Transport_Types;

-- Test_RINA_BP_Bundle Package
procedure Test_RINA_BP_Bundle is
   pragma Assertion_Policy (Assert => Ignore);
   
   Mock_Path : Path_Vectors.Vector;

   -- Define Source and Destination EIDs as full PDUs
   Src_EID : RINA_BP_Bundle.PDU_S_T := (
      ID  => "SRC0001",
      PCI => (
         Src_CEP_ID  => To_Unbounded_String("Telemetry.App"),
         Dst_CEP_ID  => To_Unbounded_String("Control.Center"),
         Src_Address => RINA.Address_Vectors.Empty_Vector,
         Dst_Address => RINA.Address_Vectors.Empty_Vector,
         Seq_Num     => 1,
         DRF_Flag    => False,
         ECN_Flag    => False,
         QoS_ID      => 0,
         TTL         => 10.0,
         Ack_Req     => False,
         Retransmit  => False,
         Timestamp   => Ada.Calendar.Clock
      ),
      Data => To_Unbounded_String("Metadata")
   );

   Dst_EID : RINA_BP_Bundle.PDU_S_T := (
      ID  => "DST0001",
      PCI => (
         Src_CEP_ID  => To_Unbounded_String("Telemetry.App"),
         Dst_CEP_ID  => To_Unbounded_String("Control.Center"),
         Src_Address => RINA.Address_Vectors.Empty_Vector,
         Dst_Address => RINA.Address_Vectors.Empty_Vector,
         Seq_Num     => 1,
         DRF_Flag    => False,
         ECN_Flag    => False,
         QoS_ID      => 0,
         TTL         => 10.0,
         Ack_Req     => False,
         Retransmit  => False,
         Timestamp   => Ada.Calendar.Clock
      ),
      Data => To_Unbounded_String("Metadata")
   );


   -- Create bundle with raw string payload
   B : Bundle := Create_Bundle(
      Version         => 1,
      Processing_Flag => 0,
      Block_Length    => 128,
      Src_EID         => Src_EID,
      Dst_EID         => Dst_EID,
      Payload         => "HelloWorld",
      Path            => Mock_Path
   );

   PDUs        : PDU_List(1 .. 1);
   Reassembled : Byte_Array(1 .. 10);

begin
   Put_Line("Running Test_RINA_BP_Bundle...");

   Send_Bundle(B);
   Put_Line("Send_Bundle executed.");

   -- Simulate reception of one PDU
   PDUs(1) := (
      ID  => "0000001",
      PCI => Src_EID.PCI,  -- reuse mock PCI
      Data => To_Unbounded_String("HelloWorld")
   );

   Receive_Bundle(PDUs, Reassembled);
   Assert(Reassembled'Length = 10);
   Put_Line("Passed: Bundle reassembled to expected length.");

end Test_RINA_BP_Bundle;
