with Ada.Text_IO; use Ada.Text_IO;
with Ada.Assertions; use Ada.Assertions;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Calendar; use Ada.Calendar;
with EFCP; use EFCP;
with Transport_Types; use Transport_Types;
with RINA;  use RINA;

-- Test the EFCP package
procedure Test_EFCP is

   Original_SDU : SDU_S_T := (
      PCI  => (
         Src_CEP_ID  => To_Unbounded_String("AppA"),
         Dst_CEP_ID  => To_Unbounded_String("AppB"),
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
      Data => To_Unbounded_String("HelloWorldEFCPTest")
   );

   Fragmented_PDU : PDU_S_T;
   Reassembled_SDU : SDU_S_T;
   P1, P2 : SDU_S_T;
   Combined : SDU_S_T;
   Fragment_Size_Str : constant String := "5";

begin
   -- Test Fragment
   declare
      Temp_SDU : SDU_S_T := Original_SDU;
   begin
      Fragment(Temp_SDU, Fragment_Size_Str, Fragmented_PDU);
      Put_Line("Fragmented Data: " & To_String(Fragmented_PDU.Data));
      Assert(To_String(Fragmented_PDU.Data) = "Hello");
      Assert(Fragmented_PDU.PCI.DRF_Flag = True);
   end;

   -- Test Reassemble
   Reassemble(Fragmented_PDU, Reassembled_SDU);
   Assert(To_String(Reassembled_SDU.Data) = To_String(Fragmented_PDU.Data));
   Put_Line("Reassemble passed.");

   -- Test Concatenate
   P1.Data := To_Unbounded_String("Foo");
   P2.Data := To_Unbounded_String("Bar");
   Concatenate(P1, P2, Combined);
   Assert(To_String(Combined.Data) = "FooBar");
   Put_Line("Concatenate passed.");

   -- Test Separation
   declare
      Split_SDU : SDU_S_T := (
         PCI => Original_SDU.PCI,
         Data => To_Unbounded_String("ABCDEFGH")
      );
   begin
      Separation(Split_SDU, P1, P2);
      Assert(To_String(P1.Data) = "ABCD");
      Assert(To_String(P2.Data) = "EFGH");
      Put_Line("Separation passed.");
   end;

   -- Test Control_Transmit
   declare
      Test_PDU : PDU_S_T := Fragmented_PDU;
   begin
      Control_Transmit(Test_PDU);
      Assert(Test_PDU.PCI.Seq_Num = Fragmented_PDU.PCI.Seq_Num + 1);
      Put_Line("Control_Transmit passed.");
   end;

   -- Test Retransmit (prints only)
   Retransmit(Fragmented_PDU);

   -- Test Flow_Control (prints only)
   Flow_Control(Fragmented_PDU);

end Test_EFCP;
