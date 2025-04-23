with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Containers.Vectors;
with Transport_Types; use Transport_Types;
with IPCP_Types; use IPCP_Types;
--with Rina_BP_Bundle; use Rina_BP_Bundle;
with Ada.Calendar; use Ada.Calendar;
with Ada.Calendar.Formatting; use Ada.Calendar.Formatting;
with Ada.Real_Time; use Ada.Real_Time;

package body IPC_Data_Transfer is

--  --SDU delmiting extracts SDU from raw byte array 
--  procedure Delimit_SDU(Raw_Data : in Byte_Array; SDU : out Byte_Array) is
--     --first byte of input data to indicate the length of SDU
--     SDU_Length : Natural := Natural (Raw_Data (1));
--  begin
--     --take SDU portion based on length, skip length byte 
--     SDU := Raw_Data(2 .. SDU_Length + 1);
--  end Delimit_SDU;

--EFCP both DTP and DTCP for fragmenting SDU into PDUs
procedure DTP(SDU : in Byte_Array; Fragment_PDU : out PDU_List) is
   use EFCP;
   First_Byte : Natural := SDU'First;
   Last_Byte  : Natural;
   PDU_Count : Natural := 0;
   Temp_PDU : PDU_S_T;
   Temp_List : PDU_List(1 ..100);
   Temp_ID   : String(1 .. 10);
   Frag_ID : String(1 .. 7);
   --default PCI for each frag
   Default_PCI : PCI_T := (
      Src_CEP_ID => To_Unbounded_String(""),
      Dst_CEP_ID => To_Unbounded_String(""),
      Seq_Num    => 0,
      DRF_Flag   => False,
      ECN_Flag   => False,
      QoS_ID     => 0,
      TTL        => 10.0,
      Ack_Req    => False,
      Retransmit => False,
      Timestamp  => Clock);
   
begin
 
   
   --loop for each fragment to make PDU list
 -- Loop through SDU and slice into fragments
   while First_Byte <= SDU'Last loop
      -- Calculate last byte for this fragment
      Last_Byte := Natural'Min(First_Byte + Max_PDU_Size - 1, SDU'Last);

      declare
         Frag_Length   : constant Natural := Last_Byte - First_Byte + 1;
         Frag_Data     : Byte_Array(1 .. Frag_Length);
         Convert_Data  : String(1 .. Frag_Length);
         Str_Num       : constant String := Integer'Image(PDU_Count + 1);
      begin
         -- Extract fragment
         Frag_Data := SDU(First_Byte .. Last_Byte);

         
         for J in Frag_Data'Range loop
            Convert_Data(J) := Character'Val(Frag_Data(J));
         end loop;

         Frag_ID := (others => '0');
         declare
            Offset : constant Natural := Integer'Min(7, Str_Num'Length -1);
         begin
            Frag_ID(7 - Offset + 1 ..7) := Str_Num(Str_Num'First + 1 .. Str_Num'Last);
         end;
        

        Temp_PDU := (
            ID => Frag_ID,
            PCI => Default_PCI,
            Data => To_Unbounded_String(Convert_Data)
        );

         PDU_Count := PDU_Count + 1;
         Temp_List(PDU_Count) := Temp_PDU;
         First_Byte := Last_Byte + 1;

      end;
   end loop;
end DTP;

--procedure for transmission control on a PDU
procedure DTCP(PDU : in out EFCP.PDU_S_T) is
   --the placeholder for current time 
   Now : Ada.Calendar.Time := Ada.Calendar.Clock;
   --gets timestamp from PDU's PCI to get last active 
   Elapsed_Time : Duration := Now - PDU.PCI.Timestamp;
   Max_Time : Duration := PDU.PCI.TTL; -- timeout threshold

begin
   --if active time is greater than max time this would mark PDU for retransmission and request the acknowledgement 
   --interpretation of Delta-t
   if Elapsed_Time > Max_Time then
      PDU.PCI.Ack_Req := True;
      PDU.PCI.Retransmit := True;
      Put_Line("TTL exceeded. Retransmission initiated..");
   else
      --if it is within the time this would request just the acknowledgement 
      PDU.PCI.Ack_Req := True;
      Put_Line("Acknowledgement requested");
   end if;
   
   
end DTCP;

--relay pdu
procedure Relay_PDU(PDU : in out EFCP.PDU_S_T) is 
begin
   --logging destination CEP ID the PDU that is being relayed to it 
   Put_Line(To_String(" Relay PDU to  " & PDU.PCI.Dst_CEP_ID));
end Relay_PDU;

--multiplexing 
procedure Multiplex_PDU(PDU : in EFCP.PDU_S_T; Lower_Flow_ID : out Flow_ID) is

begin
   --convert source CEP ID to integer and applies modulo 10 to get the flow then assigns it to Lower_Flow_ID
   Lower_Flow_ID := Flow_ID(Integer'Value(To_String(PDU.PCI.Src_CEP_ID)) mod 10);
end Multiplex_PDU;



end IPC_Data_Transfer;