with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Containers.Vectors;
with System.Mmap;
with Transport_Types; use Transport_Types;
with IPCP_Types; use IPCP_Types;
--with Rina_BP_Bundle; use Rina_BP_Bundle;

package body IPC_Data_Transfer is

--SDU delmiting
procedure Delimit_SDU(Raw_Data : in Byte_Array; SDU : out Byte_Array) is
   SDU_Length : Natural := Natural (Raw_Data (1));
begin
   SDU := Raw_Data(2 .. SDU_Length + 1);
end Delimit_SDU;

--EFCP both DTP and DTCP 
procedure DTP(SDU : in Byte_Array; Fragment_PDU : out PDU_List) is
   Frag_Num : Natural := (SDU'Length + Max_PDU_Size -1) / Max_PDU_Size;
   Frag_Index : Natural := 1;
   First_Byte : Positive := SDU'First;
   Last_Byte : Positive;
   
begin
   Fragment_PDU := (1 .. Frag_Num => (others => <>));
   for I in 1 .. Frag_Num loop
      Last_Byte := First_Byte + Max_PDU_Size -1;
      if Last_Byte > SDU'Last then 
         Last_Byte := SDU'Last;
      end if;
      declare
         Frag_Data : Byte_Array (1 .. Last_Byte - First_Byte + 1);
         Convert_Data : String(1 .. Last_Byte - First_Byte + 1);          
         Default_PCI : Transport_Types.PCI_T := (Src_CEP_ID => To_Unbounded_String(""), 
                                                    Dst_CEP_ID => To_Unbounded_String(""), 
                                                    Seq_Num => 0, 
                                                    DRF_Flag => False, 
                                                    ECN_Flag => False, 
                                                    QoS_ID => 0, 
                                                    TTL => 64, 
                                                    Ack_Req => False, 
                                                    Retransmit => False, 
                                                    Timestamp => 0);
         Fragment_ID : String(1 ..7);
         Temp_ID : constant String := Integer'Image(I);
      begin
         Frag_Data := SDU (First_Byte .. Last_Byte);
         for J in Convert_Data'Range loop
            Convert_Data(J) := Character'Val(Frag_Data(J));
         end loop;
         Fragment_ID := "Fragment" & Temp_ID(Temp_ID'First + 1 .. Temp_ID'Last);  
         Fragment_PDU(I) := (ID => Fragment_ID,
                             PCI => Default_PCI,
                             Data => Convert_Data);
         
         First_Byte := Last_Byte + 1;
      end;
   end loop;
end DTP;

procedure DTCP(PDU : in out EFCP.PDU_S_T) is
   Real_Time : Natural := 0;
   Last_Active_Time : Natural := PDU.PCI.Timestamp;
   Max_Time : constant Natural := 1000; -- timeout threshold

begin
   if Real_Time - Last_Active_Time > Max_Time then
      PDU.PCI.Ack_Req := True;
      PDU.PCI.Retransmit := True;
   else
      PDU.PCI.Ack_Req := True;
   end if;
   
end DTCP;

--relay pdu
procedure Relay_PDU(PDU : in out EFCP.PDU_S_T) is 
begin
   Put_Line(To_String("[Relay_PDU] : Relay PDU to  " & PDU.PCI.Dst_CEP_ID));
end Relay_PDU;

--multiplexing 
procedure Multiplex_PDU(PDU : in EFCP.PDU_S_T; Lower_Flow_ID : out Flow_ID) is

begin
   Lower_Flow_ID := Flow_ID(Integer'Value(To_String(PDU.PCI.Src_CEP_ID)) mod 10);
end Multiplex_PDU;



end IPC_Data_Transfer;