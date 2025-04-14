with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Containers.Vectors;
with System.Mmap;
with Transport_Types; use Transport_Types;
with IPCP_Types; use IPCP_Types;
--with Rina_BP_Bundle; use Rina_BP_Bundle;

package body IPC_Data_Transfer is

--SDU delmiting extracts SDU from raw byte array 
procedure Delimit_SDU(Raw_Data : in Byte_Array; SDU : out Byte_Array) is
   --first byte of input data to indicate the length of SDU
   SDU_Length : Natural := Natural (Raw_Data (1));
begin
   --take SDU portion based on length, skip length byte 
   SDU := Raw_Data(2 .. SDU_Length + 1);
end Delimit_SDU;

--EFCP both DTP and DTCP for fragmenting SDU into PDUs
procedure DTP(SDU : in Byte_Array; Fragment_PDU : out PDU_List) is
   -- calculates number of fragments to divide the SDU based on maximum PDU size 
   Frag_Num : Natural := (SDU'Length + Max_PDU_Size -1) / Max_PDU_Size;
   --used for tracking current fragment indeex
   Frag_Index : Natural := 1;
   --setting starting byte index
   First_Byte : Positive := SDU'First;
   --declare variable to hold ending byte index for each fragment 
   Last_Byte : Positive;
   
begin
   -- initialize PDU list with either empty or default values for each fragment 
   Fragment_PDU := (1 .. Frag_Num => (others => <>));
   --loop for each fragment to make PDU list
   for I in 1 .. Frag_Num loop
      --calculate last byte for current fragment
      Last_Byte := First_Byte + Max_PDU_Size -1;
      --if last byte index is above SDU's actual size, adjust the last byte 
      if Last_Byte > SDU'Last then 
         Last_Byte := SDU'Last;
      end if;
      declare
         -- fragment byte array that holds the sliced chunk of SDU
         Frag_Data : Byte_Array (1 .. Last_Byte - First_Byte + 1);
         -- makes string version of fragment to be used 
         Convert_Data : String(1 .. Last_Byte - First_Byte + 1);
         --default PCI filled with placeholder values          
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
         --preparing string buffer for fragment ID 
         Fragment_ID : String(1 ..7);
         --converting current loop index to string to use as an ID 
         Temp_ID : constant String := Integer'Image(I);
      begin
         --take actual byte from SDU for fragment
         Frag_Data := SDU (First_Byte .. Last_Byte);
         --for loop to convert each byte into a character 
         for J in Convert_Data'Range loop
            --converting each byte in Frag_Data to character value 
            Convert_Data(J) := Character'Val(Frag_Data(J));
         end loop;
         --for creating fragment strings and using slicing of image to remove leading space 
         Fragment_ID := "Fragment" & Temp_ID(Temp_ID'First + 1 .. Temp_ID'Last);
         --storing the made fragment into the PDU list at index I using ID, default PCI and the converted data 
         Fragment_PDU(I) := (ID => Fragment_ID,
                             PCI => Default_PCI,
                             Data => To_Unbounded_String(Convert_Data));
         --update the first byte index for the next coming fragment
         First_Byte := Last_Byte + 1;
      end;
   end loop;
end DTP;

--procedure for transmission control on a PDU
procedure DTCP(PDU : in out EFCP.PDU_S_T) is
   --the placeholder for current time 
   Real_Time : Natural := 0;
   --gets timestamp from PDU's PCI to get last active 
   Last_Active_Time : Natural := PDU.PCI.Timestamp;
   Max_Time : constant Natural := 1000; -- timeout threshold

begin
   --if active time is greater than max time this would mark PDU for retransmission and request the acknowledgement 
   --interpretation of Delta-t
   if Real_Time - Last_Active_Time > Max_Time then
      PDU.PCI.Ack_Req := True;
      PDU.PCI.Retransmit := True;
   else
      --if it is within the time this would request just the acknowledgement 
      PDU.PCI.Ack_Req := True;
   end if;
   
end DTCP;

--relay pdu
procedure Relay_PDU(PDU : in out EFCP.PDU_S_T) is 
begin
   --logging destination CEP ID the PDU that is being relayed to it 
   Put_Line(To_String("[Relay_PDU] : Relay PDU to  " & PDU.PCI.Dst_CEP_ID));
end Relay_PDU;

--multiplexing 
procedure Multiplex_PDU(PDU : in EFCP.PDU_S_T; Lower_Flow_ID : out Flow_ID) is

begin
   --convert source CEP ID to integer and applies modulo 10 to get the flow then assigns it to Lower_Flow_ID
   Lower_Flow_ID := Flow_ID(Integer'Value(To_String(PDU.PCI.Src_CEP_ID)) mod 10);
end Multiplex_PDU;



end IPC_Data_Transfer;