with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Containers.Vectors;
with Transport_Types; use Transport_Types;
with IPCP_Types; use IPCP_Types;
with Rina_BP_Bundle; use Rina_BP_Bundle;

package body IPC_Data_Transfer is



   --function for SDU delimiting 
   function Delimit_SDU(Data : Unbounded_String) return SDU_T is 
      PCI : PCI_T := (Src_CEP_ID => To_Unbounded_String ("Source"),
                      Dst_CEP_ID => To_Unbounded_String("Destination"),
                      Seq_Num    => 0,
                      DRF_Flag   => True,
                      ECN_Flag   => False,
                      QoS_ID     => 1,
                      TTL        => 10);
      begin 
         return (PCI => PCI, Data => Data);
   end Delimit_SDU;

   --EFCP DTP functions
   function DTP_Fragment(SDU : SDU_T; Fragment_Size : Positive) return PDU_Buffer is
      PDUs        : PDU_Buffer;
      Data_String : String := To_String(SDU.Data);
      Total       : Natural := Data_String'Length;
      Index       : Natural := 1;
      Counter     : Natural := 0;
      begin
         while Index <= Total loop  
            declare 
               Chunk : String := Data_String(Index .. Integer'Min(Index+Fragment_Size -1, Total));
               PDU   : PDU_T := (ID   => (other => '0'),
                                 PCI  => SDU.PCI,
                                 Data => (others => ' '));
            begin 
               Counter := Counter +1;
               PDU.ID(1 ..6) := Integer'Image(Counter)(2..7);
               PDU.PCI.Seq_Num := Counter;
               PDU.Data(1 .. Chunk'Length) := Chunk;
               PDUs.Append(PDU);
            end;
            Index := Index + Fragment_Size;
         end loop;
         return PDUs;
   end DTP_Fragment;

   function DTP_Reassemble(PDUs : PDU_Buffer) return SDU_T is
      Full_Data : Unbounded_String := To_Unbounded_String("");
   begin
      for P of PDUs loop
         declare
            Fragment : String := P.Data;
         begin
            Full_Data := Full_Data & To_Unbounded_String(Fragment);
         end;
      end loop;
      return (PCI => PDUs(0).PCI, Data => Full_Data);
   end DTP_Reassemble;

   --EFCP DTCP procedure
   procedure DTCP_Control(PDUs : in out PDU_Buffer) is
   begin 
      for P of PDUs loop
         if P.PCI.TTL = 0 then
            Put_Line("TTL expiration, PDU dropped: " & P.ID);
         else 
            P.PCI.TTL := P.PCI.TTL -1;
         end if;
      end loop;
   end DTCP_Control;

   --relaying task 
   procedure Relay_PDU(PDU : PDU_T; Dest : Unbounded_String) is
   begin
      Put_Line("Relay PDU " & PDU.ID & " to" & To_String(Dest));
   end Relay_PDU;

   --multiplexing
   procedure Multiplex_Flows(Flows : in Flow_List; PDUs : in out PDU_Buffer) is
   begin
      for I in PDUs.First_Index .. PDUs.Last_Index loop
         PDUs(I).PCI.QoS_ID := Flows(I mod Flows.Length).QoS_ID;
         Put_Line("Multiplexd PDU" & PDUs(I).ID & "to Flow QoS" & Integer'Image(PDUs(I).PCI.QoS_ID));
      end loop;
   end Multiplex_Flow;

   --SDU prtect
   procedure Protect_SDU(SDU : in out SDU_T) is
      Hash : Natural := 0;
      Str  : String := To_String(SDU.Data);
   begin
      for C of Str loop 
         Hash := (Hash + Character'Pos(C)) mod 256;
      end loop;
      SDU.Data := SDU.Data & To_Unbounded_String("|CHK: " & Integer'Image(Hash));
   end Protect_SDU;

   --adding bundle protocol
   function Bundle_To_SDU(B: Bundle) return SDU_T is
      Combined : Unbounded_String := 
                                    To_Unbounded_String(B.Src_EID) & "|" &
                                    To_Unbounded_string(B.Dst_EID) & "|" & 
                                    To_Unbounded_String(B.Payload);
   begin
      return Delimit_SDU(Combined);
   end Bundle_To_SDU;

   --getting sdu from bundle 
   function SDU_To_Bundle(S : SDU_T) return Bundle is
      Data_Str : String := To_String(S.Data);
      Sep1, Sep2 : Natural;
      B : Bundle;
   begin
      Sep1 := Index(Data_Str, "|");
      Sep2 := Index(Data_Str, "|", From => Sep1 +1);
      B.Src_EID(1..Sep1 -1) := Data_Str(1 .. Sep1-1);
      B.Dst_EID(1 ..Sep2 - Sep1-1) := Data_Str(Sep1 + 1 .. Sep2 -1);
      B.Payload(1 ..Data_Str'Length -Sep2) := Data_Str(Sep2+1 .. Data_Str'Length);
      return B;
   end SDU_To_Bundle;

end IPC_Data_Transfer;