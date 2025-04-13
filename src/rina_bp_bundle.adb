with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Containers; use Ada.Containers;
with IPC_Data_Transfer; use IPC_Data_Transfer;
with Transport_Types; use Transport_Types;
with IPCP_Types; use IPCP_Types;
with RINA; use RINA;
with RIB; 

package body Rina_BP_Bundle is

   --create bundle
   function Create_Bundle(Version : Natural; 
                          Processing_Flag : Integer; 
                          Block_Length : Integer; 
                          Src_EID : String; 
                          Dst_EID : String; 
                          Payload : String;
                          Path    : Path_Vectors.Vector) return Bundle is
      B : Bundle;
   begin
      B.Header.Version := Version;
      B.Header.Processing_Flag := Processing_Flag;
      B.Header.Block_Length := Block_Length;
      B.Src_EID(1 .. Src_EID'Length) := Src_EID;
      B.Dst_EID(1 .. Dst_EID'Length) := Dst_EID;
      for I in Payload'Range loop
         B.Payload(I) := Character'Pos(Payload(I));
      end loop;
      B.Path := Path;
      return B;
   end Create_Bundle;

   --procedure for sending bundle without the streams util
   procedure Send_Bundle(B : in Bundle) is
      SDU : Byte_Array(B.Payload'Range);
      PDUs : PDU_List(1 .. 10);
      Flow : Flow_ID;
   begin
      Delimit_SDU(B.Payload, SDU);
      DTP(SDU => SDU, Fragment_PDU => PDUs);

      for I in PDUs'Range loop
         if B.Path.Length > 0 then 
            PDUs(I).PCI.Dst_CEP_ID := B.Path.Element(0).Element(0).Name;
         end if;
         DTCP(PDUs(I));
         Multiplex_PDU(PDUs(I), Flow);
         Relay_PDU(PDUs(I));
      end loop;
      


      Put_Line("[Send_Bundle] Bundle sent successfully");
   end Send_Bundle;

   --function for receiving bundle 
   procedure Receive_Bundle(PDUs : in out PDU_List; Reassemble : out Byte_Array) is
      Total_Length : Natural := 0;
      
   begin
      for I in PDUs'Range loop
         DTCP(PDUs(I));
         declare
            Temp_String : constant String := To_String(PDUs(I).Data);
         begin
            Total_Length := Total_Length + Temp_String'Length;
         end;
      end loop;
      Reassemble := (1 .. Total_Length => 0);

      declare
         Offset : Positive := 1;
         
      begin
          
         for I in PDUs'Range loop
            declare
               Temp_String : constant String := To_String(PDUS(I).Data);
            begin
               for J in Temp_String'Range loop
                  Reassemble(Offset) := Character'Pos(Temp_String(J));
                  Offset := Offset + 1;
               end loop;
            end;
         end loop;
      end;

      
   end Receive_Bundle;


   
end Rina_BP_Bundle;