with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Containers; use Ada.Containers;
with IPC_Data_Transfer; use IPC_Data_Transfer;
with Transport_Types; use Transport_Types;
with IPCP_Types; use IPCP_Types;
with RINA; use RINA;
with RIB; 
with Ada.Calendar; use Ada.Calendar;
with EFCP; use EFCP;
with Ada.Calendar.Formatting;

package body Rina_BP_Bundle is

   --create bundle function with all the information: header, source/destination, payload, and path
   function Create_Bundle(Version : Natural; 
                          Processing_Flag : Integer; 
                          Block_Length : Integer; 
                          Src_EID : PDU_S_T; 
                          Dst_EID : PDU_S_T; 
                          Payload : String;
                          Path    : Path_Vectors.Vector) return Bundle is
      --variable B of type Bundle to construct and return values
      B : Bundle;
   begin
      B.Header.Version := Version;
      B.Header.Processing_Flag := Processing_Flag;
      B.Header.Block_Length := Block_Length;
      B.Src_EID := Src_EID;
      B.Dst_EID := Dst_EID;
      --converts each character in payload string to ASCII value and stores into payload array
      for I in Payload'Range loop
         B.Payload(I) := Character'Pos(Payload(I));
      end loop;
      B.Path := Path;
      return B;
   end Create_Bundle;

   --procedure for sending bundle without the streams util
    procedure Send_Bundle(B : in Bundle) is
      Now : Time := Clock;
      P : PDU_S_T;
      Payload_To_String : String(1 .. B.Header.Block_Length);
      
   begin
      --due to payload being in byte array form need to adjust by index 
      for I in 1 .. B.Header.Block_Length loop
         Payload_To_String(I) := Character'Val(B.Payload(I));
      end loop;

      Put_Line("---- Sending Bundle ----");
      Put_Line("Source: " & To_String(B.Src_EID.PCI.Src_CEP_ID));
      Put_Line("Destination: " & To_String(B.Dst_EID.PCI.Dst_CEP_ID));
      Put_Line("Payload: " & Payload_To_String);
      Put_Line("Timestamp: " & Ada.Calendar.Formatting.Image(Now));
   
      P.Data := To_Unbounded_String(Payload_To_String);

      Put_Line("Bundle sent successfully");
   end Send_Bundle;

   --function for receiving bundle: receiving array of PDUs and reassemble into single byte array 
   procedure Receive_Bundle(PDUs : in out PDU_List; Reassemble : out Byte_Array) is
      --holding total length of data frags to get size of final output buffer 
      Total_Length : Natural := 0;
      
   begin
      --loop for each received PDU
      for I in PDUs'Range loop
         --DTCP procedure is applied here 
         DTCP(PDUs(I));
         declare
            --converts data payload of PDU from unbounded string to string 
            Temp_String : constant String := To_String(PDUs(I).Data);
         begin
            --takes total length of all PDU data 
            Total_Length := Total_Length + Temp_String'Length;
         end;
      end loop;
      --final Byte_Array to hold the reassembled payload is initialized here 
      Reassemble := (1 .. Total_Length => 0);

      declare
         --for tracking the insertion points into reassumbled byte array 
         Offset : Positive := 1;
         
      begin
          --for iterating through each PDU to copy data into final byte array 
         for I in PDUs'Range loop
            declare
               --converting unbounded string data to string
               Temp_String : constant String := To_String(PDUS(I).Data);
            begin
               --iterating through the characters in string 
               for J in Temp_String'Range loop
                  --converting each character to ASCII value and place into final byte array 
                  Reassemble(Offset) := Character'Pos(Temp_String(J));
                  --insertion point moved forward 
                  Offset := Offset + 1;
               end loop;
            end;
         end loop;
      end;

      
   end Receive_Bundle;


   
end Rina_BP_Bundle;