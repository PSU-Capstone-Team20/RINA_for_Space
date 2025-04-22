with Ada.Text_IO; use Ada.Text_IO;
<<<<<<< Updated upstream
-- with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Streams; use Ada.Streams;
with Ada.Streams.Stream_IO; use Ada.Streams.Stream_IO;
--with Ada.Stream_IO; use Ada.Stream_IO;

=======
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
>>>>>>> Stashed changes

package body Rina_BP_Bundle is

   --create bundle
   function Create_Bundle(Version : Natural; 
                          Processing_Flag : Integer; 
                          Block_Length : Integer; 
<<<<<<< Updated upstream
                          Src_EID : String; 
                          Dst_EID : String; 
                          Payload : String) return Bundle is
=======
                          Src_EID : PDU_S_T; 
                          Dst_EID : PDU_S_T; 
                          Payload : String;
                          Path    : Path_Vectors.Vector) return Bundle is
      --variable B of type Bundle to construct and return values
>>>>>>> Stashed changes
      B : Bundle;
   begin
      B.Header.Version := Version;
      B.Header.Processing_Flag := Processing_Flag;
      B.Header.Block_Length := Block_Length;
<<<<<<< Updated upstream
      B.Src_EID(1 .. Src_EID'Length) := Src_EID;
      B.Dst_EID(1 .. Dst_EID'Length) := Dst_EID;
      B.Payload(1 .. Payload'Length) := Payload;
=======
      B.Src_EID := Src_EID;
      B.Dst_EID := Dst_EID;
      --converts each character in payload string to ASCII value and stores into payload array
      for I in Payload'Range loop
         B.Payload(I) := Character'Pos(Payload(I));
      end loop;
      B.Path := Path;
>>>>>>> Stashed changes
      return B;
   end Create_Bundle;

   --procedure for sending bundle without the streams util
<<<<<<< Updated upstream
   procedure Send_Bundle(B : in Bundle) is 
   begin
=======
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

      --Control_Transmit(P);
      --Relay_PDU(P);
      


>>>>>>> Stashed changes
      Put_Line("Bundle sent successfully");
   end Send_Bundle;

   --function for receiving bundle 
   function Receive_Bundle return Bundle is
      B: Bundle;
   begin
<<<<<<< Updated upstream
      return B;
=======
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
         Offset : Natural := 1;
         
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

      
>>>>>>> Stashed changes
   end Receive_Bundle;


   
end Rina_BP_Bundle;