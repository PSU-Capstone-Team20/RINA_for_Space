with Ada.Text_IO; use Ada.Text_IO;
-- with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Streams; use Ada.Streams;
with Ada.Streams.Stream_IO; use Ada.Streams.Stream_IO;
--with Ada.Stream_IO; use Ada.Stream_IO;


package body Rina_BP_Bundle is

   --create bundle
   function Create_Bundle(Version : Natural; 
                          Processing_Flag : Integer; 
                          Block_Length : Integer; 
                          Src_EID : String; 
                          Dst_EID : String; 
                          Payload : String) return Bundle is
      B : Bundle;
   begin
      B.Header.Version := Version;
      B.Header.Processing_Flag := Processing_Flag;
      B.Header.Block_Length := Block_Length;
      B.Src_EID(1 .. Src_EID'Length) := Src_EID;
      B.Dst_EID(1 .. Dst_EID'Length) := Dst_EID;
      B.Payload(1 .. Payload'Length) := Payload;
      return B;
   end Create_Bundle;

   --procedure for sending bundle without the streams util
   procedure Send_Bundle(B : in Bundle) is 
   begin
      Put_Line("Bundle sent successfully");
   end Send_Bundle;

   --function for receiving bundle 
   function Receive_Bundle return Bundle is
      B: Bundle;
   begin
      return B;
   end Receive_Bundle;


   
end Rina_BP_Bundle;