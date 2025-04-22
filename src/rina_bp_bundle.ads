
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Calendar; use Ada.Calendar;
with Ada.Streams; use Ada.Streams;
with Ada.Streams.Stream_IO; use Ada.Streams.Stream_IO;
--with Ada.Stream_IO; use Ada.Stream_IO;
<<<<<<< Updated upstream
=======
with RINA; use RINA;
with RIB;
with IPC_Data_Transfer; use IPC_Data_Transfer;
with Transport_Types;
>>>>>>> Stashed changes

package Rina_BP_Bundle is

   --bunde ID structure
   type Bundle_Header is tagged record
      Version         : Natural := 6;
      Processing_Flag : Integer;
      Block_Length    : Integer;
   end record;

   --TODO: need to add APN instead of just src and destination EIDs

   subtype PDU_S_T is Transport_Types.PDU_T;

   --bundle structure
   type Bundle is tagged record
      Header       : Bundle_Header;
<<<<<<< Updated upstream
      Src_EID      : String(1 .. 1024); -- source endpoing ID 
      Dst_EID      : String(1 .. 1024); -- destination endpoint ID
      Payload      : String(1 .. 4096); -- payload
   end record;

   --create bundle function 
   function Create_Bundle(Version : Natural; Processing_Flag : Integer; Block_Length : Integer; Src_EID : String; Dst_EID : String; Payload : String) return Bundle;
=======
      Src_EID      : PDU_S_T; -- source endpoing ID 
      Dst_EID      : PDU_S_T; -- destination endpoint ID
      Payload      : Byte_Array(1 .. 4096); -- payload
      Path         : Path_Vectors.Vector;
   end record;

   --create bundle function 
   function Create_Bundle(Version : Natural; Processing_Flag : Integer; Block_Length : Integer; Src_EID : PDU_S_T; Dst_EID : PDU_S_T; Payload : String; Path : Path_Vectors.Vector) return Bundle;
>>>>>>> Stashed changes

   --sending bundle procedure 
   procedure Send_Bundle(B : in Bundle);

   --function for receiving bundle 
   function Receive_Bundle return Bundle; 
   
   --  --serializing : encode bundle into a stream
   --  procedure Serial_Bundle(Stream : in out Ada.Streams.Stream_IO.Stream_Access; B : in Bundle); 

   --  --de-serizliing: decode bundle from stream
   --  function Deserial_Bundle(Stream : in out Ada.Streams.Stream_IO.Stream_Access) return Bundle;

end Rina_BP_Bundle;
