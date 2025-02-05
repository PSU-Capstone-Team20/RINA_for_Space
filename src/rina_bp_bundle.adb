with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Streams; use Ada.Streams;
with Ada.Streams.Stream_IO; use Ada.Streams.Stream_IO;
--with Ada.Stream_IO; use Ada.Stream_IO;


package body Rina_BP_Bundle is

   --create bundle
   function Create_Bundle(Version : Natural; Processing_Flag : Natural; Block_Length : Natural; Src_EID : String; Dst_EID : String; Payload : String) return Bundle is
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
      
   begin
      return Create_Bundle(Version => 6, Processing_Flag => 2, Block_Length => 512, Src_EID => "Mars Observer", Dst_EID => "Ground Station 1", Payload => "Water found in region 5");
   end Receive_Bundle;

   --  --Alire does not support Stream_IO.Write for direct writing, function for converting integer
   --  --to a stream_element_array
   --  function To_Stream_Element_Array(Value : Integer) return Ada.Streams.Stream_Element_Array is 
   --     subtype Byte_Array is Ada.Streams.Stream_Element_Array(1 .. Integer'Size / 8);
   --     Temp : Byte_Array;
   --  begin
   --     for I in Temp'Range loop
   --        Temp(I) := Ada.Streams.Stream_Element(Value / (2 **((I-1)* 8)) mod 256);
   --     end loop;
   --     return Temp;
   --  end To_Stream_Element_Array;

   --  --convert back to integer
   --  function To_Integer(Buffer : Ada.Streams.Stream_Element_Array) return Integer is
   --     Result : Integer := 0;
   --  begin 
   --     for I in Buffer'Range loop
   --        Result := Result + Integer(Buffer(I) * (2 ** (I-1)* 8));
   --     end loop;
   --     return Result;
   --  end To_Integer;

   --  --stream based serialization of bundle 
   --  procedure Serial_Bundle(Stream : in out Stream_IO.File_Type; B : in Bundle) is
   --  begin
      
   --     Stream_IO.Write(Stream, To_Stream_Element_Array(B.Header.Version));
   --     Stream_IO.Write(Stream, To_Stream_Element_Array(B.Header.Processing_Flag));
   --     Stream_IO.Write(Stream, To_Stream_Element_Array(B.Header.Block_Length));
   --     Stream_IO.Write(Stream, To_Stream_Element_Array(Integer'Length(B.Src_EID)));
   --     Stream_IO.Write(Stream, B.Src_EID);
   --     Stream_IO.Write(Stream, To_Stream_Element_Array(Integer'Length(B.Dst_EID)));
   --     Stream_IO.Write(Stream, B.Dst_EID);
   --     Stream_IO.Write(Stream, To_Stream_Element_Array(Integer'Length(B.Payload)));
   --     Stream_IO.Write(Stream, B.Payload);
   --  end Serial_Bundle;

   --  --deserialize bundle 
   --  function Deserial_Bundle(Stream : in out Stream_IO.File_Type) return Bundle is
   --     B : Bundle;
   --     Length : Integer;
   --  begin 
   --     Stream_IO.Read(Stream, B.Header.Version);
   --     Stream_IO.Read(Stream, B.Header.Processing_Flag);
   --     Stream_IO.Read(Stream, B.Header.Block_Length);
   --     Stream_IO.Read(Stream, Length);
   --     Stream_IO.Read(Stream, B.Src_EID, B.Src_EID'Last);
   --     Stream_IO.Read(Stream, Length);
   --     Stream_IO.Read(Stream, B.Dst_EID, B.Dst_EID'Last);
   --     Stream_IO.Read(Stream, Length);
   --     Stream_IO.Read(Stream, B.Payload, B.Payload'Last);
   --     return B;
   --  end Deserial_Bundle;

   --  --sending bundle using stream 
   --  procedure Send_Bundle(B : in Bundle) is 
   --     Stream : Stream_IO.File_Type;
   --  begin
   --     Stream_IO.Create(Stream, Stream_IO.Out_File, "bundle.dat");
   --     Serial_Bundle(Stream, B);
   --     Stream_IO.Close(Stream);
   --     Put_Line("Serialized Bundle sent");
   --  end Send_Bundle;

   --  --receive bundle using stream
   --  function Receive_Bundle return Bundle is 
   --     Stream : Stream_IO.File_Type;
   --  begin
   --     Stream_IO.Open(Stream, Stream_IO.In_File, "bundle.dat");
   --     declare
   --        B: Bundle := Deserial_Bundle(Stream);
   --     begin
   --        Stream_IO.Close(Stream);
   --        return B;
   --     end;
   --  end Receive_Bundle;
   
end Rina_BP_Bundle;