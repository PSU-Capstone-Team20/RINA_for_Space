with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Streams; use Ada.Streams;
with Rina;

package body Rina_BP_Bundle is

   --bundle creation function 
   function Create_Bundle (Source_Name : Rina.Address_T; Destination_Name : Rina.Address_T; Payload : Rina.SDU_T) return Bundle is
      New_Bundle : Bundle;
   begin
      New_Bundle.Primary_Block.Src_Endpoint := Source_Name;
      New_Bundle.Primary_Block.Dst_Endpoint := Destination_Name;
      New_Bundle.Primary_Block.Seq_Num := 1;
      New_Bundle.Primary_Block.QoS.Priority := 1;
      New_Bundle.Payload_Block := Payload;
      New_Bundle.Extension_Blocks := (1 .. 1024 => 0); 
      return New_Bundle;
   end Create_Bundle;

   --serializing bundle into a stream 
   procedure Serial_Bundle (B : in Bundle; Stream : in out Root_Stream_Type'Class) is
      Header_Buffer    : Stream_Element_Array(1 .. B.Primary_Block.Header'Length);
      Payload_Buffer   : Stream_Element_Array(1 .. B.Payload_Block.Data_Length);
      Extension_Buffer : Stream_Element_Array(1 .. B.Extension_Blocks'Length);
   begin
      -- need to copy data to buffer 
      for I in Header_Buffer'Range loop
         Header_Buffer(I) := Stream_Element(B.Primary_Block.Header(I));
      end loop;
      --write the primary block 
      Stream.Write (Header_Buffer);

      for I in Payload_Buffer'Range loop
         Payload_Buffer(I) := Stream_Element(Character'Pos(B.Payload_Block.Data_Payload(I)));
      end loop;
      --write payload 
      Stream.Write (Payload_Buffer);

      for I in Extension_Buffer'Range loop
         Extension_Buffer(I) := B.Extension_Blocks(I);
      end loop;
      --writing extension blocks
      Stream.Write (Extension_Buffer);
      
   end Serial_Bundle;

   --de-serialize stream 
   procedure Deserial_Bundle (Stream : in out Root_Stream_Type'Class; B : out Bundle) is
      Header_Buffer    : Stream_Element_Array(1 .. B.Primary_Block.Header'Length);
      Payload_Buffer   : Stream_Element_Array(1 .. B.Payload_Block.Data_Length);
      Extension_Buffer : Stream_Element_Array(1 .. B.Extension_Blocks'Length);
      --Last             : Stream_Element_Offset; -- causing error "too many arguments in Read and Write"
   begin
      --read primary block 
      Stream.Read (Header_Buffer);
      for I in Header_Buffer'Range loop
         B.Primary_Block.Header(I) := Rina.U8_T(Header_Buffer(I));
      end loop;
      --reading payload block 
      Stream.Read (Payload_Buffer);
      for I in Payload_Buffer'Range loop
         B.Payload_Block.Data_Payload(I) := Character'Val(Payload_Buffer(I));
      end loop;
      --reading extension blocks 
      Stream.Read (Extension_Buffer);
      for I in Extension_Buffer'Range loop
         B.Extension_Blocks(I) := Extension_Buffer(I);
      end loop;
   end Deserial_Bundle;

   --validate the integrity of a bundle 
   function Validate_Bundle (B : Bundle) return Boolean is
   begin
      return B.Primary_Block.Length > 0 and B.Payload_Block.Data_Length > 0;
   end Validate_Bundle;

   --extract the bundle id from the bundle 
   function Get_Bundle_ID (B : Bundle) return Bundle_ID is
      ID : Bundle_ID;
   begin
      ID.Source_Name := B.Primary_Block.Src_Endpoint;
      ID.Destination_Name := B.Primary_Block.Dst_Endpoint;
      ID.Creation_Time := Clock;
      ID.Sequence_Num := B.Primary_Block.Seq_Num;
      return ID;
   end Get_Bundle_ID;

end Rina_BP_Bundle;