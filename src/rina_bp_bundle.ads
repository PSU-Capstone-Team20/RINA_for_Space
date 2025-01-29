with Rina;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Unbounded;
with Ada.Calendar; use Ada.Calendar;
with Ada.Streams; use Ada.Streams;

package Rina_BP_Bundle is

   --bunde ID structure
   type Bundle_ID is record
      Source_Name         : Rina.Address_T;
      Destination_Name    : Rina.Address_T;
      Creation_Time       : Ada.Calendar.Time;
      Sequence_Num        : Natural;
   end record;

   --bundle structure
   type Bundle is record 
      Primary_Block     : Rina.PCI_T(255);
      Payload_Block     : Rina.SDU_T;
      Extension_Blocks   : Stream_Element_Array(1 .. 1024); 
   end record;

   --create a bundle function
   function Create_Bundle (Source_Name : Rina.Address_T; Destination_Name : Rina.Address_T; Payload : Rina.SDU_T) return Bundle;

   --serializing: encode bundle into a stream 
   procedure Serial_Bundle (B : in Bundle; Stream : in out Root_Stream_Type'Class);

   --de-serializing: decoding bundle from a stream
   procedure Deserial_Bundle (Stream : in out Root_Stream_Type'Class; B : out Bundle);

   --validation function for bundle 
   function Validate_Bundle (B : Bundle) return Boolean;

   --function for extracting bundle ID from bundle 
   function Get_Bundle_ID(B : Bundle) return Bundle_ID;

end Rina_BP_Bundle;
