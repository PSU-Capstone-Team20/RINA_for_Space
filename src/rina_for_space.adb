with Rina;
-- with dif;
-- with ipcp;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
--with Ada.Integer_Text_IO;
with Ada.Text_IO; use Ada.Text_IO;
with Rina_BP_Bundle;
with Ada.Streams.Stream_IO;
with Ada.Calendar;

procedure Rina_For_Space is


   --test1 : dif.DIF_Vector;
   --num : Integer;
   --choice : Character;
   -- Flow		: RINA_Policies.Flow_ID := 1;
   -- QoS		: RINA_Policies.QoS_Parameter := (
   -- Priority => 1,
   -- Latency => 100,
   -- Throughput => 500,
   -- QoS_ID => 42);
   --test : SDNV (1 .. 5);

   --testing for bundle 
   Test_Payload : Rina.SDU_T := (Data_Length => 15, Data_Payload => "Water has been found in zone 40");

   Test_Bundle : Rina_BP_Bundle.Bundle; 
   Deserial_Bundle : Rina_BP_Bundle.Bundle;
   Bundle_ID : Rina_BP_Bundle.Bundle_ID;

   Stream : Ada.Streams.Stream_IO.File_Type;

begin

   --let's try making a bundle 
   Test_Bundle := Rina_BP_Bundle.Create_Bundle (Source_Name => (DIF_ID => 1, APN => "Mars Observer"), Destination_Name => (DIF_ID => 2, APN => "Ground Station 1"), Payload => Test_Payload);
   Put_Line("Bundle Creation Processing: ");
   Put_Line(" Source is: " & Test_Bundle.Primary_Block.Src_Endpoint.APN);
   Put_Line(" Destination is: " & Test_Bundle.Primary_Block.Dst_Endpoint.APN);
   Put_Line(" Payload: " & Test_Bundle.Payload_Block.Data_Payload(1 .. Test_Bundle.Payload_Block.Data_Length));

   Ada.Streams.Stream_IO.Create(Stream, Name => "test.dat", Mode => Ada.Streams.Stream_IO.Out_File);
   Rina_BP_Bundle.Serial_Bundle(Test_Bundle, Stream'Access);

   Ada.Streams.Stream_IO.Close(Stream);

   Ada.Streams.Stream_IO.Open(Stream, Name => "test.dat", Mode => Ada.Streams.Stream_IO.In_File);
   Rina_BP_Bundle.Deserial_Bundle(Stream'Access, Deserial_Bundle);
   Ada.Streams.Stream_IO.Close(Stream);

   Put_Line("De-Serialized Bundle: ");
   Put_Line(" Source: " & Deserial_Bundle.Primary_Block.Src_Endpoint.APN);
   Put_Line(" Destination: " & Deserial_Bundle.Primary_Block.Dst_Endpoint.APN);
   Put_Line(" Payload is: " & Deserial_Bundle.Payload_Block.Data_Payload(1 .. Deserial_Bundle.Payload_Block.Data_Length));

   if Rina_BP_Bundle.Validate_Bundle(Deserial_Bundle) then
      Put_Line("Valid Bundle");
   else
      Put_Line("Invalid Bundle");
   end if;

   --get bundle ID
   Bundle_ID := Rina_BP_Bundle.Get_Bundle_ID(Deserial_Bundle);
   Put_Line("Bundle ID is: ");
   Put_Line(" Source: " & Bundle_ID.Source_Name);
   Put_Line(" Destination: " & Bundle_ID.Destination_Name);
   Put_Line(" Creation Time: " & Ada.Calendar.Image(Bundle_ID.Creation_Time));
   Put_Line(" Sequence Number: " & Integer'Image(Bundle_ID.Sequence_Num));





   
   --test := RINA_Policies.Encode_SDNV(1420);
   --Put_Line (test'Image);
   --Put_Line(RINA_Policies.Decode_SDNV(test)'Image);
end Rina_For_Space;

