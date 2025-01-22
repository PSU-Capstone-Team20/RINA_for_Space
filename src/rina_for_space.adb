with Rina;
-- with dif;
-- with ipcp;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
--with Ada.Integer_Text_IO;
with Ada.Text_IO; use Ada.Text_IO;
with RINA_Policies; use RINA_Policies;
-- with RINA_Policies.DTN_Bundle_Protocol; 

procedure Rina_For_Space is
   Flow_ID  : RINA_Policies.Flow_ID := 1;
   QoS      : Rina.QoS_Parameter := (Priority   => 1,
                                              Latency    =>  100,
                                              Throughput => 500);
   
   Source_Endpoint : Rina.Endpoint_ID := (
      App_Process_Name => To_Unbounded_String("Mars Observer"),
      ID               => 101);
   
   Destination_Endpoint : Rina.Endpoint_ID := (
      App_Process_Name => To_Unbounded_String("Ground Station"),
      ID               => 202);
   
   Data_Unit : Rina.Data_Unit_T(Header_Length => 120);

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

begin
   --create the data unit 
   Put_Line(" Creating Data Unit");
   Create_Data_Unit(Flow_ID, QoS, Source_Endpoint, Destination_Endpoint, Data_Unit);

   --for control info
   Put_Line("Control Information: ");
   Put_Line(" Source Address: " & To_String(Data_Unit.Control_Info.Src_Endpoint.App_Process_Name));
   Put_Line(" Destination Address: " & To_String(Data_Unit.Control_Info.Dst_Endpoint.App_Process_Name));
   Put_Line(" QoS: Priority:" & Integer'Image(Data_Unit.Control_Info.QoS.Priority) &
            ", Latency:" & Integer'Image(Data_Unit.Control_Info.QoS.Latency) &
            ", Throughput:" & Integer'Image(Data_Unit.Control_Info.QoS.Throughput));
   Put_Line(" PDU Type: " & Data_Unit.Control_Info.PDU_Type);

   --process data
   Put_Line("Processing Data...");
   Process_Data_Unit(Data_Unit);
   Put_Line("Updated Sequence Number is " & Integer'Image(Data_Unit.Control_Info.Seq_Num));

   --transmit the data 
   Put_Line("Transmitting data...");
   Transmit_Data_Unit(Flow_ID, Data_Unit);

   --encode and decode test 
   Put_Line("SDNV Encoding...");
   declare
      test_SDNV : SDNV(1 .. 5);
      Decoded_Val : Integer;
   begin
      test_SDNV := Encode_SDNV(1420);
      Put_Line("Encoded SDNV: " & test_SDNV'Image);
      Decoded_Val := Decode_SDNV(test_SDNV);
      Put_Line("Decdoed SDNV: " & Integer'Image(Decoded_Val));
   end;

   Put_Line("Test complete");




   
   --test := RINA_Policies.Encode_SDNV(1420);
   --Put_Line (test'Image);
   --Put_Line(RINA_Policies.Decode_SDNV(test)'Image);
end Rina_For_Space;

