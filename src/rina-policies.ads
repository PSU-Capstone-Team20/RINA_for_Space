package RINA.Policies is
   --flow and qos definitions
   type Flow_ID is new Integer;
   subtype Priority_Level is Positive;

   type QoS_Parameter is tagged record
      Priority   : Priority_Level;
      Latency    : Integer;
      Throughput : Integer;
      QoS_ID     : Rina.QoS_Id_T;
   end record;

   procedure Define_QoS (Flow : in Flow_ID; QoS : in QoS_Parameter);
   procedure Schedule_Flow (Flow : in Flow_ID);
   procedure Handle_Error (Flow : in Flow_ID; Error_Code : in Integer);
   procedure Relay_And_Forward
     (Source_Flow : in Flow_ID; Destination_Flow : in Flow_ID);

   procedure Create_Data_Unit
     (Flow : in Flow_ID; Data_Unit : out Rina.Data_Unit_T);

   procedure Process_Data_Unit (Data_Unit : in out Rina.Data_Unit_T);

   procedure Transmit_Data_Unit
     (Flow : in Flow_ID; Data_Unit : in Rina.Data_Unit_T);
end RINA.Policies;
