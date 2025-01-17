with Rina;

package RINA_Policies is
--flow and qos definitions
   type Flow_ID is range 1 .. 100;
   type QoS_Parameter is record
      Priority : Integer;
      Latency  : Integer; 
      Throughput : Integer;
   end record;

   type QoS_Table is array(Flow_ID range <>) of QoS_Parameter;

   type byte is mod 2**8;

   type SDNV is array (Positive range <>) of byte;

   --Function for encoding integer as an SDNV 
   function Encode_SDNV(Value : Integer) return SDNV;

   -- Function to decode an SDNV to an integer
   function Decode_SDNV(SDNV_Value : SDNV) return Integer;

   --QoS and Flow managing procedures
   procedure Define_QoS(Flow : in Flow_ID; QoS : in QoS_Parameter);
   procedure Schedule_Flow(Flow : in Flow_ID);
   procedure Handle_Error(Flow : in Flow_ID; Error_Code : in Integer);
   procedure Flow_Control(Flow : in Flow_ID; Max_Packets : in Positive);
   procedure Relay_And_Forward(Source_Flow : in Flow_ID; Destination_Flow : in Flow_ID);

   --data unit procedures
   procedure Create_Data_Unit(Flow : in Flow_ID; Data_Unit : out Rina.Data_Unit_T);
   procedure Process_Data_Unit(Data_Unit : in out Rina.Data_Unit_T);
   procedure Transmit_Data_Unit(Flow : in Flow_ID; Data_Unit : in Rina.Data_Unit_T);

end RINA_Policies;
