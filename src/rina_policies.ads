with Rina;
with Ada.Strings.Unbounded;
with Interfaces;

package RINA_Policies is

   --for QoS parameters 
   type QoS_Parameter is record
      Priority     : Integer;
      Latency      : Integer;
      Throughput   : Integer;
   end record;

   --for endpoint ID
   type Endpoint_ID is record 
      App_Process_Name   : Ada.Strings.Unbounded.Unbounded_String;
      ID                 : Integer;
   end record;

   --flow ID
   type Flow_ID is range 1 .. 100;

   --SDNV type 
   type SDNV is array (Positive range <>) of Interfaces.Unsigned_8;

   --data unit
   type Data_Unit is record
      Flow         : Flow_ID;
      QoS          : Rina.QoS_Parameter;
      Source       : Rina.Endpoint_ID;
      Destination  : Rina.Endpoint_ID;
   end record;

   --encode integer into SDNV format
   function Encode_SDNV(Value : Integer) return SDNV;

   --decode SDNV back to int 
   function Decode_SDNV(SDNV_Value : SDNV) return Integer;

   --encode QoS parameters to SDNV 
   function Encode_QoS(QoS : Rina.QoS_Parameter) return SDNV;

   --decode QoS from SDNV
   function Decode_QoS(SDNV_Value : SDNV) return Rina.QoS_Parameter;

   --endcode EndpointID to SDNV
   function Encode_Endpoint_ID(Endpoint : Rina.Endpoint_ID) return SDNV;

   --decode endpointiD 
   function Decode_Endpoint_ID(SDNV_Value : SDNV) return Rina.Endpoint_ID;


   --for creating data unit 
   procedure Create_Data_Unit(Flow        : Flow_ID;
                              QoS         : Rina.QoS_Parameter;
                              Source      : Rina.Endpoint_ID;
                              Destination : Rina.Endpoint_ID;
                              Unit        : out Rina.Data_Unit_T);
   
   procedure Process_Data_Unit(Unit : in out Rina.Data_Unit_T);

   procedure Transmit_Data_Unit(Flow : Flow_ID; Unit : in Rina.Data_Unit_T);

   --encode data unit to SDNV
   function Encode_Data_Unit(Unit : Data_Unit) return SDNV;

   --decode a data unit from SDNV
   function Decode_Data_Unit(SDNV_Value : SDNV) return Data_Unit;

   --log data unit info
   procedure Log_Data_Unit(Unit : Data_Unit);


end RINA_Policies;
