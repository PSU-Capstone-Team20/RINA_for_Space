with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Containers.Vectors;
with Transport_Types; use Transport_Types;

package IPCP_Types is
   
   type IPCP_State is (Initialized, Active, Inactive, Disconnected);
   
   type Flow_Info_T is record
      Flow_ID       : Natural;
      Port_ID       : Natural;
      QoS_ID        : Natural;
      Remote_CEP_ID : Unbounded_String;
   end record;

   package Flow_Vector is new Ada.Containers.Vectors (Index_Type => Natural, Element_Type => Flow_Info_T);
   subtype Flow_List is Flow_Vector.Vector;

   package PDU_Vector is new Ada.Containers.Vectors
     (Index_Type => Natural, Element_Type => Transport_Types.PDU_T);
   subtype PDU_Buffer is PDU_Vector.Vector;

   type IPCP_T is tagged record
      Name                 : Unbounded_String;
      Address              : Unbounded_String;
      State                : IPCP_State := Initialized;
      Connected_Computer   : Unbounded_String;
      Outgoing_PDUs        : PDU_Buffer;
      Incoming_PDUs        : PDU_Buffer;
      Active_Flows         : Flow_List;  
   end record;

   type IPCP_Access is access all IPCP_T;

end IPCP_Types;
