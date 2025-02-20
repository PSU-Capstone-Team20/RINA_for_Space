with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
limited with DIF;
with Ada.Containers.Vectors;

package IPCP is
   type DIF_Access is access all DIF.DIF;

   type IPCP_State is (Initialized, Active, Inactive, Disconnected);
   type Priority_Level is (Low, Medium, High, Critical);

   package PDU_Vector is new Ada.Containers.Vectors
     (Index_Type => Natural, Element_Type => Unbounded_String);
   subtype PDU_Buffer is PDU_Vector.Vector;

   type IPCP_T is tagged record
      ID            : Unbounded_String;
      State         : IPCP_State := Initialized;
      Name          : Unbounded_String;
      Address       : Unbounded_String;
      QoS_Params    : Priority_Level := Medium;
      Connected_DIF : DIF_Access := null;  -- Optional DIF Connection
      PDUs          : PDU_Buffer; -- PDU Queue for storing pending transmissions
   end record;

   type IPCP_Access is access all IPCP_T;

   -- Creates a new IPCP instance
   function Create_IPCP(Name : Unbounded_String; ID : Unbounded_String) return IPCP_Access;

   -- Handles internal data flow within the IPCP
   procedure Process_PDU(IPCP : in out IPCP_T; PDU : in Unbounded_String);

end IPCP;
