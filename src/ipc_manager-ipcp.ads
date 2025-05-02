with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Containers.Vectors;
with Ada.Calendar; use Ada.Calendar;
with Transport_Types; use Transport_Types;
with IPCP_Types; use IPCP_Types;

package IPC_Manager.IPCP is
   -- Creates a new IPCP instance
   function Make_IPCP(Name : Unbounded_String) return IPCP_T;

   -- Flow Management

   -- Adds a new flow record to the IPCP's active flow list
   procedure Add_Flow(IPCP : in out IPCP_T; Flow : Flow_Info_T);

   -- Removes a flow from the IPCP's active list by Flow_ID
   procedure Remove_Flow(IPCP : in out IPCP_T; Flow_ID : Natural);
   
   -- Checks if a flow with the given Flow_ID exists in the IPCP
   function Flow_Exists(IPCP : IPCP_T; Flow_ID : Natural) return Boolean;

   -- Buffer Management

   -- Assigns a PDU to the outgoing or incoming buffer of the IPCP instance
   procedure Assign_PDU(IPCP_Instance : in out IPCP_T; PDU : PDU_T; To_Outgoing : Boolean := True);

   -- Returns the first PDU from either the outgoing or incoming buffer (non-destructive)
   function Get_PDU(IPCP : in out IPCP_T; From_Outgoing : Boolean := True) return PDU_T;

   -- Returns and removes the first PDU from the selected buffer (destructive read)
   function Pop_PDU(IPCP : in out IPCP_T; From_Outgoing : Boolean := True) return PDU_T;

   -- Clears the incoming and/or outgoing PDU buffers of the IPCP
   procedure Clear_PDU_Buffer(IPCP : in out IPCP_T; Clear_Incoming : Boolean := True; Clear_Outgoing : Boolean := True);
   
   --this is here due to the current implementation not fully processing a PDU when it gets handled by an IPCP
   --fuller implementation will gather information from the PDU via this procedure for transmission, presently this is a stub
   --procedure Process_PDU(IPCP : in out IPCP_T; PDU : in PDU_T);

end IPC_Manager.IPCP;
