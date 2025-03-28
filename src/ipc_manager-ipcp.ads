with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Containers.Vectors;
with Ada.Calendar; use Ada.Calendar;
with Transport_Types; use Transport_Types;
with IPCP_Types; use IPCP_Types;

package IPC_Manager.IPCP is
   -- Creates a new IPCP instance
   function Make_IPCP(Name : Unbounded_String) return IPCP_T;

   -- IPCP Flow Management
   --  procedure Add_Flow(IPCP : in out IPCP_T; Flow : Flow_Info_T);
   --  procedure Remove_Flow(IPCP : in out IPCP_T; Flow_ID : Natural);
   --  function Find_Flow(IPCP : IPCP_T; Flow_ID : Natural) return Flow_Info_T;
   --  procedure Update_Flow_Priority(IPCP : in out IPCP_T; Flow_ID : Natural; New_Priority : Priority_Lvl);

   -- IPCP PDU Buffer Management
   procedure Assign_PDU(IPCP_Instance : in out IPCP_T; PDU : PDU_T; To_Outgoing : Boolean := True);
   function Get_PDU(IPCP : in out IPCP_T; From_Outgoing : Boolean := True) return PDU_T;
   function Pop_PDU(IPCP : in out IPCP_T; From_Outgoing : Boolean := True) return PDU_T;
   procedure Clear_PDU_Buffer(IPCP : in out IPCP_T; Clear_Incoming : Boolean := True; Clear_Outgoing : Boolean := True);
   --procedure Process_PDU(IPCP : in out IPCP_T; PDU : in PDU_T);

end IPC_Manager.IPCP;
