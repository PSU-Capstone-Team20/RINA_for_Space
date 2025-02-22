with IPCP; use IPCP;
with Ada.Containers.Vectors;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package IPC_Manager is

   package IPCP_Vectors is new Ada.Containers.Vectors
   (Index_Type => Natural, Element_Type => IPCP_Access);
   subtype IPCP_Vector is IPCP_Vectors.Vector;

   type IPCP_Manager_T is tagged record
      Managed_IPCPs : IPCP_Vector;
   end record;

   -- Creates an IPCP and adds it to the manager
   procedure Create_IPCP(Name : Unbounded_String; ID : Unbounded_String; Manager : in out IPCP_Manager_T);

   -- Lists all IPCPs managed
   procedure List_IPCPs(Manager : IPCP_Manager_T);

   -- Assigns a PDU to an IPCP instance (avoids circular dependency)
   procedure Assign_PDU(IPCP_Instance : IPCP_Access; PDU : PDU_T);

   -- Finds an IPCP by ID
   function Find_IPCP(Manager : IPCP_Manager_T; ID : Unbounded_String) return IPCP_Access;

end IPC_Manager;
