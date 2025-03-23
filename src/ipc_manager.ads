with Ada.Containers.Vectors;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with IPC_Manager.IPCP; use IPC_Manager.IPCP;

package IPC_Manager is
   type IPCP_Access is private;

   package IPCP_Vectors is new Ada.Containers.Vectors
   (Index_Type => Natural, Element_Type => IPCP_Access);
   subtype IPCP_Vector is IPCP_Vectors.Vector;

   type IPCP_Manager_T is tagged record
      Name           : Unbounded_String;
      Managed_IPCPs  : IPCP_Vector;
   end record;

   -- Creates an IPCP and adds it to the manager
   procedure Create_IPCP(Name : Unbounded_String; Manager : in out IPCP_Manager_T);

   -- Lists all IPCPs managed
   procedure List_IPCPs(Manager : IPCP_Manager_T);

   -- gets IPCP at specified index
   --  function Get_IPCP(Manager : IPCP_Manager_T; index : Integer) return IPCP_Access;

   -- Finds an IPCP by ID
   function Find_IPCP(Manager : IPCP_Manager_T; Name : Unbounded_String) return IPCP_Access;

   -- Deletes an IPCP from the manager
   procedure Delete_IPCP(Manager : in out IPCP_Manager_T; Name : Unbounded_String);

private
   -- 
   type IPCP_T is limited private;
   type IPCP_Access is access all IPCP_T; -- Forward declare private

end IPC_Manager;
