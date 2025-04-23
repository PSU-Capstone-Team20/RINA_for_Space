with Ada.Containers.Vectors;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with IPCP_Types; use IPCP_Types;
with Log; use Log;

-- IPC Manager Package
-- This package manages a list of IPCPs, creates/destroys them, offer IPCP lookup 
-- by name/ID, and response to flow allocation requests.
package IPC_Manager is

   package IPCP_Vectors is new Ada.Containers.Vectors
   (Index_Type => Natural, Element_Type => IPCP_Access);
   subtype IPCP_Vector is IPCP_Vectors.Vector;
   --not used
   type IPCP_Manager_T is tagged record
      Name           : Unbounded_String;
      Managed_IPCPs  : IPCP_Vector;
   end record;

   -- Creates an IPCP and adds it to the manager
    procedure Create_IPCP(
      Name               : Unbounded_String;
      --Address            : Unbounded_String;
      --Connected_Computer : Unbounded_String;
      Manager            : in out IPCP_Manager_T
   );

   -- Lists all IPCPs managed
   procedure List_IPCPs(Manager : IPCP_Manager_T);

   -- Finds an IPCP by ID
   function Find_IPCP(Manager : IPCP_Manager_T; Name : Unbounded_String) return IPCP_Access;

   -- Deletes an IPCP from the manager
   procedure Delete_IPCP(Manager : in out IPCP_Manager_T; Name : Unbounded_String);

   procedure Allocate_Flow(
      Manager          : in out IPCP_Manager_T;
      Src_Name         : Unbounded_String;
      Dst_Name         : Unbounded_String;
      Flow             : Flow_Info_T
   );


end IPC_Manager;
