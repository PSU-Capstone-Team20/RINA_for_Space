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

   -- Centralized method to connect an IPCP to a DIF
   --  procedure Connect_IPCP_to_DIF(IPCP_ID : Unbounded_String; DIF : in out DIF; Manager : in out IPCP_Manager_T);


end IPC_Manager;