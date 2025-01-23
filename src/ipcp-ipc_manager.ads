with Ada.Text_IO; use Ada.Text_IO;
with Ada.Containers.Vectors;
with IPCP;
with Rina;

package IPCP.IPC_Manager is

   package IPCP_Vector is new Ada.Containers.Vectors(Index_Type => Positive, Element_Typer => IPCP.IPCP.T);

   type IPCP_List is new IPCP_Vector.Vector;

   procedure Initialize_Manager(Manager : in out IPCP_List);

   procedure Add_IPCP(Manager : in out IPCP_List; IPCP_Instance : IPCP.IPCP_T);

   procedure Remove_IPCP(Manager : in out IPCP_List; ID : Integer);

   function Locate_IPCP(Manager : IPCP_List; ID : Integer) return IPCP.IPCP_T;

   function Find_DIF_Address(Manager : IPCP_List; DIF_ID : Integer) return RINA.Address_T;

   procedure Display_All_IPCPs(Manager : IPCP_List);

end IPCP.IPC_Manager;