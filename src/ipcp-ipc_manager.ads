-- IPC_Manager.ads

with Ada.Containers.Vectors;
with IPCP;

package IPC_Manager is

   package IPCP_Vector is new Ada.Containers.Vectors(Index_type => Natural, Element_Type => IPCP.IPCP_Record);

   -- IPC Manager Record
   type IPC_Manager_Record is record
      Managed_IPCPs : IPCP_Vector.Vector;
   end record;

   -- IPC Manager Operations
   procedure Add_IPCP(Manager : in out IPC_Manager_Record; IPCP_Instance : IPCP.IPCP_Record);
   procedure Remove_IPCP(Manager : in out IPC_Manager_Record; IPCP_ID : IPCP.IPCP_ID);
   procedure Get_IPCP_Info(Manager : IPC_Manager_Record; IPCP_ID : IPCP.IPCP_ID);
   procedure List_All_IPCPs(Manager : IPC_Manager_Record);
   procedure Monitor_IPCP_Status(Manager : in out IPC_Manager_Record);
   procedure Restart_IPCP(Manager : in out IPC_Manager_Record; IPCP_ID : IPCP.IPCP_ID);

   -- Communication Management
   procedure Retry_All_Failed_Communications(Manager : in out IPC_Manager_Record); -- handles multiple IPCP instances
   procedure Transmit_All_Stored_Data(Manager : in out IPC_Manager_Record);

   -- Policy Management
   -- TO-DO: Need to confirm whether if we should set Global policy in IPC Manager
   -- Should we have flow-Specific and application-specific policies Managed by IPCP and IPC API?
   --  procedure Set_Policy(Manager : in out IPC_Manager_Record; Policy_Name : String; Policy_Value : String);
   --  procedure Get_Policy(Manager : IPC_Manager_Record; Policy_Name : String);

end IPC_Manager;
