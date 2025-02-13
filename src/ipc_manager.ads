with Ada.Containers.Vectors;
with Ada.Calendar;
with Ada.Text_IO; use Ada.Text_IO;
with IPCP; use IPCP;
with DIF; use DIF;
with Flow_Manager; use Flow_Manager;
with Resource_Manager; use Resource_Manager;
with CDAP; use CDAP;
with Security; use Security;

package IPC_Manager is

   -- IPC Manager type 
   type IPCM_T is tagged record
      IPCP_List : IPCP_Vector.Vector;
   end record;

   -- IPCP Management Operations
   procedure Initialize_Manager(Manager : in out IPCM_T);
   procedure Create_IPCP(Manager : in out IPCM_T; ID : IPCP_ID; Name : String; Address : String; QoS_Params : String);
   procedure Activate_IPCP(Manager : in out IPCM_T; ID : IPCP_ID);
   procedure Terminate_IPCP(Manager : in out IPCM_T; ID : IPCP_ID);

   procedure List_All_IPCPs(Manager : IPCM_T);

   -- Flow Management
   procedure Allocate_Flow(Manager : in out IPCM_T; Src : String; Dst : String);
   procedure Deallocate_Flow(Manager : in out IPCM_T; Flow_ID : Integer);

   -- Resource Management
   procedure Monitor_Resources(Manager : IPCM_T);

   -- CDAP Integration
   procedure Handle_CDAP_Request(Manager : in out IPCM_T; Object_Name : String; Op : Operation_Type);

end IPC_Manager;
