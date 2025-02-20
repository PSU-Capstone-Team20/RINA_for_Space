with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with IPCP; use IPCP;
with DIF_Manager; use DIF_Manager;
with IPC_Manager; use IPC_Manager;

package IPC_API is

   type Port_ID is new Natural;

   -- Allocates resources to support a flow between source and destination with a certain QoS
   function Allocate(Src_Application : Unbounded_String;
                      Dst_Application : Unbounded_String;
                      QoS             : Priority_Level;
                      DIF_M           : in out DIF_MANAGER_T;
                      IPC_M           : in out IPCP_Manager_T) return Port_ID;

   -- Sends data to the destination application on the specified port
   procedure Send(Port : Port_ID;
                   SDU  : String;
                   IPC_M : in out IPCP_Manager_T);

   -- Receives an SDU from the destination application on the specified port
   function Receive(Port : Port_ID;
                     IPC_M : in out IPCP_Manager_T) return String;

   -- Terminates the flow and frees all communication resources
   procedure Deallocate(Port : Port_ID;
                         IPC_M : in out IPCP_Manager_T);

end IPC_API;
