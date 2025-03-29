with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with IPCP_Types;
with Transport_Types; use Transport_Types;
with IPC_Manager; use IPC_Manager;

package IPC_API is

   IPC_Error : exception;

   -- Allocates a new flow from a source IPCP
   procedure Allocate_Flow(
      Manager     : in out IPCP_Manager_T;
      Name        : Unbounded_String;
      Src_CEP_ID  : Unbounded_String;
      QoS         : Natural;
      Flow_Handle : out IPCP_Types.Flow_Info_T
   );

   -- Sends a message over an existing flow
   procedure Send(
      Manager     : in out IPCP_Manager_T;
      Name        : Unbounded_String;
      Flow_Handle : IPCP_Types.Flow_Info_T;
      Data        : Unbounded_String
   );

   -- Receives a message from an existing flow
   procedure Receive(
      Manager     : in out IPCP_Manager_T;
      Name        : Unbounded_String;
      Flow_Handle : IPCP_Types.Flow_Info_T;
      Data        : out Unbounded_String
   );

   -- Deallocates a flow by its handle
   procedure Deallocate_Flow(
      Manager     : in out IPCP_Manager_T;
      Name        : Unbounded_String;
      Flow_Handle : IPCP_Types.Flow_Info_T
   );

end IPC_API;
