with IPC_Manager;
with IPCP_Types; use IPCP_Types;

package IPC_API is

   -- Exceptions for Error Handling
   IPC_Error : exception;
   Allocation_Error : exception;
   Deallocation_Error : exception;

   procedure Allocate_Flow(Manager : in out IPC_Manager.IPCP_Manager_T; 
                           Name : Unbounded_String;
                           Src_CEP_ID : String; 
                           QoS : Natural; 
                           Flow_Handle : out Flow_Info_T);
   
   procedure Send(Manager : in out IPC_Manager.IPCP_Manager_T; 
                  Name : Unbounded_String; 
                  Flow_Handle : Flow_Info_T;
                  Data : String);

   procedure Receive(Manager : in out IPC_Manager.IPCP_Manager_T; 
                     Name : Unbounded_String; 
                     Flow_Handle : Flow_Info_T;
                     Data : out String);

   procedure Deallocate_Flow(Manager : in out IPC_Manager.IPCP_Manager_T; 
                             Name : Unbounded_String; 
                             Flow_Handle : Flow_Info_T);

end IPC_API;
