with IPC_Manager; use IPC_Manager;
with IPCP_Types; use IPCP_Types;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;

package body IPC_API is

   -- Procedure to allocate a new flow
   procedure Allocate_Flow(Manager : in out IPCP_Manager_T;
                          Name : Unbounded_String;
                          Src_CEP_ID : String;
                          QoS : Natural;
                          Flow_Handle : out Flow_Info_T) is
      IPCP_Instance : IPCP_Access := Find_IPCP(Manager, Name);
   begin
      if IPCP_Instance = null then
         raise IPC_Error with "IPCP instance not found.";
      end if;

      -- Create a new flow
      Flow_Handle := (Flow_ID       => IPCP_Instance.Active_Flows.Length + 1,
                     Port_ID       => 1, -- Placeholder
                     QoS_ID        => QoS,
                     Remote_CEP_ID => Src_CEP_ID);

      -- Add the flow to the active flows list
      IPCP_Instance.Active_Flows.Append(Flow_Handle);

   exception
      when others =>
         raise IPC_Error with "Error allocating flow.";
   end Allocate_Flow;

   -- Procedure to send data over a flow
   procedure Send(Manager : in out IPCP_Manager_T;
                 Name : Unbounded_String;
                 Flow_Handle : Flow_Info_T;
                 Data : String) is
      IPCP_Instance : IPCP_Access := Find_IPCP(Manager, Name);
      PDU : PDU_T;
   begin
      if IPCP_Instance = null then
         raise IPC_Error with "IPCP instance not found.";
      end if;

      -- Check if the flow exists
      for Flow of IPCP_Instance.Active_Flows loop
         if Flow.Flow_ID = Flow_Handle.Flow_ID then
            -- Create a PDU and add it to the outgoing buffer
            PDU.Data := To_Unbounded_String(Data);
            Assign_PDU(IPCP_Instance.all, PDU);
            return;
         end if;
      end loop;

      raise IPC_Error with "Flow not found.";

   exception
      when others =>
         raise IPC_Error with "Error sending data.";
   end Send;

   -- Procedure to receive data from a flow
   procedure Receive(Manager : in out IPCP_Manager_T;
                    Name : Unbounded_String;
                    Flow_Handle : Flow_Info_T;
                    Data : out String) is
      IPCP_Instance : IPCP_Access := Find_IPCP(Manager, Name);
      PDU : PDU_T;
   begin
      if IPCP_Instance = null then
         raise IPC_Error with "IPCP instance not found.";
      end if;

      -- Check if the flow exists
      for Flow of IPCP_Instance.Active_Flows loop
         if Flow.Flow_ID = Flow_Handle.Flow_ID then
            -- Check if there is incoming data
            if IPCP_Instance.Incoming_PDUs.Length > 0 then
               PDU := Pop_PDU(IPCP_Instance.all, From_Outgoing => False);
               Data := To_String(PDU.Data);
               return;
            else
               Data := "No data available";
               return;
            end if;
         end if;
      end loop;

      raise IPC_Error with "Flow not found.";

   exception
      when others =>
         raise IPC_Error with "Error receiving data.";
   end Receive;

   -- Procedure to deallocate a flow
   procedure Deallocate_Flow(Manager : in out IPCP_Manager_T;
                            Name : Unbounded_String;
                            Flow_Handle : Flow_Info_T) is
      IPCP_Instance : IPCP_Access := Find_IPCP(Manager, Name);
      Index : Natural := IPCP_Instance.Active_Flows.First_Index;
   begin
      if IPCP_Instance = null then
         raise IPC_Error with "IPCP instance not found.";
      end if;

      -- Find and remove the flow
      for Flow of IPCP_Instance.Active_Flows loop
         if Flow.Flow_ID = Flow_Handle.Flow_ID then
            IPCP_Instance.Active_Flows.Delete(Index);
            return;
         end if;
         Index := Index + 1;
      end loop;

      raise IPC_Error with "Flow not found.";

   exception
      when others =>
         raise IPC_Error with "Error deallocating flow.";
   end Deallocate_Flow;

end IPC_API;
