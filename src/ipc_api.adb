with IPC_Manager;            use IPC_Manager;
with IPCP_Types;             use IPCP_Types;
with IPC_Manager.IPCP;       use IPC_Manager.IPCP;
with Ada.Strings.Unbounded;  use Ada.Strings.Unbounded;
with Ada.Containers;         use Ada.Containers;
with Ada.Text_IO;            use Ada.Text_IO;

package body IPC_API is

   -- Allocate a flow
   procedure Allocate_Flow(
      Manager     : in out IPCP_Manager_T;
      Name        : Unbounded_String;
      Src_CEP_ID  : Unbounded_String;
      QoS         : Natural;
      Flow_Handle : out Flow_Info_T
   ) is
      IPCP_Instance : IPCP_Access := Find_IPCP(Manager, Name);
      Flow_ID       : Natural;
   begin
      if IPCP_Instance = null then
         raise IPC_Error with "IPCP instance not found.";
      end if;

      Flow_ID := Natural(IPCP_Instance.Active_Flows.Length) + 1;

      Flow_Handle := (
         Flow_ID       => Flow_ID,
         Port_ID       => 1,
         QoS_ID        => QoS,
         Remote_CEP_ID => Src_CEP_ID
      );

      Add_Flow(IPCP_Instance.all, Flow_Handle);
   end Allocate_Flow;

   -- Send data over a flow
   procedure Send(
      Manager     : in out IPCP_Manager_T;
      Name        : Unbounded_String;
      Flow_Handle : Flow_Info_T;
      Data        : Unbounded_String
   ) is
      IPCP_Instance : IPCP_Access := Find_IPCP(Manager, Name);
      PDU           : PDU_T;
      Raw_Data      : constant String := To_String(Data);
      Len           : constant Natural := Raw_Data'Length;
   begin
      if IPCP_Instance = null then
         raise IPC_Error with "IPCP instance not found.";
      end if;

      if not Flow_Exists(IPCP_Instance.all, Flow_Handle.Flow_ID) then
         raise IPC_Error with "Flow not found.";
      end if;

      -- Copy into fixed-length Data field
      PDU.Data := (others => ' ');
      PDU.Data(1 .. Len) := Raw_Data;

      Assign_PDU(IPCP_Instance.all, PDU);
   end Send;

   -- Receive data from a flow
   procedure Receive(
      Manager     : in out IPCP_Manager_T;
      Name        : Unbounded_String;
      Flow_Handle : Flow_Info_T;
      Data        : out Unbounded_String
   ) is
      IPCP_Instance : IPCP_Access := Find_IPCP(Manager, Name);
      PDU           : PDU_T;
      Valid_End     : Natural := 1024;
   begin
      if IPCP_Instance = null then
         raise IPC_Error with "IPCP instance not found.";
      end if;

      if not Flow_Exists(IPCP_Instance.all, Flow_Handle.Flow_ID) then
         raise IPC_Error with "Flow not found.";
      end if;

      if IPCP_Instance.Incoming_PDUs.Length > 0 then
         PDU := Pop_PDU(IPCP_Instance.all, From_Outgoing => False);

         -- Trim trailing spaces to find valid data length
         -- this can delete once we get a proper length field
         while Valid_End > 0 and then PDU.Data(Valid_End) = ' ' loop
            Valid_End := Valid_End - 1;
         end loop;

         if Valid_End = 0 then
            Data := To_Unbounded_String("");
         else
            Data := To_Unbounded_String(PDU.Data(1 .. Valid_End));
         end if;

      else
         Data := To_Unbounded_String("No data available");
      end if;
   end Receive;

   -- Deallocate a flow
   procedure Deallocate_Flow(
      Manager     : in out IPCP_Manager_T;
      Name        : Unbounded_String;
      Flow_Handle : Flow_Info_T
   ) is
      IPCP_Instance : IPCP_Access := Find_IPCP(Manager, Name);
   begin
      if IPCP_Instance = null then
         raise IPC_Error with "IPCP instance not found.";
      end if;

      Remove_Flow(IPCP_Instance.all, Flow_Handle.Flow_ID);
   end Deallocate_Flow;

end IPC_API;
