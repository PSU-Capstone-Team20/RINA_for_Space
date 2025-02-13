with Ada.Text_IO; use Ada.Text_IO;
with IPCP;

package body IPC_Manager is

   -- Initialize IPC Manager
   procedure Initialize_Manager(Mgr : in out IPCM_T) is -- IPCM_T is IPC Manager type 
   begin
      Mgr.IPCP_List.Clear;
      Put_Line("IPC Manager initialized.");
   end Initialize_Manager;

   -- Create and Add a New IPCP Instance
   procedure Create_IPCP(Mgr : in out IPCM_T; ID : IPCP_ID; Name : String; Address : String; QoS_Params : String) is
      New_IPCP : IPCP_T;
   begin
      Initialize_IPCP(New_IPCP, ID, Name, Address, QoS_Params);
      Mgr.IPCP_List.Append(New_IPCP);
      Put_Line("Created IPCP: " & Name & " with ID: " & ID'Image);
   end Create_IPCP;

   -- Activate IPCP by ID
   procedure Activate_IPCP(Mgr : in out IPCM_T; ID : IPCP_ID) is
   begin
      for IPCP of Mgr.IPCP_List loop
         if IPCP.ID = ID then
            Activate_IPCP(IPCP);
            Put_Line("Activated IPCP: " & IPCP.Name);
            return;
         end if;
      end loop;
      Put_Line("Error: IPCP with ID " & ID'Image & " not found.");
   end Activate_IPCP;

   -- Terminate IPCP by ID
   procedure Terminate_IPCP(Mgr : in out IPCM_T; ID : IPCP_ID) is
   begin
      for IPCP of Mgr.IPCP_List loop
         if IPCP.ID = ID then
            Terminate_IPCP(IPCP);
            Put_Line("Terminated IPCP: " & IPCP.Name);
            return;
         end if;
      end loop;
      Put_Line("Error: IPCP with ID " & ID'Image & " not found.");
   end Terminate_IPCP;

   -- List All Managed IPCPs
   procedure List_All_IPCPs(Mgr : IPCM_T) is
   begin
      Put_Line("Listing all IPCPs managed:");
      for IPCP of Mgr.IPCP_List loop
         Put_Line("ID: " & IPCP.ID'Image & ", Name: " & IPCP.Name & ", State: " & IPCP_State'Image(IPCP.State));
      end loop;
   end List_All_IPCPs;

   -- Allocate a Flow Between Two IPCPs
   procedure Allocate_Flow(Mgr : in out IPCM_T; Src : String; Dest : String) is
   begin
      Allocate_New_Flow(Src, Dest); -- Calls Flow Manager
      Put_Line("Flow allocated between " & Src & " and " & Dest);
   end Allocate_Flow;

   -- Deallocate a Flow
   procedure Deallocate_Flow(Mgr : in out IPCM_T; Flow_ID : Integer) is
   begin
      Release_Flow(Flow_ID); -- Calls Flow Manager
      Put_Line("Flow ID " & Flow_ID'Image & " deallocated.");
   end Deallocate_Flow;

   -- Monitor All IPCP Resources
   procedure Monitor_Resources(Mgr : IPCM_T) is
   begin
      for IPCP of Mgr.IPCP_List loop
         Monitor(IPCP.ID); -- Calls Resource Manager
         Put_Line("Monitoring resources for IPCP: " & IPCP.Name);
      end loop;
   end Monitor_Resources;

   -- CDAP Handling
   procedure Handle_CDAP_Request(Mgr : in out IPCM_T; Object_Name : String; Op : Operation_Type) is
      Msg : CDAP_Message;
      Result : Result_Code;
   begin
      case Op is
         when Create =>
            Msg := Create_Request(Object_Name, "Default_Class");
         when Delete =>
            Msg := Delete_Request(Object_Name);
         when Read =>
            Msg := Read_Request(Object_Name);
         when Write =>
            Msg := Write_Request(Object_Name, "New Value");
         when Start | Stop =>
            Put_Line("Unsupported operation: " & Operation_Type'Image(Op));
            return;
         when Connect =>
            Put_Line("CDAP Connect must be handled separately.");
            return;
      end case;

      Process_Message(Msg, Result);
      Put_Line("CDAP Request Processed. Result: " & Result_Code'Image(Result));
   end Handle_CDAP_Request;

end IPC_Manager;
