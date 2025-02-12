with Ada.Text_IO; use Ada.Text_IO;
with IPCP;

package body IPCP.IPC_Manager is

   -- Add a new IPCP instance to the manager
   procedure Add_IPCP(Manager : in out IPC_Manager_Record; IPCP_Instance : IPCP.IPCP_Record) is
   begin
      Manager.Managed_IPCPs.Append(IPCP_Instance);
      Put_Line("IPCP " & IPCP_Instance.Name & " added to the manager.");
   end Add_IPCP;

   -- Remove an IPCP instance by ID
   procedure Remove_IPCP(Manager : in out IPC_Manager_Record; IPCP_ID : IPCP.IPCP_ID) is
   begin
      for I in Manager.Managed_IPCPs.First_Index .. Manager.Managed_IPCPs.Last_Index loop
         if Manager.Managed_IPCPs(I).ID = IPCP_ID then
            Manager.Managed_IPCPs.Delete(I);
            Put_Line("IPCP with ID " & IPCP_ID'Image & " removed from the manager.");
            exit;
         end if;
      end loop;
   end Remove_IPCP;

   -- Get information about a specific IPCP
   procedure Get_IPCP_Info(Manager : IPC_Manager_Record; IPCP_ID : IPCP.IPCP_ID) is
   begin
      for IPCP_Instance of Manager.Managed_IPCPs loop
         if IPCP_Instance.ID = IPCP_ID then
            Put_Line("IPCP Info:");
            Put_Line("ID: " & IPCP_Instance.ID'Image);
            Put_Line("Name: " & IPCP_Instance.Name);
            Put_Line("State: " & IPCP.IPCP_State'Image(IPCP_Instance.State));
            return;
         end if;
      end loop;
      Put_Line("IPCP with ID " & IPCP_ID'Image & " not found.");
   end Get_IPCP_Info;

   -- List all IPCP instances managed
   procedure List_All_IPCPs(Manager : IPC_Manager_Record) is
   begin
      Put_Line("Listing all managed IPCPs:");
      for IPCP_Instance of Manager.Managed_IPCPs loop
         Put_Line("ID: " & IPCP_Instance.ID'Image & ", Name: " & IPCP_Instance.Name & ", State: " & IPCP.IPCP_State'Image(IPCP_Instance.State));
      end loop;
   end List_All_IPCPs;

   -- Monitor status of all IPCP instances
   procedure Monitor_IPCP_Status(Manager : in out IPC_Manager_Record) is
   begin
      for IPCP_Instance of Manager.Managed_IPCPs loop
         Put_Line("Monitoring IPCP: " & IPCP_Instance.Name & " - State: " & IPCP.IPCP_State'Image(IPCP_Instance.State));
      end loop;
   end Monitor_IPCP_Status;

   -- Restart a specific IPCP instance
   procedure Restart_IPCP(Manager : in out IPC_Manager_Record; IPCP_ID : IPCP.IPCP_ID) is
   begin
      for I in Manager.Managed_IPCPs.First_Index .. Manager.Managed_IPCPs.Last_Index loop
         if Manager.Managed_IPCPs(I).ID = IPCP_ID then
            IPCP.Terminate(Manager.Managed_IPCPs(I));
            IPCP.Activate(Manager.Managed_IPCPs(I));
            Put_Line("IPCP with ID " & IPCP_ID'Image & " has been restarted.");
            exit;
         end if;
      end loop;
   end Restart_IPCP;

   -- Retry all failed communications for all managed IPCPs
   procedure Retry_All_Failed_Communications(Manager : in out IPC_Manager_Record) is
   begin
      for IPCP_Instance of Manager.Managed_IPCPs loop
         IPCP.Retry_Failed_Communication(IPCP_Instance);
      end loop;
      Put_Line("Retried all failed communications for managed IPCPs.");
   end Retry_All_Failed_Communications;

   -- Transmit all stored data for all managed IPCPs
   procedure Transmit_All_Stored_Data(Manager : in out IPC_Manager_Record) is
   begin
      for IPCP_Instance of Manager.Managed_IPCPs loop
         IPCP.Transmit_Stored_Data(IPCP_Instance);
      end loop;
      Put_Line("Transmitted all stored data for managed IPCPs.");
   end Transmit_All_Stored_Data;

   -- Apply a policy to all managed IPCPs
   --  procedure Set_Policy(Manager : in out IPC_Manager_Record; Policy_Name : String; Policy_Value : String) is
   --  begin
   --     for IPCP_Instance of Manager.Managed_IPCPs loop
   --        IPCP.Set_Policy(IPCP_Instance, Policy_Name, Policy_Value);
   --     end loop;
   --     Put_Line("Policy " & Policy_Name & " set to " & Policy_Value & " for all managed IPCPs.");
   --  end Set_Policy;

   --  -- Retrieve a policy from the first IPCP (assuming uniform policy application)
   --  procedure Get_Policy(Manager : IPC_Manager_Record; Policy_Name : String) is
   --     Policy_Value : String;
   --  begin
   --     if not Manager.Managed_IPCPs.Is_Empty then
   --        Policy_Value := IPCP.Get_Policy(Manager.Managed_IPCPs.First_Element, Policy_Name);
   --        Put_Line("Policy " & Policy_Name & ": " & Policy_Value);
   --     else
   --        Put_Line("No IPCPs managed to retrieve policy from.");
   --     end if;
   --  end Get_Policy;


end IPCP.IPC_Manager;
