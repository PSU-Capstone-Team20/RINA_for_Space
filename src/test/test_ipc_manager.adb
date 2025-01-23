

with Ada.Text_IO; use Ada.Text_IO;
with IPCP; use IPCP;
with IPCP.IPC_Manager;

procedure Test_IPCP_Manager is
   Manager : IPCP.IPC_Manager.IPCP_List;
   Address : RINA.Address_T;
   IPCP_Instance1, IPCP_Instance2, IPCP_Instance3 : IPCP.IPCP_T;
begin
   -- Initialize the IPCP manager
   IPCP.IPC_Manager.Initialize_Manager(Manager);
   Put_Line("IPCP Manager initialized.");

   -- Add IPCP instances
   IPCP_Instance1 := (
      ID => 1, 
      Name => "IPCP1", 
      DIF_ID => 100,
      Address => (DIF_ID => 100, App_Process_Name => To_Unbounded_String("Mock_App1")),
      QoS_Params => (Priority => 1, Latency => 10, Throughput => 100));

   IPCP_Instance2 := (
      ID => 2, 
      Name => "IPCP2", 
      DIF_ID => 200,
      Address => (DIF_ID => 200, App_Process_Name => To_Unbounded_String("Mock_App2")),
      QoS_Params => (Priority => 2, Latency => 20, Throughput => 200));

   IPCP_Instance3 := (
      ID => 3, Name => "IPCP3", 
      DIF_ID => 300,
      Address => (DIF_ID => 300, App_Process_Name => To_Unbounded_String("Mock_App3")),
      QoS_Params => (Priority => 3, Latency => 30, Throughput => 300));

   IPCP.IPC_Manager.Add_IPCP(Manager, IPCP_Instance1);
   IPCP.IPC_Manager.Add_IPCP(Manager, IPCP_Instance2);
   IPCP.IPC_Manager.Add_IPCP(Manager, IPCP_Instance3);

   Put_Line("Added 3 IPCP instances.");

   -- Display all IPCPs
   Put_Line("Displaying all IPCPs:");
   IPCP.IPC_Manager.Display_All_IPCPs(Manager);

   -- Locate an IPCP by ID
   Put_Line("Locating IPCP with ID 2:");
   declare
      Located_IPCP : IPCP.IPCP_T := IPCP.IPC_Manager.Locate_IPCP(Manager, 2);
   begin
      Put_Line("Located IPCP - Name: " & Located_IPCP.Name & ", DIF_ID: " & Located_IPCP.DIF_ID'Image);
   end;

   -- Find a DIF Address by DIF_ID
   Put_Line("Finding DIF address for DIF_ID 100:");
   Address := IPCP.IPC_Manager.Find_DIF_Address(Manager, 100);
   Put_Line("Found Address - DIF_ID: " & Address.DIF_ID'Image);
   Put_Line("Found Address - App_Process_Name: " & To_String(Address.App_Process_Name));

   -- Remove an IPCP
   Put_Line("Removing IPCP with ID 2.");
   IPCP.IPC_Manager.Remove_IPCP(Manager, 2);

   -- Display all IPCPs after removal
   Put_Line("Displaying all IPCPs after removal:");
   IPCP.IPC_Manager.Display_All_IPCPs(Manager);

   -- Attempt to locate a removed IPCP
   Put_Line("Attempting to locate removed IPCP with ID 2:");
   begin
      declare
         Located_IPCP : IPCP.IPCP_T := IPCP.IPC_Manager.Locate_IPCP(Manager, 2);
      begin
         Put_Line("Located IPCP - Name: " & Located_IPCP.Name & ", DIF_ID: " & Located_IPCP.DIF_ID'Image);
      end;
   exception
      when Program_Error =>
         Put_Line("Error: IPCP with ID 2 not found (as expected).");
   end;

   -- Attempt to find a DIF address for a non-existent DIF_ID
   Put_Line("Attempting to find DIF address for non-existent DIF_ID 400:");
   begin
      Address := IPCP.IPC_Manager.Find_DIF_Address(Manager, 400);
   exception
      when Program_Error =>
         Put_Line("Error: DIF_ID 400 not found (as expected).");
   end;
end Test_IPCP_Manager;
