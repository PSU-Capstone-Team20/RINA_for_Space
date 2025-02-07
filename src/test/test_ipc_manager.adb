with Ada.Text_IO; use Ada.Text_IO;
with Ada.Assertions; use Ada.Assertions;
with IPCP;
with IPCP.IPC_Manager;
with RINA;
with RINA_Policies;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

procedure IPCP_Manager_Test is
   -- Declare IPCP Instances
   IPCP1, IPCP2 : IPCP.IPCP_T;

   -- Declare IPC Manager
   IPCP_Manager : IPCP.IPC_Manager.IPCP_List;

   -- Address and QoS Definitions
   Address1 : RINA.Address_T := (DIF_ID => 1001, App_Process_Name => To_Unbounded_String("MockApp1"));
   Address2 : RINA.Address_T := (DIF_ID => 1002, App_Process_Name => To_Unbounded_String("MockApp2"));

   QoS1 : RINA_Policies.QoS_Parameter := (Priority => 1, Latency => 100, Throughput => 1000, QoS_ID => 1);
   QoS2 : RINA_Policies.QoS_Parameter := (Priority => 2, Latency => 50, Throughput => 500, QoS_ID => 2);

begin
   Put_Line("Starting IPCP Manager Unit Tests...");

   -- Initialize IPC Manager
   IPCP.IPC_Manager.Initialize_Manager(IPCP_Manager);
   Assert(IPCP_Manager.Length = 0, "Test Failed: IPC Manager should be empty after initialization.");
   Put_Line("IPC Manager initialized correctly.");

   -- Add IPCP Instances
   IPCP.Initialize_IPCP(IPCP1, 1, "DIF1", 1001, Address1, QoS1);
   IPCP.Initialize_IPCP(IPCP2, 2, "DIF2", 1002, Address2, QoS2);
   IPCP.IPC_Manager.Add_IPCP(IPCP_Manager, IPCP1);
   IPCP.IPC_Manager.Add_IPCP(IPCP_Manager, IPCP2);
   Assert(IPCP_Manager.Length = 2, "Test Failed: IPC Manager should contain 2 IPCPs.");
   Put_Line("IPCPs added successfully.");

   -- Locate IPCP by ID
   declare
      Located_IPCP : IPCP.IPCP_T;
   begin
      Located_IPCP := IPCP.IPC_Manager.Locate_IPCP(IPCP_Manager, 1);
      Assert(Located_IPCP.Name = "DIF1", "Test Failed: IPCP with ID 1 not found or incorrect.");
      Put_Line("IPCP located by ID successfully.");
   end;

   -- Find DIF Address
   declare
      Found_Address : RINA.Address_T;
   begin
      Found_Address := IPCP.IPC_Manager.Find_DIF_Address(IPCP_Manager, 1002);
      Assert(To_String(Found_Address.App_Process_Name) = "MockApp2", "Test Failed: DIF Address for ID 1002 not found.");
      Put_Line("DIF Address found successfully.");
   end;

   -- Remove IPCP
   IPCP.IPC_Manager.Remove_IPCP(IPCP_Manager, 1);
   Assert(IPCP_Manager.Length = 1, "Test Failed: IPCP was not removed from manager.");
   Put_Line("IPCP removed successfully.");

   -- Display All IPCPs
   Put_Line("Displaying all IPCPs:");
   IPCP.IPC_Manager.Display_All_IPCPs(IPCP_Manager);

   -- Error Handling - Locate Non-Existent IPCP
   begin
      IPCP.IPC_Manager.Locate_IPCP(IPCP_Manager, 3);  -- ID 3 does not exist
      Assert(False, "Test Failed: Expected error for non-existent IPCP not raised.");
   exception
      when Program_Error => Put_Line("Correctly caught error when locating non-existent IPCP.");
   end;

   -- Test 8: Error Handling - Remove Non-Existent IPCP
   begin
      IPCP.IPC_Manager.Remove_IPCP(IPCP_Manager, 3);  -- ID 3 does not exist
      Assert(False, "Test Failed: Expected error for non-existent IPCP not raised.");
   exception
      when others => Put_Line("Correctly handled removal of non-existent IPCP.");
   end;

   Put_Line("All IPCP Manager Unit Tests Completed Successfully.");

exception
   when Assertion_Error =>
      Put_Line("An assertion failed during testing.");
   when others =>
      Put_Line("An unexpected error occurred during testing.");

end IPCP_Manager_Test;
