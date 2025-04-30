with Ada.Text_IO; use Ada.Text_IO;
with Ada.Assertions; use Ada.Assertions;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with IPC_Manager; use IPC_Manager;
with IPCP_Types; use IPCP_Types;

-- Test the IPC Manager package
procedure Test_IPC_Manager is
   Manager   : IPCP_Manager_T := (Name => To_Unbounded_String("MainManager"), Managed_IPCPs => IPCP_Vectors.Empty_Vector);
   IPCP1     : constant Unbounded_String := To_Unbounded_String("Sensor.IPCP");
   IPCP2     : constant Unbounded_String := To_Unbounded_String("Receiver.IPCP");

begin
   -- Create two IPCPs
   Create_IPCP(Name => IPCP1, Manager => Manager);
   Create_IPCP(Name => IPCP2, Manager => Manager);
   Put_Line("Create_IPCP calls completed.");

   -- List
   List_IPCPs(Manager);
   Put_Line("List_IPCPs completed.");

   -- Find
   declare
      Found_IPCP : IPCP_Access := Find_IPCP(Manager, IPCP1);
   begin
      Assert(Found_IPCP /= null);
      Put_Line("Find_IPCP returned a valid IPCP.");
   end;

   -- Delete IPCPs
   Delete_IPCP(Manager => Manager, Name => IPCP1);
   Delete_IPCP(Manager => Manager, Name => IPCP2);
   Put_Line("Delete_IPCP calls completed.");

end Test_IPC_Manager;
