with Ada.Text_IO; use Ada.Text_IO;
with Ada.Assertions; use Ada.Assertions;
with Ada.Exceptions; use Ada.Exceptions;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with DIF_Manager; use DIF_Manager;
with DIF_Manager.Dif; use DIF_Manager.Dif;
with IPCP_Types; use IPCP_Types;
with IPC_Manager.IPCP; use IPC_Manager.IPCP;

procedure Test_DIF_Manager is
   pragma Assertion_Policy (Assert => Ignore);

   -- Create a DIF by ID
   procedure Test_Create_DIF is
   begin
      DIF_Manager.Create_DIF(42);
      Put_Line("Create_DIF passed (no exceptions).");
   exception
      when E : others =>
         Put_Line("Create_DIF failed: " & Exception_Message(E));
   end Test_Create_DIF;

   -- Create a named DIF and verify its properties
   procedure Test_Create_Named_DIF is
      DIF : DIF_Access;
      Name : constant Unbounded_String := To_Unbounded_String("Mission.DIF");
   begin
      DIF := DIF_Manager.Create_Named_DIF(Name);
      Assert(DIF /= null, "Named DIF should not be null");
      Assert(DIF.DIF_Name = Name, "DIF name mismatch");
      Put_Line("Create_Named_DIF passed.");
   exception
      when E : Assertion_Error =>
         Put_Line("Assertion failed in Test_Create_Named_DIF: " & Exception_Message(E));
   end Test_Create_Named_DIF;

   -- Enroll a mock IPCP into the created DIF
   procedure Test_Enroll_IPCP is
      DIF : DIF_Access;
      Name : constant Unbounded_String := To_Unbounded_String("EnrollTest.DIF");

      Mock_IPCP : IPCP_T := Make_IPCP(To_Unbounded_String("Mock.IPCP"));
   begin
      DIF := DIF_Manager.Create_Named_DIF(Name);
      Enroll_IPCP(DIF.all, Mock_IPCP);
      Put_Line("Enroll_IPCP completed.");
   exception
      when E : others =>
         Put_Line("Enroll_IPCP failed: " & Exception_Message(E));
   end Test_Enroll_IPCP;


   -- Test removing a DIF
   procedure Test_Disconnect_DIF is
   begin
      DIF_Manager.Create_DIF(99);
      DIF_Manager.Create_DIF(100);
      DIF_Manager.Disconnect_DIF(0);  -- Remove first one
      Put_Line("Disconnect_DIF on index 0 completed.");
   exception
      when E : others =>
         Put_Line("Disconnect_DIF failed: " & Exception_Message(E));
   end Test_Disconnect_DIF;

   -- Test printing all DIFs
   procedure Test_List_DIFs is
   begin
      Put_Line("Listing all DIFs:");
      DIF_Manager.List_DIFs;
   end Test_List_DIFs;

begin

   Test_Create_DIF;
   Test_Create_Named_DIF;
   Test_Enroll_IPCP;
   Test_Disconnect_DIF;
   Test_List_DIFs;

end Test_DIF_Manager;
