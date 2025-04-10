with Ada.Assertions; use Ada.Assertions;
with Ada.Exceptions; use Ada.Exceptions;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;
with DIF_Manager.Dif; use DIF_Manager.Dif;

procedure Test_DIF is
   pragma Assertion_Policy (Assert => Ignore);

   -- Test Create_DIF to create a DIF
   procedure Test_Create_DIF is
      DIF1 : DIF_Access;
   begin
      DIF1 := Create_DIF;
      Assert(DIF1.DIF_ID = 0, "DIF_ID should be 0");
      Assert(DIF1.AccessibleDIFs.Is_Empty, "AccessibleDIFs should be empty");
      Assert(DIF1.Applications.Is_Empty, "Applications should be empty");
   exception
      when E : Assertion_Error =>
         Put_Line("Assertion failed in Test_Create_DIF: " & Exception_Message(E));
   end Test_Create_DIF;

   -- Test Create_Named_DIF to create a named DIF
   procedure Test_Create_Name_DIF is
      DIF1 : DIF_Access;
      Name : constant Unbounded_String := To_Unbounded_String("TestDIF");
   begin
      DIF1 := Create_Named_DIF(Name);
      Assert(DIF1 /= null, "DIF1 should not be null");
      if DIF1 /= null then
         Assert(DIF1.DIF_ID = 0, "DIF_ID should be 0");
         Assert(DIF1.DIF_Name = Name, "DIF_Name should be " & To_String(Name));
         Assert(DIF1.AccessibleDIFs.Is_Empty, "AccessibleDIFs should be empty");
         Assert(DIF1.Applications.Is_Empty, "Applications should be empty");
      end if;
   exception
      when E : Assertion_Error =>
         Put_Line("Assertion failed in Test_Create_Name_DIF: " & Exception_Message(E));
   end Test_Create_Name_DIF;

   -- Test Create_Named_DIF with an empty name
   procedure Test_Create_Empty_Name_DIF is
      DIF1 : DIF_Access;
   begin
      DIF1 := Create_Named_DIF(Null_Unbounded_String);
      Assert(DIF1 = null, "DIF1 should be null for empty name");
   exception
      when E : Assertion_Error =>
         Put_Line("Assertion failed in Test_Create_Empty_Name_DIF: " & Exception_Message(E));
   end Test_Create_Empty_Name_DIF;

   -- Test real policy behavior in Create_Named_DIF
   procedure Test_Create_Named_DIF_With_Real_Policy is
      Allowed_DIF  : DIF_Access;
      Denied_DIF   : DIF_Access;
      Good_Name    : constant Unbounded_String := To_Unbounded_String("TestDIF");
      Bad_Name     : constant Unbounded_String := To_Unbounded_String("not allowed");
   begin
      -- Should succeed
      Allowed_DIF := Create_Named_DIF(Good_Name);
      Assert(Allowed_DIF /= null, "Allowed_DIF should not be null");
      if Allowed_DIF /= null then
         Assert(Allowed_DIF.DIF_Name = Good_Name, "DIF name should match");
      end if;

      -- Should be denied by policy
      Denied_DIF := Create_Named_DIF(Bad_Name);
      Assert(Denied_DIF = null, "Denied_DIF should be null due to policy rejection");
   exception
      when E : Assertion_Error =>
         Put_Line("Assertion failed in Test_Create_Named_DIF_With_Real_Policy: " & Exception_Message(E));
   end Test_Create_Named_DIF_With_Real_Policy;


begin
   Test_Create_DIF;
   Test_Create_Name_DIF;
   Test_Create_Empty_Name_DIF;
   Test_Create_Named_DIF_With_Real_Policy;
end Test_DIF;
