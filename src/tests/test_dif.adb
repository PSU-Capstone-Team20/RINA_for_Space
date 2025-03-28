with Ada.Assertions; use Ada.Assertions;
with Ada.Exceptions;   use Ada.Exceptions;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;
with Test_Utils; use Test_Utils;
with DIF; use DIF; 

procedure Test_DIF is
   pragma Assertion_Policy (Assert => Ignore);

   -- Test CreateDIF to create a mock DIF
   procedure Test_Create_DIF is
      DIF1 : DIF_Access;
   begin
      DIF1 := Create_Mock_DIF;
      Assert(DIF1.DIF_ID = 0, "DIF_ID should be 0");
      Assert(DIF1.AccessibleDIFs.Is_Empty, "AccessibleDIFs should be empty");
      Assert(DIF1.Applications.Is_Empty, "Applications should be empty");
   exception
      when E : Assertion_Error =>
         Put_Line ("Assertion failed in Test_CreateDIF: " & Exception_Message (E));
   end Test_Create_DIF;
  
   -- Test CreateNameDIF to create a mock named DIF
   procedure Test_Create_Name_DIF is
      DIF1 : DIF_Access;
      Name : constant UString := +"TestDIF";
   begin
      DIF1 := Create_Mock_Named_DIF(Name);
      Assert(DIF1.DIF_ID = 0, "DIF_ID should be 0");
      Assert(DIF1.DIF_Name = Name, "DIF_Name should be " & (+Name));
      Assert(DIF1.AccessibleDIFs.Is_Empty, "AccessibleDIFs should be empty");
      Assert(DIF1.Applications.Is_Empty, "Applications should be empty");
   exception
      when E : Assertion_Error =>
         Put_Line ("Assertion failed in Test_Create_Name_DIF: " & Exception_Message (E));   
   end Test_Create_Name_DIF;

   -- Test CreateNameDIF to create a DIF without a name
   procedure Test_Create_Empty_Name_DIF is
      DIF1 : DIF_Access;
   begin
      DIF1 := Create_Mock_Named_DIF(Null_UString);
      Assert(DIF1.DIF_Name = Null_UString, "DIF_Name should be empty");
   exception
      when E : Assertion_Error =>
         Put_Line ("Assertion failed in Test_Create_Empty_Name_DIF: " & Exception_Message (E));
   end Test_Create_Empty_Name_DIF;

begin
   Test_Create_DIF;
   Test_Create_Name_DIF;
   Test_Create_Empty_Name_DIF;

end Test_DIF;
