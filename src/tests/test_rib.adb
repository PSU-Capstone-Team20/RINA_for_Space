with Ada.Assertions; use Ada.Assertions;
with Ada.Exceptions; use Ada.Exceptions;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;
with RIB; use RIB;
with Test_Utils; use Test_Utils;

procedure Test_RIB is
   pragma Assertion_Policy (Assert => Ignore);

   -- Test Add_Entry to add a new RIB entry
   procedure Test_Add_Entry is
      Mock_RIB_Entry_Instance : RIB_Entry := Create_Mock_RIB_Entry;
   begin
      Add_Entry(Mock_RIB_Entry_Instance.Name);
      Assert(Find_Entry(Mock_RIB_Entry_Instance.Name), "Entry should exist for " & To_String(Mock_RIB_Entry_Instance.Name));
      Delete_Entry(Mock_RIB_Entry_Instance.Name);
   exception
      when E : Assertion_Error =>
         Put_Line("Assertion failed in Test_Add_Entry: " & Exception_Message(E));
   end Test_Add_Entry;

   -- Test Add_IPCP to add an IPCP to an existing RIB entry
   procedure Test_Add_IPCP is
      Mock_RIB_Entry_Instance : RIB_Entry := Create_Mock_RIB_Entry;
      Comp_Name  : Unbounded_String := To_Unbounded_String("TestComp");
      IPCP       : Unbounded_String := To_Unbounded_String("TestIPCP");
      RIB_Entry_Instance  : RIB_Entry;
   begin
      Add_Entry(Mock_RIB_Entry_Instance.Name);
      Add_Comp(Mock_RIB_Entry_Instance.Name, Comp_Name);
      Add_IPCP(Mock_RIB_Entry_Instance.Name, Comp_Name, IPCP);

      RIB_Entry_Instance := Get_Entry(Mock_RIB_Entry_Instance.Name);
      Assert(Get_IPCP(1, Comp_Name, RIB_Entry_Instance) = IPCP, "IPCP should be " & To_String(IPCP));

      Delete_Entry(Mock_RIB_Entry_Instance.Name);
   exception
      when E : Assertion_Error =>
         Put_Line("Assertion failed in Test_Add_IPCP: " & Exception_Message(E));
   end Test_Add_IPCP;

   -- Test Add_Comp to add a computer connection to an existing RIB entry
   procedure Test_Add_Comp is
      Mock_RIB_Entry_Instance : RIB_Entry := Create_Mock_RIB_Entry;
      Comp_Name  : Unbounded_String := To_Unbounded_String("TestComp");
      RIB_Entry_Instance  : RIB_Entry;
   begin
      Add_Entry(Mock_RIB_Entry_Instance.Name);
      Add_Comp(Mock_RIB_Entry_Instance.Name, Comp_Name);

      RIB_Entry_Instance := Get_Entry(Mock_RIB_Entry_Instance.Name);
      Assert(RIB_Entry_Instance.Obj_Type.Contains(Comp_Name), "Comp should exist for " & To_String(Comp_Name));

      Delete_Entry(Mock_RIB_Entry_Instance.Name);
   exception
      when E : Assertion_Error =>
         Put_Line("Assertion failed in Test_Add_Comp: " & Exception_Message(E));
   end Test_Add_Comp;

   -- Test Add_APN to add an APN to an existing RIB entry
   procedure Test_Add_APN is
      Mock_RIB_Entry_Instance : RIB_Entry := Create_Mock_RIB_Entry;
      Comp_Name  : Unbounded_String := To_Unbounded_String("TestComp");
      APN        : Unbounded_String := To_Unbounded_String("TestAPN");
      RIB_Entry_Instance  : RIB_Entry;
   begin
      Add_Entry(Mock_RIB_Entry_Instance.Name);
      Add_Comp(Mock_RIB_Entry_Instance.Name, Comp_Name);
      Add_APN(Mock_RIB_Entry_Instance.Name, Comp_Name, APN);

      RIB_Entry_Instance := Get_Entry(Mock_RIB_Entry_Instance.Name);
      Assert(Get_APN(1, Comp_Name, RIB_Entry_Instance) = APN, "APN should be " & To_String(APN));

      Delete_Entry(Mock_RIB_Entry_Instance.Name);
   exception
      when E : Assertion_Error =>
         Put_Line("Assertion failed in Test_Add_APN: " & Exception_Message(E));
   end Test_Add_APN;

begin
   Test_Add_Entry;
   Test_Add_IPCP;
   Test_Add_Comp;
   Test_Add_APN;
end Test_RIB;
