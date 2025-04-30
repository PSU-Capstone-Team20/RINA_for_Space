with Ada.Text_IO;         use Ada.Text_IO;
with Ada.Assertions; use Ada.Assertions;
with Ada.Exceptions; use Ada.Exceptions;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with IPC_Manager.IPCP;    use IPC_Manager.IPCP;
with IPCP_Types;          use IPCP_Types;

procedure Test_IPCP is

   -- Test empty IPCP name
   procedure Test_Empty_Name is
      IPCP : IPCP_T := Make_IPCP(Null_Unbounded_String);
   begin
      Assert(Length(IPCP.Name) = 0, "Name should be empty");
      Assert(IPCP.State = Initialized);
      Put_Line("Test_Empty_Name passed.");
   exception
      when E : others =>
         Put_Line("Test_Empty_Name failed: " & Exception_Message(E));
   end Test_Empty_Name;

   -- Test very long IPCP name
   procedure Test_Long_Name is
      Long_Str : constant String := (1 .. 500 => 'A');
      IPCP : IPCP_T := Make_IPCP(To_Unbounded_String(Long_Str));
   begin
      Assert(Length(IPCP.Name) = 500, "Expected 500-character name");
      Assert(IPCP.State = Initialized);
      Put_Line("Test_Long_Name passed.");
   exception
      when E : others =>
         Put_Line("Test_Long_Name failed: " & Exception_Message(E));
   end Test_Long_Name;

   -- Test special characters in IPCP name
   procedure Test_Special_Char_Name is
      IPCP : IPCP_T := Make_IPCP(To_Unbounded_String("ICPC@#2024!"));
   begin
      Assert(To_String(IPCP.Name) = "ICPC@#2024!");
      Put_Line("Test_Special_Char_Name passed.");
   exception
      when E : others =>
         Put_Line("Test_Special_Char_Name failed: " & Exception_Message(E));
   end Test_Special_Char_Name;

   -- Test Two instances have independent state
   procedure Test_Isolation is
      IPCP1 : IPCP_T := Make_IPCP(To_Unbounded_String("One"));
      IPCP2 : IPCP_T := Make_IPCP(To_Unbounded_String("Two"));
   begin
      Assert(To_String(IPCP1.Name) /= To_String(IPCP2.Name));
      Assert(IPCP1.State = Initialized and IPCP2.State = Initialized);
      Put_Line("Test_Isolation passed.");
   exception
      when E : others =>
         Put_Line("Test_Isolation failed: " & Exception_Message(E));
   end Test_Isolation;

begin
   Test_Empty_Name;
   Test_Long_Name;
   Test_Special_Char_Name;
   Test_Isolation;

end Test_IPCP;
