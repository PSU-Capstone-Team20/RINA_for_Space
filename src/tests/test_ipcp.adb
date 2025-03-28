with Ada.Assertions; use Ada.Assertions;
with Ada.Exceptions;   use Ada.Exceptions;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;
with Test_Utils; use Test_Utils;


procedure Test_IPCP is
   pragma Assertion_Policy (Assert => Ignore);

   procedure Test_Make_IPCP is
   begin
      NULL;
   end Test_Make_IPCP;

begin
   Test_Make_IPCP;

end Test_IPCP;
