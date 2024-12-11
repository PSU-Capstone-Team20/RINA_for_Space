with Ada.Assertions;  use Ada.Assertions;
with Ada.Exceptions;   use Ada.Exceptions;

with Dif; use Dif;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Containers; use Ada.Containers;

procedure Tests is
   pragma Assertion_Policy (Assert => Ignore);
   
   procedure Test_CreateDIF is
      DV : Dif.DIF_Vector;
   begin     
      Dif.createDIF (1, DV);
      assert (DV.Length = 0, "Length of DV is not 0");
      assert (DV (0).DIF_ID = 1);
   exception
      when E : Assertion_Error =>
         Put_Line("Assertion failed in Test_CreateDIF: " & Exception_Message(E));
   end;
begin
   Test_CreateDIF;
end Tests;
