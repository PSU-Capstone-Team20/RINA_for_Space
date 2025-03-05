with RINA; use RINA;
with RINA.CDAP; use RINA.CDAP;

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Assertions;  use Ada.Assertions;
with Ada.Exceptions;   use Ada.Exceptions;

procedure Test_CDAP is
   pragma Assertion_Policy (Assert => Ignore);

   procedure Test_Build_Request is
      -- Build_Request (Op : Operation_Type; Params : Request_Params) return CDAP_Message;
      Message_1 : CDAP_Message := Build_Request (Create, (Name  => +"Test", 
                                                          Class => +"Class", 
                                                          Value => +"Value"));
   begin
      assert (Message_1.Op_Type = Delete, "Op_Type is not Create");
      assert (Message_1.Msg_Type = Request);  
      assert (Message_1.Object /= null);
   exception
      when E : Assertion_Error =>
         Put_Line("Assertion failed in Test_Build_Request: " & Exception_Message(E));
   end;

begin
   Test_Build_Request;
end;