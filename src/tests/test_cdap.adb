with Ada.Text_IO; use Ada.Text_IO;
with Ada.Assertions; use Ada.Assertions;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with CDAP; use CDAP;

procedure Test_CDAP is

   Mock_Params : Request_Params := (
      Name  => To_Unbounded_String("AppA"),
      Class => To_Unbounded_String("Mock_Class"),
      Value => To_Unbounded_String("Mock_Value")
   );

   Msg : CDAP_Message;

   procedure Assert_Object_Match(M : CDAP_Message; P : Request_Params) is
   begin
      Assert(M.Object /= null, "Object should not be null");
      Assert(M.Object.Name = P.Name, "Object.Name mismatch");
      Assert(M.Object.Class = P.Class, "Object.Class mismatch");
      Assert(M.Object.Value = P.Value, "Object.Value mismatch");
   end Assert_Object_Match;

begin
   Put_Line("Running Test_CDAP...");

   -- Build_Request
   Msg := Build_Request(Create, Mock_Params);
   Assert(Msg.Op_Type = Create, "Build_Request Op_Type mismatch");
   Assert_Object_Match(Msg, Mock_Params);

   -- Create_Request
   Msg := Create_Request(Mock_Params);
   Assert(Msg.Op_Type = Create, "Create_Request failed");
   Assert_Object_Match(Msg, Mock_Params);

   -- Delete_Request
   Msg := Delete_Request(Mock_Params);
   Assert(Msg.Op_Type = Delete, "Delete_Request failed");
   Assert_Object_Match(Msg, Mock_Params);

   -- Read_Request
   Msg := Read_Request(Mock_Params);
   Assert(Msg.Op_Type = Read, "Read_Request failed");
   Assert_Object_Match(Msg, Mock_Params);

   -- Write_Request
   Msg := Write_Request(Mock_Params);
   Assert(Msg.Op_Type = Write, "Write_Request failed");
   Assert_Object_Match(Msg, Mock_Params);

end Test_CDAP;
