with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
--with Ada.Containers.Vectors;
with Ada.Text_IO; use Ada.Text_IO;

package CDAP is

   type Operation_Type is (Create, Delete, Read, Write, Start, Stop, Connect);

   type Message_Type is (Request, Response);

   type Result_Code is (Success, Failure, Not_Found, Access_Denied);

   type Object_Instance is record
      Name : Unbounded_String;
      Class : Unbounded_String;
      Value : Unbounded_String;
   end record;

   type Object_Instance_Access is access Object_Instance;

   type CDAP_Message is record
      Op_Type : Operation_Type;
      Msg_Type : Message_Type;
      Object : Object_Instance_Access;
      Result : Result_Code;
   end record;

   type Request_Params is record
      Name  : Unbounded_String := Null_Unbounded_String;
      Class : Unbounded_String := Null_Unbounded_String;
      Value : Unbounded_String := Null_Unbounded_String;
   end record;

   function Build_Request (Op : Operation_Type; Params : Request_Params) return CDAP_Message;

   function Create_Request (Params : Request_Params) return CDAP_Message;
   function Delete_Request (Params : Request_Params) return CDAP_Message;
   function Read_Request (Params : Request_Params) return CDAP_Message;
   function Write_Request (Params : Request_Params) return CDAP_Message;


   procedure Process_Message (Message : in CDAP_Message; Result : out Result_Code);

   -- TODO: Get this working
   procedure Connect;
   procedure Disconnect;

end CDAP;