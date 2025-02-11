with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
--with Ada.Containers.Vectors;
with Ada.Text_IO; use Ada.Text_IO;

package CDAP is

   type Operation_Type is (Create, Delete, Read, Write, Start, Stop, Connect);

   type Message_Type is (Request, Response);

   type Result_Code is (Success, Failure, Not_Found, Access_Denied);

   type Object_Instance is record
      Name : String(1 .. 1024);
      Class : String(1 .. 1024);
      Value : String(1 .. 1024);
   end record;

   type Object_Instance_Access is access Object_Instance;

   type CDAP_Message is record
      Op_Type : Operation_Type;
      Msg_Type : Message_Type;
      Object : Object_Instance_Access;
      Result : Result_Code;
   end record;

   -- Function to create a request message
   function Create_Request (Object_Name : String; Object_Class : String) return CDAP_Message;
   -- Function to create a delete request message
   function Delete_Request (Object_Name : String) return CDAP_Message;
   -- Function to create a read request message
   function Read_Request (Object_Name : String) return CDAP_Message;
   -- Function to create a write request message
   function Write_Request (Object_Name : String; Value : String) return CDAP_Message;
   -- Procedure to process a received message
   procedure Process_Message (Message : in CDAP_Message; Result : out Result_Code);
   -- Procedure to establish a connection
   procedure Connect (Address : String; Port : Natural; Result : out Result_Code);
   -- Procedure to disconnect
   procedure Disconnect;

end CDAP;