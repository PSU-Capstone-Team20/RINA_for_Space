with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package body CDAP is

   function Create_Request (Object_Name : String; Object_Class : String) return CDAP_Message is
   begin
      return null;
   end Create_Request;

   function Delete_Request (Object_Name : String) return CDAP_Message is
   begin
      return null;
   end Delete_Request;

   function Read_Request (Object_Name : String) return CDAP_Message is
   begin
      return null;
   end Read_Request;

   function Write_Request (Object_Name : String; Value : String) return CDAP_Message is
   begin
      return null; 
   end Write_Request;

   procedure Process_Message (Message : in CDAP_Message; Result : out Result_Code) is
   begin
      null; 
   end Process_Message;

   procedure Connect (Address : String; Port : Natural; Result : out Result_Code) is
   begin
      null; 
   end Connect;

   procedure Disconnect is
   begin
      null;
   end Disconnect;

end CDAP;