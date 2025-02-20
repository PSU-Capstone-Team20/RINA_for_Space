with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Containers.Vectors;
with Ada.Real_Time;         use Ada.Real_Time;
with Ada.Text_IO; use Ada.Text_IO;

package body CDAP is

   function Build_Request (Op : Operation_Type; Params : Request_Params) return CDAP_Message is
      Obj : aliased Object_Instance := (
         Name  => Params.Name,
         Class => Params.Class,
         Value => Params.Value);
      Obj_Access : Object_Instance_Access := new Object_Instance'(Obj);
   begin
      return (
         Op_Type   => Op,
         Msg_Type  => Request,
         Object    => Obj_Access,
         Result    => Success    --TODO: Add error handling
      );
   end Build_Request;

   function Create_Request (Params : Request_Params) return CDAP_Message is
   begin
      return Build_Request(Create, Params);
   end Create_Request;

   function Delete_Request (Params : Request_Params) return CDAP_Message is
   begin
      return Build_Request(Delete, Params);
   end Delete_Request;

   function Read_Request (Params : Request_Params) return CDAP_Message is
   begin
      return Build_Request(Read, Params);
   end Read_Request;

   function Write_Request (Params : Request_Params) return CDAP_Message is
   begin
      return Build_Request(Write, Params);
   end Write_Request;

   procedure Process_Message (Message : in CDAP_Message; Result : out Result_Code) is
   begin
      case Message.Op_Type is
         when Create =>
            if Message.Object /= null then
               Put_Line("Processing Create Request for Object: " &
                        To_String(Message.Object.all.Name));
            else
               Put_Line("Create Request received with no object instance.");
            end if;
            Result := Success;

         when Delete =>
            if Message.Object /= null then
               Put_Line("Processing Delete Request for Object: " &
                        To_String(Message.Object.all.Name));
            else
               Put_Line("Delete Request received with no object instance.");
            end if;
            Result := Success;

         when Read =>
            if Message.Object /= null then
               Put_Line("Processing Read Request for Object: " &
                        To_String(Message.Object.all.Name));
            else
               Put_Line("Read Request received with no object instance.");
            end if;
            Result := Success;

         when Write =>
            if Message.Object /= null then
               Put_Line("Processing Write Request for Object: " &
                        To_String(Message.Object.all.Name));
            else
               Put_Line("Write Request received with no object instance.");
            end if;
            Result := Success;

         when others =>
            Put_Line("Unsupported operation: " & Operation_Type'Image(Message.Op_Type));
            Result := Failure;
      end case;
   end Process_Message;

   procedure Connect is
   begin
      null;
   end Connect;

   procedure Disconnect is
   begin
      null;
   end Disconnect;

end CDAP;