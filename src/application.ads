with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package application is

    type application is tagged record
       appName : Unbounded_String;
       appID   : Integer;
    end record;

   procedure Initialize (App : in out application; Name : String; ID : Integer);
   function Get_ApplicationName (App : application) return Unbounded_String;
   function Get_ApplicationID (App : application) return Integer;
   procedure Send_Data (App : application; Data : Unbounded_String);
   function Receive_Data (App : application) return Unbounded_String;
   procedure Process_Data (App : application; Data : Unbounded_String);
   procedure Delete (App : in out application);
      
end application;