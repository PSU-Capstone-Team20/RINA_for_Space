with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Strings.Unbounded.Text_IO; use Ada.Strings.Unbounded.Text_IO;
with rina;

package body application is

    procedure Initialize (App : in out application; Name : String; ID : rina.address) is
    begin
        App.appName := To_Unbounded_String(Name);
        App.appID := ID;
    end Initialize;

    function Get_ApplicationName (App : application) return Unbounded_String is
    begin
        return App.appName;
    end Get_ApplicationName;

    function Get_ApplicationID (App : application) return rina.address is
    begin
        return App.appID;
    end Get_ApplicationID;

    procedure Send_Data (App : application; Data : Unbounded_String) is
    begin
        -- TODO: Integrate with other systems when they are ready
        Put_Line("Sending data: " & To_String(Data));
    end Send_Data;

    function Receive_Data (App : application) return Unbounded_String is
    begin
        -- TODO: Integrate with other systems when they are ready
        return To_Unbounded_String("Received data");
    end Receive_Data;

    procedure Process_Data (App : application; Data : Unbounded_String) is
    begin
        -- TODO: Integrate with other systems when they are ready
        Put_Line("Processing data: " & To_String(Data));
    end Process_Data;

    procedure Delete (App : in out application) is
    begin
        -- DISCUSS: Thinking applications should be deleted at a higher level but wanted to put this here and discuss further with the group.
        App.appName := Null_Unbounded_String;
        App.appID := 0;
    end Delete;

end application;
