package Render_Buffer is

    type Render_Buffer is array (1 .. 60, 1 .. 100) of Character;

    procedure Clear_Buffer (RB : in out Render_Buffer);
    procedure Render_Buffer_To_Screen (RB : Render_Buffer);
    procedure Update_Buffer (RB : in out Render_Buffer; Row, Col : Integer; Char : Character);
    
    procedure Draw_Char (RB : in out Render_Buffer; Char : Character; X, Y : Integer);
    procedure Draw_String (RB : in out Render_Buffer; Str : String; X, Y : Integer);
    procedure Draw_RinaForSpace (RB : in out Render_Buffer; X, Y : Integer);
    procedure Draw_Border (RB : in out Render_Buffer; Vertical, Horizontal : Character; X1, Y1, X2, Y2 : Integer);
    procedure Draw_Line (RB : in out Render_Buffer; Char : Character; X1, Y1, X2, Y2 : Integer);
   
    function Clear_Screen return String;
    function Hide_Cursor return String;

    -- Cursor type and variable with X and Y
    type Cursor is record
        X : Integer;
        Y : Integer;
        Char : Character := ' '; -- Default cursor character
    end record;

    procedure Move_Cursor (RB : in out Render_Buffer; C : in out Cursor; New_X, New_Y : Integer);
    function Get_Cursor_Position (C : Cursor) return String;
    procedure Add_Cursor_To_Buffer (RB : in out Render_Buffer; C : Cursor);
    procedure Clear_Cursor_From_Buffer (RB : in out Render_Buffer; C : in out Cursor);
    procedure Set_Initial_Cursor_Position (RB : in out Render_Buffer; C : in out Cursor; X, Y : Integer; Cursor_Char : Character);

end Render_Buffer;
