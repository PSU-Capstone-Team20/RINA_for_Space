package Render_Buffer is

    type Render_Buffer is array (1 .. 20, 1 .. 50) of Character;

    procedure Clear_Buffer (RB : in out Render_Buffer);
    procedure Render_Buffer_To_Screen (RB : Render_Buffer);
    procedure Update_Buffer (RB : in out Render_Buffer; Row, Col : Integer; Char : Character);
    
    procedure Draw_Char (RB : in out Render_Buffer; Char : Character; X, Y : Integer);
    procedure Draw_String (RB : in out Render_Buffer; Str : String; X, Y : Integer);
    procedure Draw_Border (RB : in out Render_Buffer; Style : Character; X1, Y1, X2, Y2 : Integer);

end Render_Buffer;
