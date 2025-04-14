with Ada.Text_IO; use Ada.Text_IO;

package body Render_Buffer is

    procedure Clear_Buffer (RB : in out Render_Buffer) is
    begin
        for R in RB'Range (1) loop
            for C in RB'Range (2) loop
                RB (R, C) := ' ';
            end loop;
        end loop;
    end Clear_Buffer;

    procedure Render_Buffer_To_Screen (RB : Render_Buffer) is
    begin
        for R in RB'Range (1) loop
            for C in RB'Range (2) loop
                Put (RB (R, C));
            end loop;
            New_Line;
        end loop;
    end Render_Buffer_To_Screen;

    procedure Update_Buffer (RB : in out Render_Buffer; Row, Col : Integer; Char : Character) is
    begin
        if Row in RB'Range (1) and Col in RB'Range (2) then
            RB (Row, Col) := Char;
        end if;
    end Update_Buffer;

    procedure Draw_Char (RB : in out Render_Buffer; Char : Character; X, Y : Integer) is
    begin
        if X in RB'Range (1) and Y in RB'Range (2) then
            RB (X, Y) := Char;
        end if;
    end Draw_Char;

    procedure Draw_String (RB : in out Render_Buffer; Str : String; X, Y : Integer) is
    begin
        for I in Str'Range loop
            if X in RB'Range (1) and (Y + I - Str'First) in RB'Range (2) then
                RB (X, Y + I - Str'First) := Str (I);
            end if;
        end loop;
    end Draw_String;

    procedure Draw_Border (RB : in out Render_Buffer; Style : Character; X1, Y1, X2, Y2 : Integer) is
    begin
        for X in X1 .. X2 loop
            if X in RB'Range (1) then
                if Y1 in RB'Range (2) and RB (X, Y1) = ' ' then
                    RB (X, Y1) := Style;
                end if;
                if Y2 in RB'Range (2) and RB (X, Y2) = ' ' then
                    RB (X, Y2) := Style;
                end if;
            end if;
        end loop;

        for Y in Y1 .. Y2 loop
            if Y in RB'Range (2) then
                if X1 in RB'Range (1) and RB (X1, Y) = ' ' then
                    RB (X1, Y) := Style;
                end if;
                if X2 in RB'Range (1) and RB (X2, Y) = ' ' then
                    RB (X2, Y) := Style;
                end if;
            end if;
        end loop;
    end Draw_Border;

end Render_Buffer;