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
        if Y in RB'Range (1) and X in RB'Range (2) then
            RB (Y, X) := Char;
        end if;
    end Draw_Char;

    procedure Draw_String (RB : in out Render_Buffer; Str : String; X, Y : Integer) is
    begin
        for I in Str'Range loop
            if Y in RB'Range (1) and (X + I - Str'First) in RB'Range (2) then
                RB (Y, X + I - Str'First) := Str (I);
            end if;
        end loop;
    end Draw_String;

    procedure Draw_RinaForSpace (RB : in out Render_Buffer; X, Y : Integer) is
    begin
        -- Generator: https://www.asciiart.eu/text-to-ascii-art
        Draw_String(RB, " ____  ___ _   _    _       __              ____  ____   _    ____ _____ ", X, Y);
        Draw_String(RB, "|  _ \|_ _| \ | |  / \     / _| ___  _ __  / ___||  _ \ / \  / ___| ____|", X, Y + 1);
        Draw_String(RB, "| |_) || ||  \| | / _ \   | |_ / _ \| '__| \___ \| |_) / _ \| |   |  _|  ", X, Y + 2);
        Draw_String(RB, "|  _ < | || |\  |/ ___ \  |  _| (_) | |     ___) |  __/ ___ \ |___| |___ ", X, Y + 3);
        Draw_String(RB, "|_| \_\___|_| \_/_/   \_\ |_|  \___/|_|    |____/|_| /_/   \_\____|_____|", X, Y + 4);
    end Draw_RinaForSpace;

    procedure Draw_Border (RB : in out Render_Buffer; Vertical, Horizontal : Character; X1, Y1, X2, Y2 : Integer) is
    begin
        -- Horizontal
        for X in X1 .. X2 loop
            if X in RB'Range (2) then
                if Y1 in RB'Range (1) and RB (Y1, X) = ' ' then
                    RB (Y1, X) := Horizontal;
                end if;
                if Y2 /= Y1 and Y2 in RB'Range (1) and RB (Y2, X) = ' ' then
                    RB (Y2, X) := Horizontal;
                end if;
            end if;
        end loop;
        
        -- Vertical
        for Y in Y1 .. Y2 loop
            if Y in RB'Range (1) then
                if X1 in RB'Range (2) and RB (Y, X1) = ' ' then
                    RB (Y, X1) := Vertical;
                end if;
                if X2 /= X1 and X2 in RB'Range (2) and RB (Y, X2) = ' ' then
                    RB (Y, X2) := Vertical;
                end if;
            end if;
        end loop;
    end Draw_Border;

    procedure Draw_Line (RB : in out Render_Buffer; Char : Character; X1, Y1, X2, Y2 : Integer) is
    begin
        if Y1 = Y2 then

            -- Horizontal
            for X in X1 .. X2 loop
                if X in RB'Range(2) and Y1 in RB'Range(1) then
                    RB(Y1, X) := Char;
                end if;
            end loop;
        elsif X1 = X2 then

            -- Vertical
            for Y in Y1 .. Y2 loop
                if Y in RB'Range(1) and X1 in RB'Range(2) then
                    RB(Y, X1) := Char;
                end if;
            end loop;
        end if;
    end Draw_Line;

    -- TODO: Look over Oliviers example to get this to work...
    function Clear_Screen return String is
    begin
        return "\033[2J"; -- ANSI escape code to clear the screen
    end Clear_Screen;

    -- TODO: Look over Oliviers example to get this to work...
    function Hide_Cursor return String is
    begin
        return "\033[?25l"; -- ANSI escape code to hide the cursor
    end Hide_Cursor;

    procedure Move_Cursor (RB : in out Render_Buffer; C : in out Cursor; New_X, New_Y : Integer) is
    begin
        if New_Y in RB'Range(1) and New_X in RB'Range(2) then
            C.X := New_X;
            C.Y := New_Y;
        end if;
    end Move_Cursor;

    function Get_Cursor_Position (C : Cursor) return String is
    begin
        return "X: " & Integer'Image(C.X) & ", Y: " & Integer'Image(C.Y);
    end Get_Cursor_Position;

    procedure Add_Cursor_To_Buffer (RB : in out Render_Buffer; C : Cursor) is
    begin
        if C.Y in RB'Range(1) and C.X in RB'Range(2) then
            RB(C.Y, C.X) := C.Char;
        end if;
    end Add_Cursor_To_Buffer;

    procedure Clear_Cursor_From_Buffer (RB : in out Render_Buffer; C : in out Cursor) is
    begin
        if C.Y in RB'Range(1) and C.X in RB'Range(2) then
            RB(C.Y, C.X) := ' '; -- Clear the cursor character
        end if;
        C.X := 0;
        C.Y := 0;
    end Clear_Cursor_From_Buffer;

    procedure Set_Initial_Cursor_Position (RB : in out Render_Buffer; C : in out Cursor; X, Y : Integer; Cursor_Char : Character) is
    begin
        if X in RB'Range(2) and Y in RB'Range(1) then
            C.X := X;
            C.Y := Y;
            C.Char := Cursor_Char;
        else
            C.X := 0;
            C.Y := 0;
        end if;
    end Set_Initial_Cursor_Position;

end Render_Buffer;