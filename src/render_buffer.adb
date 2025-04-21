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

    function Clear_Screen return String is
    begin
        return (ASCII.ESC & "[2J" & ASCII.ESC & "[H"); -- ANSI escape code to clear the screen
    end Clear_Screen;

    function Hide_Cursor return String is
    begin
        return (ASCII.ESC & "[?25l");
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
        -- DEBUG
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
            RB(C.Y, C.X) := ' '; 
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

    procedure Load_Main_Display (RB : in out Render_Buffer; Current_Menu : String) is
    begin
            Draw_Border (RB, '|', '=', 1, 1, 83, 60);
            Draw_RinaForSpace (RB, 5, 2); 

            -- The system must be able to create a Resource Information Base (RIB)
            -- The system must be able to discover new DIFs
            -- The system must be able to discover new IPCPs

            if Current_Menu(1..3) = "DIF" then 
               
               Draw_String (RB, "Network Management", 5, 8);
               Draw_Line (RB, '=', 5, 9, 40, 9);

               -- DIF Management Menu
               Draw_String (RB, "DIF Management", 5, 10);
               -- The system must be able to create DIFs
               Draw_String (RB, "1. Create DIF", 5, 11);
               -- The system must be able to modify DIFs
               -- Draw_String (RB, "2. Modify DIF", 5, 12);
               -- The system must be able to delete DIFs
               Draw_String (RB, "3. Delete DIF", 5, 13);
               Draw_String (RB, "4. Select DIF", 5, 14);

               -- DEMO
               Draw_String(RB, "Demo", 63, 8);
               Draw_Line (RB, '=', 63, 9, 74, 9);
               Draw_String (RB, "9. NASA DSN", 63, 10);

            elsif Current_Menu = "IPCP" then
               Draw_String (RB, "Network Management", 5, 8);
               Draw_Line (RB, '=', 5, 9, 40, 9);

               Draw_String (RB, "IPCP Management", 5, 10);
               Draw_String (RB, "1. Create IPCP", 5, 11);
               Draw_String (RB, "2. Delete IPCP", 5, 12);
               -- Draw_String (RB, "3. Modify IPCP", 5, 13);
               -- Draw_String (RB, "4. Select IPCP", 5, 14);

               Draw_String (RB, "Application Management", 26, 10);
               Draw_String (RB, "5. Create Application", 26, 11);
               Draw_String (RB, "6. Delete Application", 26, 12);
               -- Draw_String (RB, "7. Modify Application", 26, 13);
               Draw_String (RB, "8. Select Application", 26, 14);

               Draw_String(RB, "Demo", 63, 8);
               Draw_Line (RB, '=', 63, 9, 74, 9);
               Draw_String (RB, "9. NASA DSN", 63, 10);

               Draw_String(RB, "0. Back", 63, 13);

            elsif Current_Menu(1..3) = "CPU" then
               Draw_String (RB, "Network Management", 5, 8);
               Draw_Line (RB, '=', 5, 9, 40, 9);
               
               -- Computer Management Menu
               Draw_String (RB, "Computer Management", 5, 10);
               Draw_String (RB, "1. Create Computer", 5, 11);
               Draw_String (RB, "2. Delete Computer", 5, 12);
               -- Draw_String (RB, "3. Modify Computer", 5, 13);
               Draw_String (RB, "4. Select Computer", 5, 14);

               -- DEMO
               Draw_String(RB, "Demo", 63, 8);
               Draw_Line (RB, '=', 63, 9, 74, 9);
               Draw_String (RB, "9. NASA DSN", 63, 10);

               Draw_String(RB, "0. Back", 63, 13);
            end if;

            -- Network Management
            -- The system will allow a user to create an application to connect to the network.
            -- The system shall enable a newly created application to connect to the network.
            -- The system shall be adaptable to future hardware.
            -- The system must be able to disconnect from DIFs
            -- The system must be able to connect to new DIFs
            -- The system must be able to create IPCPs
            -- The system must be able to delete IPCPs

            -- DATA MANAGEMENT
            -- The system must have acknowledgements.
            -- The system must be able to buffer multiple pending acknowledgements within a DIF.
            -- The system must transmit data between DIFs from origin to destination.
            -- The system should be able to transmit data repeatedly
            -- The system should be able to transmit data bidirectionally
            -- The system must be able to temporarily store data in a DIF
            -- The system must be able to transmit stored data

            -- ANOMALY HANDLING
            -- The system must retry failed communications.
            -- The system must time out failed communications after 10 retries.
            -- The system must be able to use provided data to communicate past obstacles.

            -- PAGE PANEL
            Draw_Line (RB,'-', 3, 16, 81, 16);
            Draw_Line (RB,'-', 3, 18, 81, 18);

            -- DIFS
            Draw_String (RB, "DIFs", 5, 20);
            Draw_Line (RB, '=', 5, 21, 39, 21);

            -- IPCPs
            Draw_String (RB, "Computers", 44, 20);
            Draw_Line (RB, '=', 44, 21, 80, 21);

            -- APNs
            -- Draw_String (RB, "APNs", 44, 38);
            -- Draw_Line (RB, '=', 44, 39, 80, 39);

            Render_Buffer_To_Screen (RB);
    end Load_Main_Display;

end Render_Buffer;