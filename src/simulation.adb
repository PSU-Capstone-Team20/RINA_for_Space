with Ada.Text_IO;           use Ada.Text_IO;
with Ada.Real_Time;         use Ada.Real_Time;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Strings.Fixed;     use Ada.Strings.Fixed;
with Ada.Containers;        use Ada.Containers; 
with application;
with Render_Buffer;         use Render_Buffer;
with RIB;

package body simulation is

    procedure Run_NASA_DSN_Demo is
        temp : Unbounded_String;
    begin
        -- DIFs
        RIB.Add_Entry (To_Unbounded_String ("ISP DIF"));
        RIB.Add_Entry (To_Unbounded_String ("Earth Network DIF"));
        RIB.Add_Entry (To_Unbounded_String ("Deep Space Relay DIF"));
        RIB.Add_Entry (To_Unbounded_String ("Deep Space Probe DIF"));

        -- Computers
        temp := To_Unbounded_String ("ISP Server");
        RIB.Add_Comp (To_Unbounded_String ("ISP DIF"), temp);
        temp := To_Unbounded_String ("NASA Server");
        RIB.Add_Comp (To_Unbounded_String ("ISP DIF"), temp);
        temp := To_Unbounded_String ("Local Desktop");
        RIB.Add_Comp (To_Unbounded_String ("ISP DIF"), temp);

        temp := To_Unbounded_String ("Madrid Ground Station");
        RIB.Add_Comp (To_Unbounded_String ("Earth Network DIF"), temp);
        temp := To_Unbounded_String ("Canberra Ground Station");
        RIB.Add_Comp (To_Unbounded_String ("Earth Network DIF"), temp);
        temp := To_Unbounded_String ("Goldstone Ground Station");
        RIB.Add_Comp (To_Unbounded_String ("Earth Network DIF"), temp);

        temp := To_Unbounded_String ("Mars Odyssey");
        RIB.Add_Comp (To_Unbounded_String ("Deep Space Relay DIF"), temp);
        temp := To_Unbounded_String ("Mars Reconnaissance Orbiter");
        RIB.Add_Comp (To_Unbounded_String ("Deep Space Relay DIF"), temp);
        temp := To_Unbounded_String ("Maven Orbiter");
        RIB.Add_Comp (To_Unbounded_String ("Deep Space Relay DIF"), temp);

        temp := To_Unbounded_String ("Voyager 1");
        RIB.Add_Comp (To_Unbounded_String ("Deep Space Probe DIF"), temp);
        temp := To_Unbounded_String ("Voyager 2");
        RIB.Add_Comp (To_Unbounded_String ("Deep Space Probe DIF"), temp);

        -- IPCPs
        temp := To_Unbounded_String ("ISP IPCP");
        RIB.Add_IPCP
           (To_Unbounded_String ("ISP DIF"),
            To_Unbounded_String ("ISP Server"), temp);
        temp := To_Unbounded_String ("NASA IPCP");
        RIB.Add_IPCP
           (To_Unbounded_String ("ISP DIF"),
            To_Unbounded_String ("NASA Server"), temp);
        temp := To_Unbounded_String ("Local IPCP");
        RIB.Add_IPCP
           (To_Unbounded_String ("ISP DIF"),
            To_Unbounded_String ("Local Desktop"), temp);

        temp := To_Unbounded_String ("Madrid IPCP");
        RIB.Add_IPCP
           (To_Unbounded_String ("Earth Network DIF"),
            To_Unbounded_String ("Madrid Ground Station"), temp);
        temp := To_Unbounded_String ("Canberra IPCP");
        RIB.Add_IPCP
           (To_Unbounded_String ("Earth Network DIF"),
            To_Unbounded_String ("Canberra Ground Station"), temp);
        temp := To_Unbounded_String ("Goldstone IPCP");
        RIB.Add_IPCP
           (To_Unbounded_String ("Earth Network DIF"),
            To_Unbounded_String ("Goldstone Ground Station"), temp);

        temp := To_Unbounded_String ("Mars Odyssey IPCP");
        RIB.Add_IPCP
           (To_Unbounded_String ("Deep Space Relay DIF"),
            To_Unbounded_String ("Mars Odyssey"), temp);
        temp := To_Unbounded_String ("Mars Reconnaissance IPCP");
        RIB.Add_IPCP
           (To_Unbounded_String ("Deep Space Relay DIF"),
            To_Unbounded_String ("Mars Reconnaissance Orbiter"), temp);
        temp := To_Unbounded_String ("Maven IPCP");
        RIB.Add_IPCP
           (To_Unbounded_String ("Deep Space Relay DIF"),
            To_Unbounded_String ("Maven Orbiter"), temp);

        -- Create APNs

    end Run_NASA_DSN_Demo;

    procedure Start_Simulation is
        App          : application.application;
        Input        : String (1 .. 100);
        Last         : Natural;
        Data         : Unbounded_String;
        RB           : Render_Buffer.Render_Buffer;
        temp         : Unbounded_String;
        All_Difs     : RIB.DIF_Vectors.Vector;
        All_Comps    : RIB.Comp_Vectors.Vector; 
        Current_Menu : String (1 .. 4) := "DIF ";
        Current_DIF  : Unbounded_String := To_Unbounded_String("");
        Current_Comp : Unbounded_String := To_Unbounded_String(""); 
        Current_IPCP : Unbounded_String := To_Unbounded_String(""); 
        Current_APN  : Unbounded_String := To_Unbounded_String("");
        Exit_Simulation : Boolean := False; 
    begin
        Clear_Buffer (RB);
        Put (Clear_Screen);
        Put (Hide_Cursor);

        loop
            exit when Exit_Simulation; 

            Clear_Buffer (RB);
            Put (Clear_Screen);

            All_Difs := RIB.Get_All_DIFs;
            All_Comps := RIB.Get_All_Comps;

            if Current_Menu = "DIF " then
                declare
                    DIFRow : Integer := 22;
                    CPURow : Integer := 22;
                begin
                    for I in All_Difs.First_Index .. All_Difs.Last_Index loop
                        Draw_String
                           (RB, To_String (All_Difs (I)), 5, DIFRow);
                        DIFRow := DIFRow + 1;
                    end loop;

                  --     for I in All_Comps.First_Index .. All_Comps.Last_Index loop
                  --        Draw_String
                  --           (RB, To_String (All_Comps (I)), 44, CPURow);
                  --        CPURow := CPURow + 1;
                  --    end loop;
                end;
            elsif Current_Menu = "IPCP" then
                declare
                    DIFRow     : Integer := 22;
                    CPURow     : Integer := 22;
                    DIF_Entry  : RIB.RIB_Entry;
                    Comp_Map   : RIB.Comp_Hashed_Maps.Map;
                    IPCPs      : RIB.IPCP_Vectors.Vector;
                    APNs       : RIB.Application_Vectors.Vector; 
                begin
                    for I in All_Difs.First_Index .. All_Difs.Last_Index loop
                        if All_Difs (I) = Current_DIF then
                            Draw_String
                               (RB, "> " & To_String (All_Difs (I)), 5, DIFRow);
                        else
                            Draw_String
                               (RB, "  " & To_String (All_Difs (I)), 5, DIFRow);
                        end if;
                        DIFRow := DIFRow + 1;
                    end loop;

                    if RIB.Find_Entry(Current_DIF) then
                        DIF_Entry := RIB.Get_Entry(Current_DIF);
                        Comp_Map := DIF_Entry.Obj_Type;

                        for C in Comp_Map.Iterate loop
                            declare
                                Comp_Obj : RIB.RIB_Obj := Comp_Map(C);
                                Comp_Name_Str : String := To_String(Comp_Obj.Comp_Connection);
                                Divider : String(1 .. Comp_Name_Str'Length) := (others => '-');
                                Comp_Prefix : String := "  "; 
                            begin
                                if Comp_Obj.Comp_Connection = Current_Comp then
                                    Comp_Prefix := "> ";
                                end if;

                                -- Draw Computer Name with prefix
                                Draw_String(RB, Comp_Prefix & Comp_Name_Str, 44, CPURow);
                                CPURow := CPURow + 1;
                                -- Draw Divider
                                Draw_String(RB, Divider, 44 + Comp_Prefix'Length, CPURow);
                                CPURow := CPURow + 1;

                                -- Draw associated IPCPs
                                IPCPs := Comp_Obj.Obj_Obj_Type.Accessible_IPCPs;
                                if IPCPs.Length > 0 then
                                    Draw_String(RB, "  IPCPs:", 44 + Comp_Prefix'Length, CPURow);
                                    CPURow := CPURow + 1;
                                    for I in IPCPs.First_Index .. IPCPs.Last_Index loop
                                        declare
                                            IPCP_Str : Unbounded_String := IPCPs(I);
                                            IPCP_Prefix : String := "  - "; 
                                        begin
                                            if Comp_Obj.Comp_Connection = Current_Comp and then IPCP_Str = Current_IPCP then
                                                IPCP_Prefix := "  > ";
                                            end if;
                                            Draw_String(RB, IPCP_Prefix & To_String(IPCP_Str), 44 + Comp_Prefix'Length, CPURow);
                                            CPURow := CPURow + 1;
                                        end;
                                    end loop;
                                end if;

                                -- Draw associated APNs 
                                APNs := Comp_Obj.Obj_Obj_Type.Active_APNs;
                                if APNs.Length > 0 then
                                    Draw_String(RB, "  Applications:", 44 + Comp_Prefix'Length, CPURow);
                                    CPURow := CPURow + 1;
                                    for I in APNs.First_Index .. APNs.Last_Index loop
                                        declare
                                            APN_Str : Unbounded_String := APNs(I);
                                            APN_Prefix : String := "  - ";
                                        begin
                                            if Comp_Obj.Comp_Connection = Current_Comp and then APN_Str = Current_APN then
                                                APN_Prefix := "  > ";
                                            end if;
                                            Draw_String(RB, APN_Prefix & To_String(APN_Str), 44 + Comp_Prefix'Length, CPURow);
                                            CPURow := CPURow + 1;
                                        end;
                                    end loop;
                                end if;

                                CPURow := CPURow + 1; -- Add a blank line between computers
                            end;
                        end loop;
                    else
                        Draw_String(RB, "Selected DIF not found.", 44, CPURow);
                    end if;

                end;
            elsif Current_Menu (1 .. 3) = "CPU" then
                               declare
                    DIFRow     : Integer := 22;
                    CPURow     : Integer := 22;
                    DIF_Entry  : RIB.RIB_Entry;
                    Comp_Map   : RIB.Comp_Hashed_Maps.Map;
                    IPCPs      : RIB.IPCP_Vectors.Vector;
                    APNs       : RIB.Application_Vectors.Vector; 
                begin
                    for I in All_Difs.First_Index .. All_Difs.Last_Index loop
                        if All_Difs (I) = Current_DIF then
                            Draw_String
                               (RB, "> " & To_String (All_Difs (I)), 5, DIFRow);
                        else
                            Draw_String
                               (RB, "  " & To_String (All_Difs (I)), 5, DIFRow);
                        end if;
                        DIFRow := DIFRow + 1;
                    end loop;

                    if RIB.Find_Entry(Current_DIF) then
                        DIF_Entry := RIB.Get_Entry(Current_DIF);
                        Comp_Map := DIF_Entry.Obj_Type;

                        for C in Comp_Map.Iterate loop
                            declare
                                Comp_Obj : RIB.RIB_Obj := Comp_Map(C);
                                Comp_Name_Str : String := To_String(Comp_Obj.Comp_Connection);
                                Divider : String(1 .. Comp_Name_Str'Length) := (others => '-');
                            begin
                                -- Draw Computer Name
                                Draw_String(RB, Comp_Name_Str, 44, CPURow);
                                CPURow := CPURow + 1;
                                -- Draw Divider
                                Draw_String(RB, Divider, 44, CPURow);
                                CPURow := CPURow + 1;

                                -- Draw associated IPCPs
                                IPCPs := Comp_Obj.Obj_Obj_Type.Accessible_IPCPs;
                                if IPCPs.Length > 0 then
                                    Draw_String(RB, "IPCPs:", 44, CPURow);
                                    CPURow := CPURow + 1;
                                    for I in IPCPs.First_Index .. IPCPs.Last_Index loop
                                        Draw_String(RB, "- " & To_String(IPCPs(I)), 44, CPURow); 
                                        CPURow := CPURow + 1;
                                    end loop;
                                end if;

                                -- Draw associated APNs
                                APNs := Comp_Obj.Obj_Obj_Type.Active_APNs;
                                if APNs.Length > 0 then
                                    Draw_String(RB, "Applications:", 44, CPURow);
                                    CPURow := CPURow + 1;
                                    for I in APNs.First_Index .. APNs.Last_Index loop
                                        Draw_String(RB, "- " & To_String(APNs(I)), 44, CPURow); 
                                        CPURow := CPURow + 1;
                                    end loop;
                                end if;

                                CPURow := CPURow + 1; -- Add some space between computers
                            end;
                        end loop;
                    else
                        Draw_String(RB, "Selected DIF not found.", 44, CPURow);
                    end if;

                end;
            end if;

            Load_Main_Display (RB, Current_Menu);

            Get_Line (Input, Last);

            if Current_Menu (1 .. 3) = "DIF" then
                case Input (1) is
                    -- Create DIF
                    when '1' =>
                        declare
                            Input_Line : String (1 .. 100);
                            Len        : Natural;
                        begin
                            Put ("Enter DIF Name: ");
                            Get_Line (Input_Line, Len);
                              Current_DIF  :=
                                 To_Unbounded_String (Input_Line (1 .. Len));
                            RIB.Add_Entry
                               (Current_DIF);
                            Current_Menu := "CPU ";
                        end;
                    when '2' =>
                        null; -- Modify DIF
                    when '3' =>
                        -- Delete DIF
                        declare
                            Input_Line : String (1 .. 100);
                            Len        : Natural;
                            DIF_To_Delete : Unbounded_String;
                        begin
                            Put ("Enter DIF Name to delete: ");
                            Get_Line (Input_Line, Len);
                            DIF_To_Delete := To_Unbounded_String (Input_Line (1 .. Len));
                            RIB.Delete_Entry (DIF_To_Delete); 
                            Current_Menu := "CPU "; 
                        end;
                    when '5' =>
                        null; -- Computer Menu
                    -- Select DIF
                    when '4' =>
                        declare
                            Input_Line : String (1 .. 100);
                            Len        : Natural;
                        begin
                            Put ("Enter DIF Name: ");
                            Get_Line (Input_Line, Len);
                            Current_DIF  :=
                               To_Unbounded_String (Input_Line (1 .. Len));
                            Current_Menu := "CPU "; 
                        end;
                    when '9' =>
                        Run_NASA_DSN_Demo;
                    when '0' => 
                        Exit_Simulation := True;
                    when others =>
                        Put_Line ("Invalid DIF option. Please try again.");
                end case;
            elsif Current_Menu = "IPCP" then
                case Input (1) is
                    when '1' =>
                        -- Create IPCP
                        declare
                            Input_Line : String (1 .. 100);
                            Len        : Natural;
                            IPCP_Name  : Unbounded_String; 
                        begin
                            Put ("Enter IPCP Name: ");
                            Get_Line (Input_Line, Len);
                            IPCP_Name := To_Unbounded_String (Input_Line (1 .. Len)); 
                            -- Current_IPCP := IPCP_Name;
                            RIB.Add_IPCP
                               (Current_DIF,
                                Current_Comp,
                                IPCP_Name); 
                        end;
                    when '2' =>
                        -- Delete IPCP
                        declare
                            Input_Line : String (1 .. 100);
                            Len        : Natural;
                            IPCP_To_Delete : Unbounded_String;
                        begin
                            Put ("Enter IPCP Name to delete: ");
                            Get_Line (Input_Line, Len);
                            IPCP_To_Delete := To_Unbounded_String (Input_Line (1 .. Len));
                            RIB.Delete_IPCP_By_Name -- Use the new procedure
                               (Current_DIF,
                                Current_Comp,
                                IPCP_To_Delete);
                        end;
                    when '3' =>
                        null; -- Modify IPCP
                    when '4' =>
                        -- Select IPCP
                        --  declare
                        --      Input_Line : String (1 .. 100);
                        --      Len        : Natural;
                        --  begin
                        --      Put ("Enter IPCP Name: ");
                        --      Get_Line (Input_Line, Len);
                        --      -- Current_IPCP  :=
                        --         To_Unbounded_String (Input_Line (1 .. Len));
                        --      Current_Menu := "IPCP"; 
                        --  end;
                        null;
                    when '5' =>
                        -- Create APN
                        declare
                            Input_Line : String (1 .. 100);
                            Len        : Natural;
                            APN_Name   : Unbounded_String;
                        begin
                            Put ("Enter APN Name: ");
                            Get_Line (Input_Line, Len);
                            APN_Name := To_Unbounded_String (Input_Line (1 .. Len)); 
                            Current_APN := APN_Name;
                            RIB.Add_APN
                               (Current_DIF,
                                Current_Comp,
                                Current_APN);
                        end;
                     when '6' =>
                        -- Delete APN
                        declare
                            Input_Line : String (1 .. 100);
                            Len        : Natural;
                            APN_To_Delete : Unbounded_String;
                        begin
                            Put ("Enter APN Name to delete: ");
                            Get_Line (Input_Line, Len);
                            APN_To_Delete := To_Unbounded_String (Input_Line (1 .. Len));
                            RIB.Delete_APN_By_Name
                               (Current_DIF,
                                Current_Comp,
                                APN_To_Delete);
                        end;
                        when '8' =>
                        -- Select APN
                        declare
                            Input_Line : String (1 .. 100);
                            Len        : Natural;
                            APN_To_Select : Unbounded_String;
                            DIF_Entry_Record : RIB.RIB_Entry;
                            Comp_Obj : RIB.RIB_Obj;
                            APNs : RIB.Application_Vectors.Vector;
                            Found : Boolean := False;
                        begin
                            Put ("Enter APN Name to select: ");
                            Get_Line (Input_Line, Len);
                            APN_To_Select := To_Unbounded_String (Input_Line (1 .. Len));

                            if RIB.Find_Entry(Current_DIF) then
                                DIF_Entry_Record := RIB.Get_Entry(Current_DIF);
                                if DIF_Entry_Record.Obj_Type.Contains(Current_Comp) then
                                    Comp_Obj := DIF_Entry_Record.Obj_Type(Current_Comp);
                                    APNs := Comp_Obj.Obj_Obj_Type.Active_APNs;
                                    for I in APNs.First_Index .. APNs.Last_Index loop
                                        if APNs(I) = APN_To_Select then
                                            Found := True;
                                            exit;
                                        end if;
                                    end loop;
                                end if;
                            end if;

                            if Found then
                                Current_APN := APN_To_Select;
                                Put_Line("Selected APN: " & To_String(Current_APN));
                            else
                                Put_Line("Error: APN '" & To_String(APN_To_Select) & "' not found on computer '" & To_String(Current_Comp) & "'.");
                                Current_APN := To_Unbounded_String(""); 
                            end if;
                        end;
                    when '9' =>
                        Run_NASA_DSN_Demo;
                    when '0' =>
                        Current_Menu := "CPU ";
                        Current_Comp := To_Unbounded_String ("");
                        Current_IPCP := To_Unbounded_String ("");
                        Current_APN  := To_Unbounded_String ("");
                    when others =>
                        Put_Line ("Invalid IPCP option. Please try again.");
                end case;
            elsif Current_Menu (1 .. 3) = "CPU" then
                case Input (1) is
                    when '1' =>
                        declare
                            Input_Line : String (1 .. 100);
                            Len        : Natural;
                        begin
                            Put ("Enter Computer Name: ");
                            Get_Line (Input_Line, Len);
                            Current_Comp  :=
                               To_Unbounded_String (Input_Line (1 .. Len));
                            RIB.Add_Comp (Current_DIF, Current_Comp);
                            Current_Menu := "IPCP"; 
                        end;
                    when '2' =>
                        -- Delete Computer
                        declare
                            Input_Line : String (1 .. 100);
                            Len        : Natural;
                            Comp_To_Delete : Unbounded_String;
                        begin
                              Put ("Enter Computer Name to delete: ");
                              Get_Line (Input_Line, Len);
                              Comp_To_Delete  :=
                                 To_Unbounded_String (Input_Line (1 .. Len));
                              RIB.Delete_Comp (Current_DIF, Comp_To_Delete);
                              if Current_Comp = Comp_To_Delete then
                                 Current_Comp := To_Unbounded_String("");
                              end if;
                              Current_Menu := "CPU "; -- Stay in CPU menu
                           end;
                    when '3' =>
                        null; -- Modify Computer
                    when '4' => -- Select Computer
                        declare
                            Input_Line : String (1 .. 100);
                            Len        : Natural;
                        begin
                            Put ("Enter Computer Name: ");
                            Get_Line (Input_Line, Len);
                            Current_Comp  :=
                               To_Unbounded_String (Input_Line (1 .. Len));
                            Current_Menu := "IPCP"; 
                        end;
                    when '9' =>
                        Run_NASA_DSN_Demo;
                    when '0' => -- Go back to DIF menu
                        Current_Menu := "DIF ";
                        Current_DIF := To_Unbounded_String("");
                        Current_Comp := To_Unbounded_String("");
                    when others =>
                        Put_Line
                           ("Invalid Computer option. Please try again.");
                end case;
            else
                case Input (1) is
                    when '9' =>
                        Run_NASA_DSN_Demo;
                    when '0' => 
                        Exit_Simulation := True;
                    when others =>
                        Put_Line ("Invalid option. Please try again.");
                end case;
            end if;
        end loop;

        Put_Line ("Press Enter to exit...");
        Get_Line (Input, Last);

    end Start_Simulation;

end simulation;
