with Ada.Text_IO;           use Ada.Text_IO;
with Ada.Real_Time;         use Ada.Real_Time;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Strings.Fixed;     use Ada.Strings.Fixed;
with application;
with Render_Buffer;         use Render_Buffer;
with RIB;

package body simulation is

    procedure Run_NASA_DSN_Demo is
        temp         : Unbounded_String;
    begin
        -- DIFs
        RIB.Add_Entry (To_Unbounded_String ("ISP DIF"));
        RIB.Add_Entry (To_Unbounded_String ("Earth Network DIF"));
        RIB.Add_Entry (To_Unbounded_String ("Deep Space Relay DIF"));
        RIB.Add_Entry (To_Unbounded_String ("Deep Space Probe DIF"));

        -- TODO: REBASE AND CHANGE TO GLENN's Ccomputer model
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
        Current_Menu : String (1 .. 4) := "DIF ";
        Current_DIF  : Unbounded_String;
    begin
        Clear_Buffer (RB);
        Put (Clear_Screen);
        Put (Hide_Cursor);

        loop
            Clear_Buffer (RB);
            Put (Clear_Screen);

            All_Difs := RIB.Get_All_DIFs;

            declare
                PrintRow : Integer := 22;
            begin
                for I in All_Difs.First_Index .. All_Difs.Last_Index loop
                    Draw_String (RB, To_String (All_Difs (I)), 5, PrintRow);
                    PrintRow := PrintRow + 1;
                end loop;
            end;

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
                            RIB.Add_Entry
                               (To_Unbounded_String (Input_Line (1 .. Len)));
                        end;
                    when '2' =>
                        null; -- Modify DIF
                    when '3' =>
                        null; -- Delete DIF
                    when '4' =>
                        declare
                            Input_Line : String (1 .. 100);
                            Len        : Natural;
                        begin
                            Put ("Enter DIF Name: ");
                            Get_Line (Input_Line, Len);
                            RIB.Add_Entry
                               (To_Unbounded_String (Input_Line (1 .. Len)));
                        end;
                    when '9' =>
                        Run_NASA_DSN_Demo;
                    when others =>
                        Put_Line ("Invalid DIF option. Please try again.");
                end case;
            elsif Current_Menu = "IPCP" then
                case Input (1) is
                    when '1' =>
                        null; -- Create IPCP
                    when '2' =>
                        null; -- Delete IPCP
                    when '3' =>
                        null; -- Modify IPCP
                    when '4' =>
                        null; -- Select IPCP
                    when '5' =>
                        null; -- DIF Menu
                    when '9' =>
                        Run_NASA_DSN_Demo;
                    when others =>
                        Put_Line ("Invalid IPCP option. Please try again.");
                end case;
            elsif Current_Menu (1 .. 3) = "CPU" then
                case Input (1) is
                    when '1' =>
                        null; -- Create Computer
                    when '2' =>
                        null; -- Delete Computer
                    when '3' =>
                        null; -- Modify Computer
                    when '4' =>
                        null; -- Select Computer
                    when '5' =>
                        null; -- IPCP Menu
                    when '9' =>
                        Run_NASA_DSN_Demo;
                    when others =>
                        Put_Line
                           ("Invalid Computer option. Please try again.");
                end case;
            elsif Current_Menu (1 .. 3) = "APN" then
                case Input (1) is
                    when '1' =>
                        null; -- Create APN
                    when '2' =>
                        null; -- Delete APN
                    when '3' =>
                        null; -- Modify APN
                    when '4' =>
                        null; -- Select APN
                    when '5' =>
                        null; -- Computer Menu
                    when '7' =>
                        null; -- Transmit Data
                    when '8' =>
                        null; -- Service Outage
                    when '9' =>
                        Run_NASA_DSN_Demo;
                    when others =>
                        Put_Line ("Invalid APN option. Please try again.");
                end case;
            else
                -- Main menu or other pages
                case Input (1) is
                    when '9' =>
                        Run_NASA_DSN_Demo;
                    when others =>
                        Put_Line ("Invalid option. Please try again.");
                end case;
            end if;
        end loop;

        -- Wait for user input before exiting
        Put_Line ("Press Enter to exit...");
        Get_Line (Input, Last);

    end Start_Simulation;

end simulation;

-- Old code for menu logic, kept for reference
--  case Input (1) is
--      when '1' =>
--          declare
--              Input_Line : String(1..100);
--              Last       : Natural;
--              Filter_Str : Unbounded_String;
--              All_DIFS   : RIB.DIF_Vectors.Vector := RIB.Get_All_DIFs;
--          begin
--              Put("Enter DIF Name: ");
--              Get_Line(Input_Line, Last);
--              RIB.Add_Entry(To_Unbounded_String(Input_Line(1..Last)));
--          end;
--      when '2' =>
--          null;
--      when '3' =>
--          -- Print all DIFs in the RIB
--          declare
--              All_Difs : RIB.DIF_Vectors.Vector := RIB.Get_All_DIFs;
--          begin
--              Put_Line("DIFs in RIB:");
--              for I in All_Difs.First_Index .. All_Difs.Last_Index loop
--                  Put_Line(" - " & To_String(All_Difs(I)));
--              end loop;
--          end;
--      when '4' =>
--          declare
--              Input_Line : String(1..100);
--              Len2       : Natural;
--              Filter_Str : Unbounded_String;
--              All_DIFS   : RIB.DIF_Vectors.Vector := RIB.Get_All_DIFs;
--          begin
--              Put("Enter filter string: ");
--              Get_Line(Input_Line, Len2);
--              Filter_Str := To_Unbounded_String(Input_Line(1..Len2));
--              Put_Line("Computers under matching DIFs:");
--              for I in All_DIFS.First_Index .. All_DIFS.Last_Index loop
--                  if Index(To_String(All_DIFS(I)), To_String(Filter_Str)) > 0 then
--                      declare
--                         REntry : RIB.RIB_Entry := RIB.Get_Entry(All_DIFS(I));
--                      begin
--                         for CompC in REntry.Obj_Type.Iterate loop
--                            Put_Line(" - " & To_String(REntry.Obj_Type(CompC).Comp_Connection));
--                         end loop;
--                      end;
--                  end if;
--              end loop;
--          end;
--      when '9' => -- NASA DSN DEMO
--          -- DIFs
--          RIB.Add_Entry(To_Unbounded_String("ISP DIF"));
--          RIB.Add_Entry(To_Unbounded_String("Earth Network DIF"));
--          RIB.Add_Entry(To_Unbounded_String("Deep Space Relay DIF"));
--          RIB.Add_Entry(To_Unbounded_String("Deep Space Probe DIF"));

--          -- TODO: REBASE AND CHANGE TO GLENN's Ccomputer model
--          -- Computers
--          temp := To_Unbounded_String("ISP Server");
--          RIB.Add_Comp(To_Unbounded_String("ISP DIF"), temp);
--          temp := To_Unbounded_String("NASA Server");
--          RIB.Add_Comp(To_Unbounded_String("ISP DIF"), temp);
--          temp := To_Unbounded_String("Local Desktop");
--          RIB.Add_Comp(To_Unbounded_String("ISP DIF"), temp);

--          temp := To_Unbounded_String("Madrid Ground Station");
--          RIB.Add_Comp(To_Unbounded_String("Earth Network DIF"), temp);
--          temp := To_Unbounded_String("Canberra Ground Station");
--          RIB.Add_Comp(To_Unbounded_String("Earth Network DIF"), temp);
--          temp := To_Unbounded_String("Goldstone Ground Station");
--          RIB.Add_Comp(To_Unbounded_String("Earth Network DIF"), temp);

--          temp := To_Unbounded_String("Mars Odyssey");
--          RIB.Add_Comp(To_Unbounded_String("Deep Space Relay DIF"), temp);
--          temp := To_Unbounded_String("Mars Reconnaissance Orbiter");
--          RIB.Add_Comp(To_Unbounded_String("Deep Space Relay DIF"), temp);
--          temp := To_Unbounded_String("Maven Orbiter");
--          RIB.Add_Comp(To_Unbounded_String("Deep Space Relay DIF"), temp);

--          temp := To_Unbounded_String("Voyager 1");
--          RIB.Add_Comp(To_Unbounded_String("Deep Space Probe DIF"), temp);
--          temp := To_Unbounded_String("Voyager 2");
--          RIB.Add_Comp(To_Unbounded_String("Deep Space Probe DIF"), temp);

--          -- IPCPs
--          temp := To_Unbounded_String("ISP IPCP");
--          RIB.Add_IPCP (To_Unbounded_String("ISP DIF"), To_Unbounded_String("ISP Server"), temp);
--          temp := To_Unbounded_String("NASA IPCP");
--          RIB.Add_IPCP (To_Unbounded_String("ISP DIF"), To_Unbounded_String("NASA Server"), temp);
--          temp := To_Unbounded_String("Local IPCP");
--          RIB.Add_IPCP (To_Unbounded_String("ISP DIF"), To_Unbounded_String("Local Desktop"), temp);

--          temp := To_Unbounded_String("Madrid IPCP");
--          RIB.Add_IPCP (To_Unbounded_String("Earth Network DIF"), To_Unbounded_String("Madrid Ground Station"), temp);
--          temp := To_Unbounded_String("Canberra IPCP");
--          RIB.Add_IPCP (To_Unbounded_String("Earth Network DIF"), To_Unbounded_String("Canberra Ground Station"), temp);
--          temp := To_Unbounded_String("Goldstone IPCP");
--          RIB.Add_IPCP (To_Unbounded_String("Earth Network DIF"), To_Unbounded_String("Goldstone Ground Station"), temp);

--          temp := To_Unbounded_String("Mars Odyssey IPCP");
--          RIB.Add_IPCP (To_Unbounded_String("Deep Space Relay DIF"), To_Unbounded_String("Mars Odyssey"), temp);
--          temp := To_Unbounded_String("Mars Reconnaissance IPCP");
--          RIB.Add_IPCP (To_Unbounded_String("Deep Space Relay DIF"), To_Unbounded_String("Mars Reconnaissance Orbiter"), temp);
--          temp := To_Unbounded_String("Maven IPCP");
--          RIB.Add_IPCP (To_Unbounded_String("Deep Space Relay DIF"), To_Unbounded_String("Maven Orbiter"), temp);

--          -- Create APNs

--          -- DEBUG MAP DISPLAY:
--          RIB.Display_Map;

--          -- TODO: Change print lines to use Render_Buffer

--      when others =>
--          Put_Line ("Invalid option. Please try again.");
--  end case;
