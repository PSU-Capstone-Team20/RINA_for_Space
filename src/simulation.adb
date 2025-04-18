with Ada.Text_IO;           use Ada.Text_IO;
with Ada.Real_Time;         use Ada.Real_Time;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Strings.Fixed;     use Ada.Strings.Fixed;
with application;
with Render_Buffer;         use Render_Buffer;
with RIB;

package body simulation is

    procedure Start_Simulation is

        App   : application.application;
        Input : String (1 .. 100);
        Last  : Natural;
        Data  : Unbounded_String;
        RB    : Render_Buffer.Render_Buffer;
        temp : Unbounded_String;

    begin
        Clear_Buffer (RB);
        Put(Clear_Screen);
        Put(Hide_Cursor);

        loop
            Put(Clear_Screen);

            -- Get list of all current DIFs




            Load_Main_Display (RB);

            Get_Line(Input, Last);

            case Input (1) is
                when '1' =>
                    null;
                when '2' =>
                    null;
                when '3' =>
                    -- Print all DIFs in the RIB
                    declare
                        All_Difs : RIB.DIF_Vectors.Vector := RIB.Get_All_DIFs;
                    begin
                        Put_Line("DIFs in RIB:");
                        for I in All_Difs.First_Index .. All_Difs.Last_Index loop
                            Put_Line(" - " & To_String(All_Difs(I)));
                        end loop;
                    end;
                when '4' =>
                    declare
                        Input_Line : String(1..100);
                        Len2       : Natural;
                        Filter_Str : Unbounded_String;
                        All_DIFS   : RIB.DIF_Vectors.Vector := RIB.Get_All_DIFs;
                    begin
                        Put("Enter filter string: ");
                        Get_Line(Input_Line, Len2);
                        Filter_Str := To_Unbounded_String(Input_Line(1..Len2));
                        Put_Line("Computers under matching DIFs:");
                        for I in All_DIFS.First_Index .. All_DIFS.Last_Index loop
                            if Index(To_String(All_DIFS(I)), To_String(Filter_Str)) > 0 then
                                declare
                                   REntry : RIB.RIB_Entry := RIB.Get_Entry(All_DIFS(I));
                                begin
                                   for CompC in REntry.Obj_Type.Iterate loop
                                      Put_Line(" - " & To_String(REntry.Obj_Type(CompC).Comp_Connection));
                                   end loop;
                                end;
                            end if;
                        end loop;
                    end;
                when '9' => -- NASA DSN DEMO
                    -- DIFs
                    RIB.Add_Entry(To_Unbounded_String("ISP DIF"));
                    RIB.Add_Entry(To_Unbounded_String("Earth Network DIF"));
                    RIB.Add_Entry(To_Unbounded_String("Deep Space Relay DIF"));
                    RIB.Add_Entry(To_Unbounded_String("Deep Space Probe DIF"));     
                    
                    -- TODO: REBASE AND CHANGE TO GLENN's Ccomputer model
                    -- Computers
                    temp := To_Unbounded_String("ISP Server");
                    RIB.Add_Comp(To_Unbounded_String("ISP DIF"), temp);
                    temp := To_Unbounded_String("NASA Server");
                    RIB.Add_Comp(To_Unbounded_String("ISP DIF"), temp);
                    temp := To_Unbounded_String("Local Desktop");
                    RIB.Add_Comp(To_Unbounded_String("ISP DIF"), temp);

                    temp := To_Unbounded_String("Madrid Ground Station");
                    RIB.Add_Comp(To_Unbounded_String("Earth Network DIF"), temp);
                    temp := To_Unbounded_String("Canberra Ground Station");
                    RIB.Add_Comp(To_Unbounded_String("Earth Network DIF"), temp);
                    temp := To_Unbounded_String("Goldstone Ground Station");
                    RIB.Add_Comp(To_Unbounded_String("Earth Network DIF"), temp);

                    temp := To_Unbounded_String("Mars Odyssey");
                    RIB.Add_Comp(To_Unbounded_String("Deep Space Relay DIF"), temp);
                    temp := To_Unbounded_String("Mars Reconnaissance Orbiter");
                    RIB.Add_Comp(To_Unbounded_String("Deep Space Relay DIF"), temp);
                    temp := To_Unbounded_String("Maven Orbiter");
                    RIB.Add_Comp(To_Unbounded_String("Deep Space Relay DIF"), temp);
                    
                    temp := To_Unbounded_String("Voyager 1");
                    RIB.Add_Comp(To_Unbounded_String("Deep Space Probe DIF"), temp);
                    temp := To_Unbounded_String("Voyager 2");
                    RIB.Add_Comp(To_Unbounded_String("Deep Space Probe DIF"), temp);
                    
                    -- IPCPs
                    temp := To_Unbounded_String("ISP IPCP");
                    RIB.Add_IPCP (To_Unbounded_String("ISP DIF"), To_Unbounded_String("ISP Server"), temp);
                    temp := To_Unbounded_String("NASA IPCP");
                    RIB.Add_IPCP (To_Unbounded_String("ISP DIF"), To_Unbounded_String("NASA Server"), temp);
                    temp := To_Unbounded_String("Local IPCP");
                    RIB.Add_IPCP (To_Unbounded_String("ISP DIF"), To_Unbounded_String("Local Desktop"), temp);

                    temp := To_Unbounded_String("Madrid IPCP");
                    RIB.Add_IPCP (To_Unbounded_String("Earth Network DIF"), To_Unbounded_String("Madrid Ground Station"), temp);
                    temp := To_Unbounded_String("Canberra IPCP");
                    RIB.Add_IPCP (To_Unbounded_String("Earth Network DIF"), To_Unbounded_String("Canberra Ground Station"), temp);
                    temp := To_Unbounded_String("Goldstone IPCP");
                    RIB.Add_IPCP (To_Unbounded_String("Earth Network DIF"), To_Unbounded_String("Goldstone Ground Station"), temp);

                    temp := To_Unbounded_String("Mars Odyssey IPCP");
                    RIB.Add_IPCP (To_Unbounded_String("Deep Space Relay DIF"), To_Unbounded_String("Mars Odyssey"), temp);
                    temp := To_Unbounded_String("Mars Reconnaissance IPCP");
                    RIB.Add_IPCP (To_Unbounded_String("Deep Space Relay DIF"), To_Unbounded_String("Mars Reconnaissance Orbiter"), temp);
                    temp := To_Unbounded_String("Maven IPCP");
                    RIB.Add_IPCP (To_Unbounded_String("Deep Space Relay DIF"), To_Unbounded_String("Maven Orbiter"), temp);

                    -- Create APNs
                    

                    -- DEBUG MAP DISPLAY:
                    RIB.Display_Map;

                    -- TODO: Change print lines to use Render_Buffer

                when others =>
                    Put_Line ("Invalid option. Please try again.");
            end case;
        end loop;

        -- Wait for user input before exiting
        Put_Line ("Press Enter to exit...");
        Get_Line (Input, Last);

    end Start_Simulation;

end simulation;
