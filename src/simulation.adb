with Ada.Text_IO;           use Ada.Text_IO;
with Ada.Real_Time;         use Ada.Real_Time;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with application;
with Render_Buffer;         use Render_Buffer;

package body simulation is

    procedure Test_Render_Buffer is
        RB : Render_Buffer.Render_Buffer;
    begin
        Clear_Buffer (RB);
        Put_Line (Clear_Screen);

        Draw_Border (RB, '|', '=', 1, 1, 200, 30);
        Draw_RinaForSpace (RB, 62, 4);

        Render_Buffer_To_Screen (RB);

        Draw_Border (RB, '*', '*', 25, 25, 30, 29);

        Render_Buffer_To_Screen (RB);
    end Test_Render_Buffer;

    procedure Start_Simulation is

        App   : application.application;
        Input : String (1 .. 100);
        Last  : Natural;
        Data  : Unbounded_String;
        RB    : Render_Buffer.Render_Buffer;

    begin
        Clear_Buffer (RB);

        -- GIVE OPTIONS
        -- NASA DSN

        loop
            Draw_Border (RB, '|', '=', 1, 1, 100, 60);
            Draw_RinaForSpace (RB, 5, 2); -- 65 is more or less centered

            -- Network Management
            -- The system must be able to create a Resource Information Base (RIB)
            -- The system must be able to discover new DIFs
            -- The system must be able to discover new IPCPs
            -- The system must be able to connect to new IPCPs
            -- The system will allow a user to create an application to connect to the network.
            -- The system shall enable a newly created application to connect to the network.
            -- The system shall be adaptable to future hardware.
            Draw_String (RB, "Network Management", 5, 8);
            Draw_Line (RB, '=', 5, 9, 40, 9);

            -- The system must be able to create DIFs
            -- The system must be able to modify DIFs
            -- The system must be able to delete DIFs
            -- The system must be able to disconnect from DIFs
            -- The system must be able to connect to new DIFs
            Draw_String (RB, "DIF Management", 5, 10);
            Draw_String (RB, "1. Create DIF", 5, 11);
            Draw_String (RB, "2. Modify DIF", 5, 12);
            Draw_String (RB, "3. Delete DIF", 5, 13);
            Draw_String (RB, "4. Disconnect DIF", 5, 14);

            -- The system must be able to create IPCPs
            -- The system must be able to delete IPCPs
            Draw_String (RB, "IPCP Management", 26, 10);
            Draw_String (RB, "5. Create IPCP", 26, 11);
            Draw_String (RB, "6. Delete IPCP", 26, 12);







            -- DATA MANAGEMENT
            -- The system must have acknowledgements.
            -- The system must be able to buffer multiple pending acknowledgements within a DIF.
            -- The system must transmit data between DIFs from origin to destination.
            -- The system should be able to transmit data repeatedly
            -- The system should be able to transmit data bidirectionally
            -- The system must be able to temporarily store data in a DIF
            -- The system must be able to transmit stored data
            Draw_String (RB, "Data Management", 44,8);
            Draw_Line (RB, '=', 44, 9, 59, 9);

            Draw_String (RB, "7. Transmit Data", 44, 10);



            



            -- ANOMALY HANDLING
            -- The system must retry failed communications.
            -- The system must time out failed communications after 10 retries.
            -- The system must be able to use provided data to communicate past obstacles.
            Draw_String (RB, "Anomaly Handling", 63, 8);
            Draw_Line (RB, '=', 63, 9, 78, 9);







            Render_Buffer_To_Screen (RB);

            Get_Line(Input, Last);
            case Input (1) is
                when '1' =>
                    null;
                when '2' =>
                    null;
                when '3' =>
                    null;
                when '4' =>
                    null;
                when others =>
                    Put_Line ("Invalid option. Please try again.");
            end case;
        end loop;

        -- Wait for user input before exiting
        Put_Line ("Press Enter to exit...");
        Get_Line (Input, Last);

    end Start_Simulation;

end simulation;
