with Ada.Text_IO; use Ada.Text_IO;
with Ada.Real_Time; use Ada.Real_Time;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with application;
with Render_Buffer; use Render_Buffer;

package body simulation is

    procedure Test_Render_Buffer is
        RB : Render_Buffer.Render_Buffer;
    begin
        Clear_Buffer(RB);

        -- Test Draw_String
        Draw_String(RB, "Hello, World!", 1, 1);

        -- Test Draw_Border
        Draw_Border(RB, '+', 1, 2, 5, 10);
        Render_Buffer_To_Screen(RB);
    end Test_Render_Buffer;

    procedure Start_Simulation is
        App : application.Application;
        Input : String(1..100);
        Last : Natural;
        Data : Unbounded_String;
        RB : Render_Buffer.Render_Buffer;
    begin
        -- DEBUG 
        Test_Render_Buffer; 

        Clear_Buffer(RB);

        --  loop
        --      Clear_Buffer(RB);
        --      Update_Buffer(RB, 1, 1, 'C'); -- Example update
        --      Render_Buffer_To_Screen(RB);

        --      Put_Line("Choose an option:");
        --      Put_Line("1. Network Management");
        --      Put_Line("2. Data Management");
        --      Put_Line("3. Anomaly Handling");
        --      Put_Line("4. Exit");
        --      Get_Line(Input, Last);

        --      case Input(1) is
        --          when '1' =>
        --              Put_Line("Network Management");

        --          when '2' =>
        --              Put_Line("Data Management");

        --          when '3' =>
        --              Put_Line("Anomaly Handling");

        --          when '4' =>
        --              Put_Line("Exiting simulation...");
        --              exit;

        --          when others =>
        --              Put_Line("Invalid option. Please try again.");
        --      end case;
        --  end loop;

    end Start_Simulation;

end simulation;




-- Requirements:
                    -- The system must be able to create DIFs
                    -- The system must be able to modify DIFs
                    -- The system must be able to delete DIFs
                    -- The system must be able to disconnect from DIFs

                    -- The system must be able to create IPCPs
                    -- The system must be able to delete IPCPs

                    -- The system must be able to discover new DIFs
                    -- The system must be able to discover new IPCPs
                    -- The system must be able to connect to new DIFs
                    -- The system must be able to connect to new IPCPs

                    -- The system must be able to create a Resource Information Base (RIB)

                    -- The system will allow a user to create an application to connect to the network.
                    -- The system shall enable a newly created application to connect to the network.

                    -- The system shall be adaptable to future hardware.







                    -- The system must have acknowledgements.
                    -- The system must be able to buffer multiple pending acknowledgements within a DIF.

                    -- The system must transmit data between DIFs from origin to destination.
                    -- The system should be able to transmit data repeatedly
                    -- The system should be able to transmit data bidirectionally

                    -- The system must be able to temporarily store data in a DIF
                    -- The system must be able to transmit stored data



                    -- The system must retry failed communications.
                    -- The system must time out failed communications after 10 retries.

                    -- The system must be able to use provided data to communicate past obstacles.





