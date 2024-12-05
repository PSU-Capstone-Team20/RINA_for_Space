with Rina;
with dif;
with ipcp;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Integer_Text_IO;
with Ada.Text_IO; use Ada.Text_IO;
with RINA_Policies; use RINA_Policies;
with RINA_Policies.DTN_Bundle_Protocol; 

procedure Rina_For_Space is
   test1 : dif.DIF_Vector;
   num : Integer;
   choice : Character;
   -- Flow		: RINA_Policies.Flow_ID := 1;
   -- QoS		: RINA_Policies.QoS_Parameter := (
   -- Priority => 1,
   -- Latency => 100,
   -- Throughput => 500,
   -- QoS_ID => 42);


begin

   -- Put_Line("Initializing test");
   -- RINA_Policies.DTN_Bundle_Protocol.Create_Bundle(Flow, Bundle);
   -- RINA_Policies.DTN_Bundle_Protocol.Send_Bundle(Flow, Bundle);
   -- RINA_Policies.DTN_Bundle_Protocol.Receive_Bundle(Flow, Bundle);
   -- RINA_Policies.DTN_Bundle_Protocol.Handle_Custody(Bundle,RINA_Policies.DTN_Bundle_Protocol.Pending);
 
   -- Put_Line("RINA Policies test complete");
   -- Put_Line ("Initializing test");
   -- RINA_Policies.Define_QoS(Flow, QoS);
   -- RINA_Policies.Schedule_Flow(Flow);
   -- RINA_Policies.Relay_And_Forward (Source_Flow => Flow, Destination_Flow => 2);
   -- RINA_Policies.Handle_Error (Flow, Error_Code => 404);
   -- Put_Line ("RINA Policies test");
   
   -- Original Test Code 
   -- TODO: Delete if no longer needed
   -- dif.createDIF(1, test1);
   -- num := dif.getID(test1(0));
   -- dif.createIPCP(To_Unbounded_String("Test"), test1(0));
   -- dif.listIPCP(test1(0));
   -- dif.deleteIPCP(To_Unbounded_String("Test"), test1(0));
   -- if test1(0).MemberIPCPs.Is_Empty then
   --   Put_Line("Successful deletion of IPCP");
   -- end if;
   
   -- Below is temporary(?) code to give a console interface for presentation demo
   -- TODO: Either remove or clean up after demo

   Put_Line("Select an option:");
   Put_Line("1. Create DIF");
   Put_Line("2. Create IPCP");
   Put_Line("3. List IPCP");
   Put_Line("4. List DIF");
   Put_Line("5. Disconnect IPCP");
   Put_Line("6. Connect DIF");
   Put_Line("7. Build DSN Demo");
   Put_Line("8. Bundle Demo");
   Put_Line("9. Print Network Structure");
   Put_Line("0. Exit");
      
   loop
      Put("Choice: ");
      Get(choice);

      case choice is
         when '1' =>
            declare
               User_Input_ID : Integer;
               User_Input_Name : String (1 .. 100);
               Last : Natural;
            begin
               Put("Enter the ID for the DIF: ");
               Ada.Integer_Text_IO.Get(User_Input_ID);
               Skip_Line;
               Put_Line("Enter the name for the DIF: ");
               Get_Line(User_Input_Name, Last);
               Dif.CreateNamedDIF(User_Input_ID,Test1,To_Unbounded_String(User_Input_Name(1 .. Last)));
               Put("DIF Created with ID" & Integer'Image(User_Input_ID));
               Put_Line(" and with name " & String'Image(User_Input_Name(1 .. Last)));
            end;   

         when '2' =>
            declare
               DIF_ID   : Integer;
               Name     : String (1 .. 100);
               Last     : Natural;
               Found    : Boolean := False;
               Index    : Natural;
            begin
               Put("Enter the DIF ID to search for: ");
               Ada.Integer_Text_IO.Get(DIF_ID);
               Skip_Line;

               for I in Test1.First_Index .. Test1.Last_Index loop
                  if Dif.GetID(Test1(I)) = DIF_ID then
                     Found := True;
                     Index := I;
                     exit;
                  end if;
               end loop;

               if Found then
                  Put_Line("Enter the name for the IPCP: ");
                  Get_Line(Name, Last);

                  Dif.CreateIPCP(To_Unbounded_String(Name(1 .. Last)), Test1(Index));
                  Put_Line("IPCP created successfully in DIF with ID: " & Integer'Image(DIF_ID));
               else
                  Put_Line("No matching DIF found with ID: " & Integer'Image(DIF_ID));
               end if;
            end;   

         when '3' =>
            declare
               DIF_ID   : Integer;
               Name     : String (1 .. 100);
               Last     : Natural;
               Found    : Boolean := False;
               Index    : Natural;
            begin
               Put("Enter the DIF ID to list IPCPs for: ");
               Ada.Integer_Text_IO.Get(DIF_ID);
               Skip_Line;

               for I in Test1.First_Index .. Test1.Last_Index loop
                  if Dif.GetID(Test1(I)) = DIF_ID then
                     Found := True;
                     Index := I;
                     exit;
                  end if;
               end loop;

               if Found then
                  Dif.listIPCP(test1(Index));
               else
                  Put_Line("No matching DIF found with ID: " & Integer'Image(DIF_ID));
               end if;
            end;  

         when '4' =>
            for I in Test1.First_Index .. Test1.Last_Index loop
               Put(Integer'Image(Dif.getID(Test1(i))));
               Put_Line(" " & Unbounded_String'Image(Dif.getName(test1(i))));
            end loop;      

         -- TODO: Add Disconnect IPCP logic
         when '5' =>
            declare
               DIF_ID   : Integer;
               Name     : String (1 .. 100);
               Last     : Natural;
               Found    : Boolean := False;
               Index    : Natural;
            begin
               Put("Enter the DIF ID to disconnect IPCP from: ");
               Ada.Integer_Text_IO.Get(DIF_ID);
               Skip_Line;

               for I in Test1.First_Index .. Test1.Last_Index loop
                  if Dif.GetID(Test1(I)) = DIF_ID then
                     Found := True;
                     Index := I;
                     exit;
                  end if;
               end loop;

               if Found then
                  Dif.listIPCP(test1(Index));

                  Put_Line("Enter the name for the IPCP to disconnect: ");
                  Get_Line(Name, Last);

                  Dif.disconnectIPCP(To_Unbounded_String(name(1 .. Last)), test1(Index));

                  Put_Line("IPCP has been disconnected");

               else
                  Put_Line("No matching DIF found with ID: " & Integer'Image(DIF_ID));
               end if;
            end;  


         --  Connect DIF Logic
         when '6' =>
            declare
               Found    : Boolean := False;
               Input_DIF1 : Integer;
               Input_DIF2 : Integer;
               Index    : Natural;
               Index2   : Natural;
            begin
               Put("Enter the first DIF ID to connect: ");
               Ada.Integer_Text_IO.Get(Input_DIF1);
               Skip_Line;

               Put("Enter the second DIF ID to connect: ");
               Ada.Integer_Text_IO.Get(Input_DIF2);
               Skip_Line;

               for I in Test1.First_Index .. Test1.Last_Index loop
                  if Dif.GetID(Test1(I)) = Input_DIF1 then
                     Found := True;
                     Index := I;
                     exit;
                  end if;
               end loop;

               for I in Test1.First_Index .. Test1.Last_Index loop
                  if Dif.GetID(Test1(I)) = Input_DIF2 then
                     Found := True;
                     Index2 := I;
                     exit;
                  end if;
               end loop;

               Dif.pairDIF(test1(Index), test1(Index2));
            end;  

         when '7' =>
            declare
               DIF_ID   : Integer;
               Name     : String (1 .. 100);
               Last     : Natural;
               Found    : Boolean := False;
               Index    : Natural;
               Index2   : Natural;
            begin
               Put_Line("Generating NASA Deep Space Network Sample");
               Put_Line("");
               -- Generate All DIFS
               Dif.CreateNamedDIF(1,Test1,To_Unbounded_String("ISP DIF"));
               Dif.CreateNamedDIF(2,Test1,To_Unbounded_String("Earth Network DIF"));
               Dif.CreateNamedDIF(3,Test1,To_Unbounded_String("Deep Space Relay DIF"));

               -- Generate All IPCPs
               for I in Test1.First_Index .. Test1.Last_Index loop
                  if Dif.GetID(Test1(I)) = 1 then
                     Found := True;
                     Index := I;
                     exit;
                  end if;
               end loop;

               Dif.CreateIPCP(To_Unbounded_String("Local Desktop"), test1(Index));
               Dif.createIPCP(To_Unbounded_String("NASA Server 1"), test1(Index));

               for I in Test1.First_Index .. Test1.Last_Index loop
                  if Dif.GetID(Test1(I)) = 2 then
                     Found := True;
                     Index := I;
                     exit;
                  end if;
               end loop;

               Dif.createIPCP(To_Unbounded_String("Madrid Ground Station"), test1(Index));
               Dif.createIPCP(To_Unbounded_String("Goldstone Ground Station"), test1(Index));
               Dif.createIPCP(To_Unbounded_String("Canberra Ground Station"), test1(Index));

               for I in Test1.First_Index .. Test1.Last_Index loop
                  if Dif.GetID(Test1(I)) = 3 then
                     Found := True;
                     Index := I;
                     exit;
                  end if;
               end loop;

               Dif.createIPCP(To_Unbounded_String("Mars Odyssey"), test1(Index));
               Dif.createIPCP(To_Unbounded_String("Mars Reconnaissance Orbiter"), test1(Index));
               Dif.createIPCP(To_Unbounded_String("Maven"), test1(Index));
               
               -- Link All DIFs
               -- Link ISP to Earth Network
               for I in Test1.First_Index .. Test1.Last_Index loop
                  if Dif.GetID(Test1(I)) = 1 then
                     Found := True;
                     Index := I;
                     exit;
                  end if;
               end loop;

               for I in Test1.First_Index .. Test1.Last_Index loop
                  if Dif.GetID(Test1(I)) = 2 then
                     Found := True;
                     Index2 := I;
                     exit;
                  end if;
               end loop;

               Dif.pairDIF(test1(Index), test1(Index2));

               -- Link Earth Network to Mars Relay
               for I in Test1.First_Index .. Test1.Last_Index loop
                  if Dif.GetID(Test1(I)) = 2 then
                     Found := True;
                     Index := I;
                     exit;
                  end if;
               end loop;

               for I in Test1.First_Index .. Test1.Last_Index loop
                  if Dif.GetID(Test1(I)) = 3 then
                     Found := True;
                     Index2 := I;
                     exit;
                  end if;
               end loop;

               Dif.pairDIF(test1(Index), test1(Index2));

               -- Print Everything
               Put_Line("Current Network Structure");

               for I in Test1.First_Index .. Test1.Last_Index loop
                  -- Print DIF
                  Put(Unbounded_String'Image(Dif.getName(test1(i))));
                  Put_Line(" ID:" & Integer'Image(Dif.getID(Test1(i))));

                  -- Print IPCPs
                  Put_Line("----- IPCPS -----");
                  Dif.listIPCP(test1(I));
                
                  -- Print Connections
                  Put_Line("----- Connections -----");
                  Dif.listAccessibleDIF(test1(i));
                  Put_Line("");
                  Put_Line("");

               end loop;   
            end;

         when '8' =>
            declare
               Flow		: RINA_Policies.Flow_ID := 1;
               Bundle   : RINA_Policies.DTN_Bundle_Protocol.Bundle_ID := 1000;  
               QoS		: RINA_Policies.QoS_Parameter := (
                           Priority => 1,
                           Latency => 100,
                           Throughput => 500,
                           QoS_ID => 42);
            begin
               Put_Line("Successful Input - 8");  
            -- Put_Line ("Initializing test");
               RINA_Policies.Define_QoS(Flow, QoS);
               RINA_Policies.Schedule_Flow(Flow);
               RINA_Policies.Relay_And_Forward (Source_Flow => Flow, Destination_Flow => 2);
               RINA_Policies.DTN_Bundle_Protocol.Create_Bundle(Flow, Bundle);
               RINA_Policies.DTN_Bundle_Protocol.Send_Bundle(Flow, Bundle);
               RINA_Policies.DTN_Bundle_Protocol.Receive_Bundle(Flow, Bundle);
               RINA_Policies.DTN_Bundle_Protocol.Handle_Custody(Bundle,RINA_Policies.DTN_Bundle_Protocol.Pending);
            end;


         when '9' =>
               for I in Test1.First_Index .. Test1.Last_Index loop
                  -- Print DIF
                  Put(Unbounded_String'Image(Dif.getName(test1(i))));
                  Put_Line(" ID:" & Integer'Image(Dif.getID(Test1(i))));


                  -- Print IPCPs
                  Put_Line("----- IPCPS -----");
                  Dif.listIPCP(test1(I));
                
                  -- Print Connections
                  Put_Line("----- Connections -----");
                  Dif.listAccessibleDIF(test1(i));
                  Put_Line("");
                  Put_Line("");

               end loop;           

         when '0' =>
            Put_Line("Exiting");
            exit;    
            
         when others =>
            Put_Line("Invalid choice. Please try again.");      
      end case;   

   end loop;      
end Rina_For_Space;

