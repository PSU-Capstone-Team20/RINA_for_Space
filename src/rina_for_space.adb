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

   Flow		: RINA_Policies.Flow_ID := -1;
   Bundle   : RINA_Policies.DTN_Bundle_Protocol.Bundle_ID := 1000;
begin

   Put_Line("Initializing test");
   RINA_Policies.DTN_Bundle_Protocol.Create_Bundle(Flow, Bundle);
   RINA_Policies.DTN_Bundle_Protocol.Send_Bundle(Flow, Bundle);
   RINA_Policies.DTN_Bundle_Protocol.Receive_Bundle(Flow, Bundle);
   RINA_Policies.DTN_Bundle_Protocol.Handle_Custody(Bundle,RINA_Policies.DTN_Bundle_Protocol.Pending);
 
   Put_Line("RINA Policies test complete");
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
   Put_Line("5. Delete IPCP");
   Put_Line("6. Delete DIF");
   Put_Line("7. Build DSN Demo");
   Put_Line("0. Exit");
      
   loop
      Put("Choice: ");
      Get(choice);

      case choice is
         when '1' =>
            declare
               User_Input : Integer;
            begin
               Put("Enter the ID for the DIF: ");
               Ada.Integer_Text_IO.Get(User_Input);
               Dif.CreateDIF(User_Input, Test1);
               Put_Line("DIF Created with ID" & Integer'Image(User_Input));
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
               Put_Line(Integer'Image(Dif.getID(Test1(i))));
            end loop;      

         -- TODO: Add delete IPCP logic
         when '5' =>
            Put_Line("Successful Input - 5");    

         -- TODO: Add delete DIF logic
         when '6' =>
            Put_Line("Successful Input - 6");

         -- TODO: Create a DSN demo that generates all necessary DIFs and IPCPs
         when '7' =>
            Put_Line("Successful Input - 7");   

         when '0' =>
            Put_Line("Exiting");
            exit;    
            
         when others =>
            Put_Line("Invalid choice. Please try again.");      
      end case;   

   end loop;      
end Rina_For_Space;

