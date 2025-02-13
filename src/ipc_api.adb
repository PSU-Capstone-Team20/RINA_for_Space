package body IPC_API is

   -- Flow Management Structure
   Active_Flows : Flow_Vector.Vector;

   -- Allocate a flow between Source and Destination with QoS
   function Request_Flow(Source : String; Destination : String; QoS : QoS_Level) return Port_ID is
      New_Flow : Flow_Record;
      New_Port : Port_ID := Port_ID(Active_Flows.Length + 1);
   begin
      New_Flow.Port := New_Port;
      New_Flow.Source := Source;
      New_Flow.Destination := Destination;
      New_Flow.QoS := QoS;
      New_Flow.Active := True;
      
      Active_Flows.Append(New_Flow);
      Allocate_New_Flow(Source, Destination); -- Calls Flow Manager
      
      Put_Line("Flow allocated: Port " & New_Port'Image & " Source=" & Source & " Destination=" & Destination);
      return New_Port;
   end Request_Flow;

   -- Send SDU from a flow's allocated port
   procedure Transmit_Data_To_Dst(Port : Port_ID; Data : String) is
      Flow_Exists : Boolean := False;
   begin
      -- Check if flow exists
      for Flow of Active_Flows loop
         if Flow.Port = Port and Flow.Active then
            Flow_Exists := True;
            Put_Line("Sending SDU from Port " & Port'Image & " to " & Flow.Destination);
            Put_Line("SDU Content: " & Data);
            return;
         end if;
      end loop;
      
      if not Flow_Exists then
         Put_Line("Error: Invalid Port " & Port'Image & " or Flow is inactive.");
      end if;
   end Transmit_Data_To_Dst;

   -- Receive an SDU from a given port
   function Receive_SDU(Port : Port_ID) return String is
      Received_Data : String(1 .. 50) := (others => ' '); -- Placeholder for SDU
      Flow_Exists   : Boolean := False;
   begin
      -- Check if flow exists
      for Flow of Active_Flows loop
         if Flow.Port = Port and Flow.Active then
            Flow_Exists := True;
            Received_Data := "Received Data for Port " & Port'Image;
            Put_Line("Data Received on Port " & Port'Image & ": " & Received_Data);
            return Received_Data;
         end if;
      end loop;

      if not Flow_Exists then
         Put_Line("Error: No active flow found for Port " & Port'Image);
      end if;
      
      return "";
   end Receive_SDU;

   -- Deallocate a flow and free resources
   procedure Deallocate_Flow(Port : Port_ID) is
   begin
      for I in Active_Flows.First_Index .. Active_Flows.Last_Index loop
         if Active_Flows(I).Port = Port then
            Release_Flow(Active_Flows(I).Port); -- Calls Flow Manager
            Active_Flows.Delete(I);
            Put_Line("Deallocated flow on Port " & Port'Image);
            return;
         end if;
      end loop;
      Put_Line("Error: Port " & Port'Image & " not found.");
   end Deallocate_Flow;

end IPC_API;
