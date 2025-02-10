with Ada.Text_IO; use Ada.Text_IO;

package body IPCP.Flow_Management is

   -- Create a new flow
   procedure Create_Flow(Flows : in out Flow_List; Source : String; Dest : String; QoS : String) is
      New_ID : Flow_ID := Flow_ID(Flows.Length + 1);
      New_Flow : Flow_Record := (ID => New_ID, Source => Source, Dest => Dest, QoS => QoS, State => Active);
   begin
      Flows.Append(New_Flow);
      Put_Line("Flow created from " & Source & " to " & Dest & " with QoS: " & QoS & ". Flow ID: " & New_ID'Image);
   end Create_Flow;

   -- Close a flow by ID
   procedure Close_Flow(Flows : in out Flow_List; ID : Flow_ID) is
   begin
      for I in Flows.First_Index .. Flows.Last_Index loop
         if Flows(I).ID = ID then
            Flows(I).State := Closed;
            Put_Line("Flow with ID " & ID'Image & " has been closed.");
            exit;
         end if;
      end loop;
   end Close_Flow;

   -- Get the status of a specific flow
   function Get_Flow_Status(Flows : Flow_List; ID : Flow_ID) return Flow_State is
   begin
      for Flow of Flows loop
         if Flow.ID = ID then
            return Flow.State;
         end if;
      end loop;
      return Closed;  
   end Get_Flow_Status;

   -- Display all flows
   procedure Display_Flows(Flows : Flow_List) is
   begin
      Put_Line("Listing all flows:");
      for Flow of Flows loop
         Put_Line("ID: " & Flow.ID'Image & ", Source: " & Flow.Source & ", Dest: " & Flow.Dest & ", QoS: " & Flow.QoS & ", State: " & Flow_State'Image(Flow.State));
      end loop;
   end Display_Flows;

end IPCP.Flow_Management;
