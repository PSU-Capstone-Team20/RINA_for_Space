package body Flow_Manager is

   -- Initialize Flow Manager
   procedure Initialize_Flow_Manager(Mgr : in out Flow_Mgr_T) is
   begin
      Mgr.Active_Flows.Clear;
      Put_Line("Flow Manager initialized.");
   end Initialize_Flow_Manager;

   -- Allocate a New Flow
   procedure Allocate_New_Flow(Mgr : in out Flow_Mgr_T; Src : String; Dest : String) is
      New_Flow : Flow_T;
   begin
      New_Flow.ID := Flow_ID(Mgr.Active_Flows.Length + 1);
      New_Flow.Src := Src;
      New_Flow.Dest := Dest;
      New_Flow.State := Established;
      
      Mgr.Active_Flows.Append(New_Flow);
      Put_Line("Flow allocated from " & Src & " to " & Dest & " with ID: " & New_Flow.ID'Image);
   end Allocate_New_Flow;

   -- Release a Flow
   procedure Release_Flow(Mgr : in out Flow_Mgr_T; Flow_ID : Flow_ID) is
   begin
      for I in Mgr.Active_Flows.First_Index .. Mgr.Active_Flows.Last_Index loop
         if Mgr.Active_Flows(I).ID = Flow_ID then
            Mgr.Active_Flows.Delete(I);
            Put_Line("Flow ID " & Flow_ID'Image & " released.");
            return;
         end if;
      end loop;
      Put_Line("Error: Flow ID " & Flow_ID'Image & " not found.");
   end Release_Flow;

   -- List All Flows
   procedure List_All_Flows(Mgr : Flow_Mgr_T) is
   begin
      Put_Line("Listing all active flows:");
      for Flow of Mgr.Active_Flows loop
         Put_Line("Flow ID: " & Flow.ID'Image & " Src: " & Flow.Src & " Dest: " & Flow.Dest & " State: " & Flow_State'Image(Flow.State));
      end loop;
   end List_All_Flows;

end Flow_Manager;
