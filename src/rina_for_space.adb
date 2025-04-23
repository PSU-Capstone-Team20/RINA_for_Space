with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Integer_Text_IO;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Streams.Stream_IO;
with Ada.Calendar; use Ada.Calendar;
with Ada.Containers.Vectors;
with IPC_Manager; use IPC_Manager;
with DIF_Manager; use DIF_Manager;
with IPC_Manager.IPCP; use IPC_Manager.IPCP;
with DIF_Manager.Dif; use DIF_Manager.Dif;
with Rina_BP_Bundle; use Rina_BP_Bundle;
with IPCP_Types; use IPCP_Types;
with CDAP;
with RIB; use RIB;
with Policy_Enforcement; use Policy_Enforcement;
with mockcomp;
with RINA; use RINA;
with EFCP; use EFCP;
with IPC_Data_Transfer; use IPC_Data_Transfer;
--with fakeComp;
with EFCP; use EFCP;


procedure Rina_For_Space is

  -- IPC_M : IPCP_Manager_T;

   --IPC_M_Joe : IPCP_Manager_T;
   --  task Joe_Comp;
   --  task body Joe_Comp is
   --     begin
   --     null;
   --  end Joe_Comp;



   IPC_M_Steve : IPCP_Manager_T;
   --  task Steve_Comp;
   --  task body Steve_Comp is
   --     begin
   --     null;
   --  end Steve_Comp;
   
   IPC_M_Chad : IPCP_Manager_T;
   --  task Chad_Comp;
   --  task body Chad_Comp is
   --     begin
   --     while true loop
   --        Put_Line ("Operating IPC Manager");
   --     end loop;
   --  end Chad_Comp;

   --  package DIF_Vectors is new Ada.Containers.Vectors
   --     (Index_Type => Natural, Element_Type => DIF_Access);
   
   --  type Task_Comp_Access is access all fakeComp.fake_comp;

   --  package Task_Comp_Vectors is new Ada.Containers.Vectors
   --     (Index_Type => Natural, Element_Type => Task_Comp_Access);

   --  TC_V : Task_Comp_Vectors.Vector;

   --  killflag : Integer := 0;

   -- ****NEW****
   --  Task running is
   --     entry start;
   --  end running;
   --  task body running is
   --  begin
   --     accept start;
   --     while killflag = 0 loop
   --     for i in TC_V.First_Index .. TC_V.Last_Index loop
   --        TC_V.Reference(i).operate;
   --     end loop;
   --     accept start;
   --     end loop;
   --  end running;

   --  procedure newfakecomp is
   --     New_Task : constant Task_Comp_Access := new fakeComp.fake_comp;
   --  begin
   --     TC_V.Append (New_Task);
   --  end;
   -- ****NEW****

   --testing for bundle 
  

   --DIF and IPCP instance test 
   --  DIF_Instance : DIF_Manager.Dif.DIF_T;
   --  IPCP_Instance : IPCP_Types.IPCP_T;
   --  IPCP_Instance1 : IPCP_Types.IPCP_T;

   --  Policy : Policy_Enforcement.DIF_Creation_Policy;


   
   
   test_Path_Output : RINA.Path_Vectors.Vector;
   temp : Unbounded_String;
   test_Start_Address : RINA.Address_Vectors.Vector;
   test_Target_Address : RINA.Address_Vectors.Vector;
   test_Blank_Element : RINA.Address_Element;
   test_task : mockcomp.mock_comp;

begin
   
   Add_Entry (To_Unbounded_String("John DIF"));
   temp := To_Unbounded_String("Doe Comp");
   Add_Comp (To_Unbounded_String("John DIF"), temp);
   temp := To_Unbounded_String("Mini Fridge IPCP");
   Add_IPCP (To_Unbounded_String("John DIF"), To_Unbounded_String("Doe Comp"), temp);
   temp := To_Unbounded_String("Mini Fridge APN");
   Add_APN (To_Unbounded_String("John DIF"), To_Unbounded_String("Doe Comp"), temp);

   Add_Entry (To_Unbounded_String("Jane DIF"));
   temp := To_Unbounded_String("Doe Comp");
   Add_Comp (To_Unbounded_String("Jane DIF"), temp);
   temp := To_Unbounded_String("Laptop IPCP");
   Add_IPCP (To_Unbounded_String("Jane DIF"), To_Unbounded_String("Doe Comp"), temp);
   temp := To_Unbounded_String("Laptop APN");
   Add_APN (To_Unbounded_String("Jane DIF"), To_Unbounded_String("Doe Comp"), temp);

   Add_Entry (To_Unbounded_String("John DIF"));
   temp := To_Unbounded_String("Smith Comp");
   Add_Comp (To_Unbounded_String("John DIF"), temp);
   temp := To_Unbounded_String("Laptop IPCP");
   Add_IPCP (To_Unbounded_String("John DIF"), To_Unbounded_String("Smith Comp"), temp);
   temp := To_Unbounded_String("Laptop APN");
   Add_APN (To_Unbounded_String("John DIF"), To_Unbounded_String("Smith Comp"), temp);
   
   Add_Entry (To_Unbounded_String("Jane DIF"));
   temp := To_Unbounded_String("Lee Comp");
   Add_Comp (To_Unbounded_String("Jane DIF"), temp);
   temp := To_Unbounded_String("Laptop IPCP");
   Add_IPCP (To_Unbounded_String("Jane DIF"), To_Unbounded_String("Lee Comp"), temp);
   temp := To_Unbounded_String("Laptop APN");
   Add_APN (To_Unbounded_String("Jane DIF"), To_Unbounded_String("Lee Comp"), temp);

   --  Add_Entry (To_Unbounded_String("Jill DIF"));
   --  temp := To_Unbounded_String("Lee Comp");
   --  Add_Comp (To_Unbounded_String("Jill DIF"), temp);
   --  temp := To_Unbounded_String("Laptop IPCP");
   --  Add_IPCP (To_Unbounded_String("Jill DIF"), To_Unbounded_String("Lee Comp"), temp);
   --  temp := To_Unbounded_String("Laptop APN");
   --  Add_APN (To_Unbounded_String("Jill DIF"), To_Unbounded_String("Lee Comp"), temp);

   --  Add_Entry (To_Unbounded_String("Jill DIF"));
   --  temp := To_Unbounded_String("Smith Comp");
   --  Add_Comp (To_Unbounded_String("Jill DIF"), temp);
   --  temp := To_Unbounded_String("Laptop IPCP");
   --  Add_IPCP (To_Unbounded_String("Jill DIF"), To_Unbounded_String("Smith Comp"), temp);
   --  temp := To_Unbounded_String("Laptop APN");
   --  Add_APN (To_Unbounded_String("Jill DIF"), To_Unbounded_String("Smith Comp"), temp);

   Display_Map;

   Put_Line("Attempting to Path from first entry to second Entry");

   test_Blank_Element.Name := To_Unbounded_String("John DIF");
   test_Blank_Element.Address_Type := To_Unbounded_String("DIF");
   test_Start_Address.Append(test_Blank_Element);
   test_Blank_Element.name := To_Unbounded_String("Smith Comp");
   test_Blank_Element.Address_Type := To_Unbounded_String("Computer");
   test_Start_Address.Append(test_Blank_Element);
   test_Blank_Element.name := To_Unbounded_String("Laptop APN");
   test_Blank_Element.Address_Type := To_Unbounded_String("APN");
   test_Start_Address.Append(test_Blank_Element);
   
   test_Blank_Element.Name := To_Unbounded_String("John DIF");
   test_Blank_Element.Address_Type := To_Unbounded_String("DIF");
   test_Target_Address.Append(test_Blank_Element);
   test_Blank_Element.name := To_Unbounded_String("Lee Comp");
   test_Blank_Element.Address_Type := To_Unbounded_String("Computer");
   test_Target_Address.Append(test_Blank_Element);
   test_Blank_Element.name := To_Unbounded_String("Laptop APN");
   test_Blank_Element.Address_Type := To_Unbounded_String("APN");
   test_Target_Address.Append(test_Blank_Element);

   test_Path_Output := D_Star_Lite(test_Start_Address, test_Target_Address);

   --TODO: FIGURE OUT HOW TO ACCESS 2D VECTOR
   --  for C in test_Path_Output loop
   --     for X in test_Path_Output.Reference (C) loop
   --        temp := temp & test_Path_Output.Reference.Reference(C)(X);
         
   --     end loop;
   --     Put_Line (temp);
   --     temp := To_Unbounded_String("");
   --  end loop;

   for C in test_Path_Output.First_Index .. test_Path_Output.Last_Index loop
      declare
         Pathway : Unbounded_String := To_Unbounded_String(" ");
         Inner_Vector: constant RINA.Address_Vectors.Vector := test_Path_Output.Element(C);
      begin
         for X in Inner_Vector.First_Index .. Inner_Vector.Last_Index loop
            Pathway := Pathway & Inner_Vector.Element(X).Name;
            if X < Inner_Vector.Last_Index then
               Pathway := Pathway & To_Unbounded_String(" -- ");
            end if;
         end loop;
         Put_Line(To_String(Pathway));
      end;
   end loop;

   --Test bundle send 
   declare
      Src_EID : EFCP.PDU_S_T;
      Dst_EID : EFCP.PDU_S_T;
      Payload :  String   := "Message: It's cold here, Temp: 30 Degrees, Longitude, Latitude: 77.246074, -18.47081709";
      B       : Bundle;
   begin
      Src_EID.PCI.Src_CEP_ID := To_Unbounded_String("Laptop APN");
      Dst_EID.PCI.Dst_CEP_ID := To_Unbounded_String("Mini Fridge APN");

      B := Create_Bundle(Version => 7, Processing_Flag => 0, 
                         Block_Length => Payload'Length, 
                         Src_EID => Src_EID, 
                         Dst_EID => Dst_EID, 
                         Payload => Payload(1 .. Payload'Length), 
                         Path => test_Path_Output);
      Put_Line("Bundle creation initiated....created....Send");
      Send_Bundle(B);
      
   end;
   
   --  Policy := Policy_Enforcement.Get_DIF_Creation_Policy(DIF_Instance.DIF_Name);
   --  DIF_Instance.Policy := Policy;
   --  DIF_Instance.DIF_ID := 1;
   --  DIF_Instance.DIF_Name := To_Unbounded_String("DIF Test");
   --  IPCP_Instance.Name := To_Unbounded_String("IPCPTest");
   --  IPCP_Instance1.Name := To_Unbounded_String("IPCPTest1");

   --  DIF_Manager.Enroll_IPCP(DIF_Instance, IPCP_Instance);
   --  DIF_Manager.Enroll_IPCP(DIF_Instance, IPCP_Instance1);

   --  Put_Line("DIF Name : " & To_String(DIF_Instance.DIF_Name));

   --  Put_Line("IPCP Name: " & To_String(IPCP_Instance.Name));
   --  Put_Line("Second IPCP Name: " & To_String(IPCP_Instance1.Name));
   


   --  Put_Line("Number of Enrolled IPCPs: " & Ada.Containers.Count_Type'Image(DIF_Instance.Enrolled_IPCPs.Length));
   
   --  for Index in DIF_Instance.Enrolled_IPCPs.First_Index .. DIF_Instance.Enrolled_IPCPs.Last_Index loop
   --     Put_Line( " - " & To_String(DIF_Instance.Enrolled_IPCPs(Index).Name));
   --  end loop;

   --  Put_Line("Policy: Routing = " & To_String(Policy.Routing_Strategy));
   --  Put_Line("Policy: Enrollment = " & To_String(Policy.Enrollment_Type));


   --  newfakecomp;
   --  TC_V.Reference(0).change_name(To_Unbounded_String("Joe"));

   --  dif_manager.Create_Named_DIF(0, To_Unbounded_String("DIF 1"));

   --  TC_V.Reference(0).add_IPCP(To_Unbounded_String("IPCP 1"));
   --  TC_V.Reference(0).connect_DIF(dif_manager.DIFs(0).DIF_Name);

   --  running.start;
   --  delay 1.0;

   --  RIB.Display_Map;

   --  TC_V.Reference(0).change_name(To_Unbounded_String("Steve"));
   --  running.start;

   --  delay 1.0;

   --  RIB.Display_Map;
   

   --create the bundle 
   --  B := Rina_BP_Bundle.Create_Bundle(Version => 6, 
   --                                    Processing_Flag => 2, 
   --                                    Block_Length => 512, 
   --                                    Src_EID => "Mars Observer", 
   --                                    Dst_EID => "Ground Station 1", 
   --                                    Payload => "Water found in region 5");


   --  --serialize bundle
   --  Send_Bundle(B);

   --  --receive the bundle
   --  --B := Receive_Bundle;

   --  --details of bundle printout 
   --  Put_Line("Received Data from Bundle: ");
   --  Put_Line("Version: " & Natural'Image(B.Header.Version));
   --  Put_Line("Processing Flag: " & Natural'Image(B.Header.Processing_Flag));
   --  Put_Line("Block Length: " & Natural'Image(B.Header.Block_Length));
   --  Put_Line("Source: " & B.Src_EID);
   --  Put_Line("Destination: " & B.Dst_EID);
   --  Put_Line("Payload: " & B.Payload);

   --  Put_Line("Bundle has been processed successfully");
   
   --test := RINA_Policies.Encode_SDNV(1420);
   --Put_Line (test'Image);
   --Put_Line(RINA_Policies.Decode_SDNV(test)'Image);
   --  killflag := 1;
   --  running.start;
   --  delay 1.0;
   NULL;
end Rina_For_Space;

