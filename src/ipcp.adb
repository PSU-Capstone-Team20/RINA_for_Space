with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package body IPCP is
   
   procedure Initialize_IPCP(
      IPCP_Instance : in out IPCP_T; 
      ID: Integer; 
      Name : String;
      DIF_ID : Integer;
      Address : RINA.Address_T;
      QoS : RINA.Policies.QoS_Parameter) is
   begin
      IPCP_Instance.ID := ID;
      IPCP_Instance.Name := Name;
      IPCP_Instance.DIF_ID := DIF_ID;
      IPCP_Instance.Address := Address;
      IPCP_Instance.QoS_Params := QoS;
   end Initialize_IPCP;

   procedure Display_IPCP_Info(IPCP_Instance : in IPCP_T) is
   begin
      Put_Line("IPCP Information:");
      Put_Line("IPCP ID: " & Integer'Image(IPCP_Instance.ID));
      Put_Line("Name: " & Integer'Image(IPCP_Instance.Name));
      Put_Line("DIF ID: " & Integer'Image(IPCP_Instance.DIF_ID));
      Put_Line("Address: " & Integer'Image(IPCP_Instance.DIF_ID) & ", App_Process_Name => " & To_String(IPCP_Instance.Address.App_Process_Name));
      Put_Line("QoS Parameters:  Priority => " & Integer'Image(IPCP_Instance.QoS_Params.Priority) & ", Latency => " & Integer(IPCP_Instance.QoS_Params.Latency) & ", Throughput => " & Integer'Image(IPCP_Instance.QoS_Params.Throughout));
   end Display IPCP_Info;

   procedure Add_Flow(IPCP_Instance : in out IPCP_T; New_Flow : Flow_Management.Flow_T) is
   begin
      IPCP_Instance.Flows.Append(New_Flow);
   end Add_Flow;

   procedure Display_Flows(IPCP_Instance : in IPCP_T) is
   begin
      for Flow of IPCP_Instance.Flows loop
         Flow_Management.Display_Flow(Flow);
      end loop;
   end Display_Flows;

   procedure Allocate_Resource(IPCP_Instance : in out IPCP_T; New_Resource : Resource_Management.Resource_T) is
   begin
      IPCP_Instance.Resources.Append(New_Resource);
   end Allocate_Resource;

   procedure Display_Resources(IPCP_Instance : in IPCP_T) is
   begin
      for Resource of IPCP_Instance.Resources loop
         Resource_Management.Display_Resource(Resource);
      end loop;
   end Display_Resources;
   
end IPCP;