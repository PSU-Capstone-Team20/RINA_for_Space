with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with RINA;
with RINA_Policies;
with Flow_Management; use Flow_Management;
with Resource_Management; use Resource_Management;

package IPCP is
   
   type IPCP_T is record
      ID : Integer;
      Name : String(1 .. 64);
      DIF_ID : Integer;
      Address : RINA.Address_T;
      QoS_Params : RINA_Policies.QoS_Parameter;
      Flows : Flow_Management.Flow_List;
      Resources : Resource_Management.Resource_List;
   end record;

   procedure Initialize_IPCP(
      IPCP_Instance : in out IPCP_T; 
      ID: Integer; 
      Name : String;
      DIF_ID : Integer;
      Address : RINA.Address_T;
      QoS : RINA.Policies.QoS_Parameter);

   procedure Display_IPCP_Info(IPCP_Instance : in IPCP_T);

   -- Flow Management
   procedure Add_Flow(IPCP_Instance : in out IPCP_T; New_Flow : Flow_Management.Flow_T);
   procedure Display_Flows(IPCP_Instance : in IPCP_T);

   -- Resource Management
   procedure Allocate_Resource(IPCP_Instance : in out IPCP_T; New_Resource : Resource_Management.Resource_T);
   procedure Display_Resources(IPCP_Instance : in IPCP_T);

end IPCP;