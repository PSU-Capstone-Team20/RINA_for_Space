with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with RINA;
with RINA_Policies;

package IPCP is
   
   type IPCP_T is record
      ID : Integer;
      Name : String(1 .. 64);
      DIF_ID : Integer;
      Address : RINA.Address_T;
      QoS_Params : RINA_Policies.QoS_Parameter;
   end record;

   procedure Initialize_IPCP(
      IPCP_Instance : in out IPCP_T; 
      ID: Integer; 
      Name : String;
      DIF_ID : Integer;
      Address : RINA.Address_T;
      QoS : RINA.Policies.QoS_Parameter);

   procedure Display_IPCP_Info(IPCP_Instance : in IPCP_T);

end IPCP;