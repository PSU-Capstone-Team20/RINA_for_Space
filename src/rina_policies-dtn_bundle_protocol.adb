with Ada.Text_IO; use Ada.Text_IO;
with RINA_Policies;

package body RINA_Policies.DTN_Bundle_Protocol is 

   --test: create new buyndle with SDNV encoding 
   procedure Create_Bundle(Flow : in RINA_Policies.Flow_ID; Bundle : out Bundle) is
   begin
      --primary block initialize 
      Bundle.Primary.Version := 1; --BPv7 
      Bundle.Primary.Processing_Flag := RINA_Policies.Encode_SDNV(0); --flag ex.
      Bundle.Primary.Creation_Timestamp := RINA_Policies.Encode_SDNV(Integer(Calendar.Clock));
      Bundle.Lifetime := RINA_Policies.Encode_SDNV(600);
      Bundle.Source_EID := "Source_Application";
      Bundle.Destination_EID := "Destination_Application";

      --payload block initialize 
      Bundle.Payload.Data_Length := RINA_Policies.Encode_SDNV(512); -- arbitrary payload length
      Bundle.Payload.Data := (others => 'X'); -- payload data for test purpose

      Put_Line("Bundle Created for FLow: " & Integer'Image(Integer(Flow)));
   end Create_Bundle;

   --decode SDNV encoding 
   procedure Process_Bundle(Flow : in RINA_Policies.Flow_ID; Bundle : in out Bundle) is 
      Decode_Timestamp : Integer := RINA_Policies.Decode_SDNV(Bundle.Primary.Creation_Timestamp);
   begin
      Put_Line("processing bundle for flow: " & Integer'Image(Integer(Flow)));
      Put_Line(" Source EID: " & Bundle.Primary.Source_EID);
      Put_Line(" Destination EID: " & Bundle.Primary.Destination_EID);
      Put_Line(" Creation Timestamp: " & Integer'Image(Decode_Timestamp));
   end Process_Bundle;

   --transmit bundle 
   procedure Transmit_Bundle(Flow : in RINA_Policies.Flow_ID; Bundle : in Bundle) is
      Data_Unit : Rina.Data_Unit_T;
   begin
      RINA_Policies.Create_Data_Unit(Flow, Data_Unit);
      Data_Unit.SDU_Head.Data_Length := RINA_policies.Decode_SDNV(Bundle.Payload.Data_Length);
      RINA_Policies.Transmit_Data_Unit(Flow, Data_Unit);
      Put_Line("Budnle Transmitted for FLow: " & Integer'Image(Integer(Flow)));
   end Transmit_Bundle; 


end RINA_Policies.DTN_Bundle_Protocol;