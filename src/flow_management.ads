with Ada.Containers.Vectors;
with RINA;
with RINA_Policies;

package Flow_Management is
   type Flow_State_T is (Active, Inactive, Pending, Closed);

   type Flow_T is record
      Flow_ID : Integer;
      Source_Address : RINA.Address_T;
      Destination_Address : RINA.Address_T;
      QoS_Params : RINA_Policies.QoS_Parameter;
      State : Flow_State_T;
      Start_Timestamp : String(1 .. 20);
      End_Timestamp : String(1 .. 20);
   end record;

   package Flow_Vector is new Ada.Containers.Vectors (Index_Type => Positive, Element_Type => Flow_T);
   subtype Flow_List is Flow_Vector.Vector;

   procedure Display_Flow(Flow : Flow_T);
end Flow_Management;
