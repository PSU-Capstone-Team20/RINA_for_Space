with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package body Policy_Enforcement is 

   function Get_DIF_Creation_Policy(Name : Unbounded_String) return DIF_Creation_Policy is
      Policy : DIF_Creation_Policy;
   begin
      if To_String(Name) = "not allowed" then
         Policy.Allow_Creation := False;
         Policy.Routing_Strategy := To_Unbounded_String("none");
         Policy.Enrollment_Type  := To_Unbounded_String("none");
         Policy.Max_Flows := 0;
      else
         Policy.Allow_Creation := True;
         Policy.Routing_Strategy := To_Unbounded_String( "manual");
         Policy.Enrollment_Type := To_Unbounded_String("Automated");
         Policy.Max_Flows := 100;
      end if;
      return Policy;
   end Get_DIF_Creation_Policy;



end Policy_Enforcement;