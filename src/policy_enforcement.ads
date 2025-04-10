with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package Policy_Enforcement is
 
   type DIF_Creation_Policy is record
      Routing_Strategy     : Unbounded_String;
      Enrollment_Type      : Unbounded_String;
      Max_Flows            : Natural;
      Allow_Creation       : Boolean;
   end record;

   function Get_DIF_Creation_Policy(Name : Unbounded_String) return DIF_Creation_Policy;

end Policy_Enforcement;