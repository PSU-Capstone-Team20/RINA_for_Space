with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;

package body Test_Utils is

   function "+" (Str : String) return UString is
   begin
      return To_Unbounded_String(Str);
   end "+";

   function "+" (UStr : UString) return String is
   begin
      return To_String(UStr);
   end "+";

   -- Create a mock DIF with default values
   function Create_Mock_DIF return DIF_Access is
      Mock_DIF : constant DIF_Access := new DIF_T;
   begin
      Mock_DIF.DIF_ID := 0;
      Mock_DIF.AccessibleDIFs.Clear;
      Mock_DIF.AccessibleDIFs.Set_Length(0);
      Mock_DIF.Applications.Clear;
      Mock_DIF.Applications.Set_Length(0);
      return Mock_DIF;
   end Create_Mock_DIF;

   -- Create a mock DIF with a specified name
   function Create_Mock_Named_DIF(Name : UString) return DIF_Access is
      Mock_DIF : constant DIF_Access := new DIF_T;
   begin
      Mock_DIF.DIF_ID := 0;
      Mock_DIF.DIF_Name := Name;
      Mock_DIF.AccessibleDIFs.Clear;
      Mock_DIF.AccessibleDIFs.Set_Length(0);
      Mock_DIF.Applications.Clear;
      Mock_DIF.Applications.Set_Length(0);
      return Mock_DIF;
   end Create_Mock_Named_DIF;

   function Create_Mock_Named_DIF_With_Policy(Name : Unbounded_String; Allow : Boolean := True) return DIF_Access is
      Mock_DIF : constant DIF_Access := new DIF_T;
      Mock_Policy : DIF_Creation_Policy;
   begin
      Mock_Policy.Allow_Creation := Allow;
      Mock_Policy.Routing_Strategy := +"MockRouting";
      Mock_Policy.Enrollment_Type := +"MockEnrollment";

      if not Mock_Policy.Allow_Creation then
         Put_Line("Mock policy denies creation of DIF: " & To_String(Name));
         return null;
      end if;

      Put_Line("Mock creating DIF: " & To_String(Name));
      Put_Line("Routing is: " & To_String(Mock_Policy.Routing_Strategy));
      Put_Line("Enrollment: " & To_String(Mock_Policy.Enrollment_Type));

      Mock_DIF.DIF_ID := 0;
      Mock_DIF.DIF_Name := Name;
      Mock_DIF.AccessibleDIFs.Clear;
      Mock_DIF.AccessibleDIFs.Set_Length(0);
      Mock_DIF.Applications.Clear;
      Mock_DIF.Applications.Set_Length(0);

      return Mock_DIF;
   end Create_Mock_Named_DIF_With_Policy;


   -- Create a mock RIB_Entry with default values
   function Create_Mock_RIB_Entry return RIB_Entry is
      Mock_Entry : RIB_Entry;
      Mock_Comp  : RIB_Obj;
      Mock_RIB_Obj_Obj : RIB_Obj_Obj;
   begin
      Mock_Entry.Name := +"MockEntry";

      -- Initialize Mock_RIB_Obj_Obj with some default values
      Mock_RIB_Obj_Obj.Active_APNs.Clear;
      Mock_RIB_Obj_Obj.Accessible_IPCPs.Clear;
      Mock_RIB_Obj_Obj.Connected_DIFs.Clear;

      -- Initialize Mock_Comp with the Mock_RIB_Obj_Obj
      Mock_Comp.Comp_Connection := +"MockComp";
      Mock_Comp.Obj_Obj_Type := Mock_RIB_Obj_Obj;

      -- Add Mock_Comp to the Obj_Type map of Mock_Entry
      Mock_Entry.Obj_Type.Include(Mock_Comp.Comp_Connection, Mock_Comp);

      return Mock_Entry;
   end Create_Mock_RIB_Entry;

end Test_Utils;
