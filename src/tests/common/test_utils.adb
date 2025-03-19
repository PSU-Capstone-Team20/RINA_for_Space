with Ada.Assertions; use Ada.Assertions;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with DIF; use DIF;

package body Test_Utils is

   --  package Mock_DIF_Vectors is new Ada.Containers.Vectors(Positive, DIF_Access);
   --  use Mock_DIF_Vectors;

   function "+" (Str : String) return UString is
   begin
      return To_Unbounded_String(Str);
   end "+";

   function "+" (UStr : UString) return String is
   begin
      return To_String(UStr);
   end "+";

   procedure Init_Test_Env is
   begin
      null;
   end Init_Test_Env;

   procedure Cleanup_Test is
   begin
      null;
   end Cleanup_Test;

   -- Create a mock DIF with default values
   function Create_Mock_DIF return DIF_Access is
      Mock_DIF : constant DIF_Access := new DIF.DIF;
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
      Mock_DIF : constant DIF_Access := new DIF.DIF;
   begin
      Mock_DIF.DIF_ID := 0;
      Mock_DIF.DIF_Name := Name;
      Mock_DIF.AccessibleDIFs.Clear;
      Mock_DIF.AccessibleDIFs.Set_Length(0);
      Mock_DIF.Applications.Clear;
      Mock_DIF.Applications.Set_Length(0);
      return Mock_DIF;
   end Create_Mock_Named_DIF;

   --  function Create_Mock_DIF_Vector return DIF_Vectors.Vector is
   --     Mock_DIF_Vec : Mock_DIF_Vectors.Vector;
   --     DIF1, DIF2, DIF3 : DIF_Access;
   --  begin
   --     DIF1 := Create_Mock_DIF;
   --     DIF1.DIF_ID := 1;
   --     DIF2 := Create_Mock_DIF;
   --     DIF2.DIF_ID := 2;
   --     DIF3 := Create_Mock_DIF;
   --     DIF3.DIF_ID := 3;

   --     Mock_DIF_Vec.Append(DIF1);
   --     Mock_DIF_Vec.Append(DIF2);
   --     Mock_DIF_Vec.Append(DIF3);
   --     return Mock_DIF_Vec;
   --  end Create_Mock_DIF_Vector;

end Test_Utils;