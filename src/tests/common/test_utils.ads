with Ada.Assertions; use Ada.Assertions;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with DIF; use DIF;

package Test_Utils is

   -- Alias for Unbounded_String 
   subtype UString is Unbounded_String;

   -- Constant for an empty Unbounded_String
   Null_UString : constant UString := Null_Unbounded_String;

   -- Convert a bounded String to an Unbounded_String
   function "+" (Str : String) return UString;

   -- Convert an Unbounded_String to a bounded String
   function "+" (UStr : UString) return String;

   procedure Init_Test_Env;
   procedure Cleanup_Test;

   function Create_Mock_DIF return DIF_Access;
   function Create_Mock_Named_DIF(Name : UString) return DIF_Access;
   -- function Create_Mock_DIF_Vector return DIF_Vectors.Vector;

end Test_Utils;
