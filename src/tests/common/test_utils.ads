with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with DIF_Manager.DIF; use DIF_Manager.DIF;
with Policy_Enforcement; use Policy_Enforcement;
with RIB; use RIB;

package Test_Utils is

   -- Alias for Unbounded_String 
   subtype UString is Unbounded_String;

   -- Constant for an empty Unbounded_String
   Null_UString : constant UString := Null_Unbounded_String;

   -- Convert a bounded String to an Unbounded_String
   function "+" (Str : String) return UString;

   -- Convert an Unbounded_String to a bounded String
   function "+" (UStr : UString) return String;

   function Create_Mock_DIF return DIF_Access;
   function Create_Mock_Named_DIF(Name : UString) return DIF_Access;
   function Create_Mock_Named_DIF_With_Policy(Name : Unbounded_String; Allow : Boolean := True) return DIF_Access;

   function Create_Mock_RIB_Entry return RIB_Entry;

end Test_Utils;
