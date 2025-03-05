
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
package RINA is
   subtype UString is Ada.Strings.Unbounded.Unbounded_String;

   Null_UString : constant UString := Ada.Strings.Unbounded.Null_Unbounded_String;

   function "+" (Str : String) return UString is
      (To_Unbounded_String (Str));

   function "+" (UStr : UString) return String is
      (To_String (UStr));
end RINA;