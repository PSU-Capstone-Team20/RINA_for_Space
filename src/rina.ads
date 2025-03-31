with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Containers.Vectors;

package RINA is
   
   type Address_Element is record
      Name         : Unbounded_String;
      Address_Type : Unbounded_String;
   end record;

   package Address_Vectors is new Ada.Containers.Vectors (
      Index_Type => Natural, Element_Type => Address_Element
   );

end RINA;
