with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Containers.Vectors;
with RIB;

package RINA is
   
   type Address_Element is record
      Name         : Unbounded_String;
      Address_Type : Unbounded_String;
   end record;

   package Address_Vectors is new Ada.Containers.Vectors (
      Index_Type => Natural, Element_Type => Address_Element
   );

   package Path_Vectors is new Ada.Containers.Vectors(
      Index_Type => Natural, Element_Type => Address_Vectors.Vector, "=" => Address_Vectors."="
   );

   type A_Star_Node is record
      Address : Address_Vectors.Vector;
      gScore : Integer;
      hScore : Integer;
      fScore : Integer;
      parentAddress : Address_Vectors.Vector;
   end record;

   package Node_Vectors is new Ada.Containers.Vectors(
      Index_Type => Natural, Element_Type => A_Star_Node
   );

   function Reconstruct_Path(ClosedList : Node_Vectors.Vector; current : out A_Star_Node) return Path_Vectors.Vector;
   function D_Star_Lite (start : Address_Vectors.Vector; goal : Address_Vectors.Vector) return Path_Vectors.Vector;

end RINA;
