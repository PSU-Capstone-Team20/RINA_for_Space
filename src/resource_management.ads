with Ada.Containers.Vectors;

package Resource_Management is
   type Resource_T is record
      Resource_ID         : Integer;
      Allocated_Bandwidth : Integer; -- In kbps
      Buffer_Space        : Integer; -- In KB
      CPU_Allocation      : Integer; -- Percentage of CPU
      Utilization         : Float;   -- Usage percentage
      Priority_Level      : Integer;
   end record;

   package Resource_Vector is new Ada.Containers.Vectors (Index_Type => Positive, Element_Type => Resource_T);
   subtype Resource_List is Resource_Vector.Vector;

   procedure Display_Resource(Resource : Resource_T);
end Resource_Management;
