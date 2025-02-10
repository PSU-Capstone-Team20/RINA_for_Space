with Ada.Containers.Vectors;

package IPCP.Resource_Management is

   type Resource_ID is new Integer;
   type Resource_State is (Allocated, Free, In_Use);

   -- Resource Management Record
   type Resource_Record is record
      ID : Resource_ID;
      Name : String(1 .. 100);
      State : Resource_State;
   end record;

   package Resource_Vector is new Ada.Containers.Vectors(Index_Type => Natural, Element_Type => Resource_Record);
   subtype Resource_List is Resource_Vector.Vector;

   -- Resource Management Operations
   procedure Allocate_Resource(Resources : in out Resource_List; Name : String);
   procedure Release_Resource(Resources : in out Resource_List; ID : Resource_ID);
   function Get_Resource_Status(Resources : Resource_List; ID : Resource_ID) return Resource_State;
   procedure Display_Resources(Resources : Resource_List);

end IPCP.Resource_Management;
