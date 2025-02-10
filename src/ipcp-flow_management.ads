with Ada.Containers.Vectors;

package IPCP.Flow_Management is

   type Flow_ID is new Integer;
   type Flow_State is (Pending, Active, Closed);

   type Flow_Record is record
      ID : Flow_ID;
      Source : String(1 .. 100);
      Dest: String(1 .. 100);
      QoS : String(1 .. 50);
      State : Flow_State;
   end record;

   package Flow_Vector is new Ada.Containers.Vectors(Index_Type => Natural, Element_Type => Flow_Record);
   subtype Flow_List is Flow_Vector.Vector;

   -- Flow Management Operations
   procedure Create_Flow(Flows : in out Flow_List; Source : String; Dest : String; QoS : String);
   procedure Close_Flow(Flows : in out Flow_List; ID : Flow_ID);
   function Get_Flow_Status(Flows : Flow_List; ID : Flow_ID) return Flow_State;
   procedure Display_Flows(Flows : Flow_List);

end IPCP.Flow_Management;
