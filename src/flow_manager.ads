with Ada.Containers.Vectors;
with Ada.Text_IO; use Ada.Text_IO;
with Flow_manager; use Flow_Manager;

package Flow_Manager is

   type Flow_ID is new Integer;
   type Flow_State is (Pending, Established, Terminated);

   -- Flow Record
   type Flow_T is record
      ID       : Flow_ID;
      Src      : String(1 .. 50);
      Dest     : String(1 .. 50);
      State    : Flow_State;
   end record;

   -- Flow Vector
   package Flow_Vector is new Ada.Containers.Vectors(Index_Type => Natural, Element_Type => Flow_T);

   -- Flow Manager Structure
   type Flow_Mgr_T is tagged record
      Active_Flows : Flow_Vector.Vector;
   end record;

   -- Flow Management Functions
   procedure Initialize_Flow_Manager(Mgr : in out Flow_Mgr_T);
   procedure Allocate_New_Flow(Mgr : in out Flow_Mgr_T; Src : String; Dest : String);
   procedure Release_Flow(Mgr : in out Flow_Mgr_T; Flow_ID : Flow_ID);
   procedure List_All_Flows(Mgr : Flow_Mgr_T);

end Flow_Manager;
