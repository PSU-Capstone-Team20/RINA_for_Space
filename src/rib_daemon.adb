with Ada.Text_IO; use Ada.Text_IO;
with RIB;

package body rib_daemon is

   --TODO : add CDAP functionality
   --TODO : add hash map updating functionality for all items in system
   task body T is
      map : RIB.RIB_Hashed_Maps.Map;
   begin
      Put_Line("Interacting with CDAP, updating RIB entries");
      --interacts with the CDAP
      --checks if any RIB_Entry/DIF/IPCP/APN got updated, updated hash map if true
      if map.Is_Empty then
         map := RIB.Get_map;
      end if;
   end T;
end rib_daemon;