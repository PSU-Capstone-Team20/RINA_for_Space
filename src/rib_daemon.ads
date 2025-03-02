with Ada.Text_IO; use Ada.Text_IO;
with RIB;
with ada.strings.Unbounded; use ada.strings.Unbounded;

package rib_daemon is
   task type RIB_Daemon is
      entry update(RIB_E_U : RIB.RIB_Entry);
      entry operate;
   end RIB_Daemon;
   --TODO: waiting for other implements currently but still in works.
end rib_daemon;