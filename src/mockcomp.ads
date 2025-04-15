with RIB_Daemon;
with IPC_Manager;
with DIF_manager.DIF;
with RIB;
with ada.Strings.Unbounded; use ada.Strings.Unbounded;

package mockcomp is
   task type mock_comp is
      entry change_name(newName : Unbounded_String);
      entry operate;
      entry delete;
   end mock_comp;
end mockcomp;