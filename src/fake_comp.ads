with RIB_Daemon;
with IPC_Manager;
with dif;
with RIB;
with ada.Strings.Unbounded; use ada.Strings.Unbounded;

package Fake_Comp is
   task type fake_comp is
      entry change_name(newName : Unbounded_String);
      entry operate;
   end fake_comp;
end Fake_Comp;