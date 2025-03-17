with RIB_Daemon;
with IPC_Manager;
with dif;
with RIB;
with ada.Strings.Unbounded; use ada.Strings.Unbounded;
with Ada.Containers.Vectors;

package fakeComp is
      package dif_connections is new Ada.Containers.Vectors
         (Index_Type => Natural, Element_Type => Unbounded_String);
   task type fake_comp is
      entry change_name(newName : Unbounded_String);
      entry add_IPCP(IPCP_Name : Unbounded_String);
      entry connect_DIF(DIF_Name : Unbounded_String);
      entry operate;
   end fake_comp;
end fakeComp;