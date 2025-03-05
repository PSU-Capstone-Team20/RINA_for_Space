with Ada.Streams.Stream_IO;
package body Fake_Comp is
   task body fake_Comp is
      Daemon : rib_daemon.RIB_Daemon;
      IPC_M : IPC_Manager.IPCP_Manager_T;
      DIF_C : dif.DIF_Vectors.Vector;
      RIB_E : RIB.RIB_Entry;
      RIB_E_O_B : RIB.RIB_Obj;
      IPCP_O : RIB.IPCP_obj;
      Name : Unbounded_String;

      --TODO : add functionality to add/remove DIFs and IPCPs
      --TODO : APNs
   begin
      loop
         select
            --input a string, change the name of the fake computer
            accept change_name(newName : Unbounded_String) do
               Name := newName;
               RIB_E.Name := newName;
            end change_name;
         or
            --call everytime the computer would normally operate
            accept operate do
               for i in DIF_C.First_Index .. DIF_C.Last_Index loop
                  RIB_E.Obj_Type.Connected_DIFs.Append (DIF_C(i).DIF_Name);
               end loop;
               for i in IPC_M.Managed_IPCPs.First_Index .. IPC_M.Managed_IPCPs.Last_Index loop
                  IPCP_O.IPCP := IPC_M.Managed_IPCPs(i).Name;
                  IPCP_O.Associated_DIF := IPC_M.Managed_IPCPs(i).Connected_DIF;
                  RIB_E.Obj_Type.Accessible_IPCPs.Append(IPCP_O);
               end loop;
               --TODO : APN loop
               --tells the Daemon to update it's entry
               Daemon.update(RIB_E);
               --clears the entry
               RIB_E.Obj_Type := RIB_E_O_B;
               --tells the daemon to operate
               Daemon.operate;
            end operate;
         or
            --auto kills the task when main is done
            terminate;
         end select;
      end loop;
   end fake_Comp;
end Fake_Comp;