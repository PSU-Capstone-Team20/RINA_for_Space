with Ada.Streams.Stream_IO;

package body fakeComp is
   task body fake_Comp is
      Daemon : rib_daemon.RIB_Daemon;
      IPC_M : IPC_Manager.IPCP_Manager_T;
      DIF_C : dif_connections.Vector;
      --application vector
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
            end change_name;
         or
            accept add_IPCP (IPCP_Name : Unbounded_String) do
               IPC_Manager.Create_IPCP(IPCP_Name, IPC_M);
               IPC_M.Managed_IPCPs(IPC_M.Managed_IPCPs.Last_Index).Connected_Computer := Name;
            end add_IPCP;
         or 
            accept connect_DIF (DIF_Name : Unbounded_String) do
               DIF_C.append(DIF_Name);
            end connect_DIF;
         or
            --call everytime the computer would normally operate
            accept operate do
            --  for i in DIF_C.First_Index .. DIF_C.Last_Index loop
            --     RIB_E.Obj_Type.Connected_DIFs.Append (DIF_C(i).DIF_Name);
            --  end loop;
            --  RIB_E.Obj_Type.Comp_Connection.Append(Name);
            --  for i in IPC_M.Managed_IPCPs.First_Index .. IPC_M.Managed_IPCPs.Last_Index loop
            --     IPCP_O.IPCP := IPC_M.Managed_IPCPs(i).Name;
            --     IPCP_O.Associated_Computer := IPC_M.Managed_IPCPs(i).Connected_Computer;
            --     RIB_E.Obj_Type.Accessible_IPCPs.Append(IPCP_O);
            --  end loop;
            for i in DIF_C.First_Index .. DIF_C.Last_Index loop
               RIB_E.Name := DIF_C(i);
               RIB_E.Obj_Type.Comp_Connection.Append(Name);
               for j in IPC_M.Managed_IPCPs.First_Index .. IPC_M.Managed_IPCPs.Last_Index loop
                  IPCP_O.IPCP := IPC_M.Managed_IPCPs(j).Name;
                  IPCP_O.Associated_Computer := IPC_M.Managed_IPCPs(j).Connected_Computer;
                  RIB_E.Obj_Type.Accessible_IPCPs.Append(IPCP_O);
               end loop;
               --tells the Daemon to update entry
               Daemon.update(RIB_E);
               --  tells the daemon to operate
               Daemon.operate;
               --  clears the entry
               RIB_E.Obj_Type := RIB_E_O_B;
            end loop;
            --TODO : APN loop
            --  Daemon.update(RIB_E);
            --  RIB_E.Obj_Type := RIB_E_O_B;
            --  Daemon.operate;
            end operate;
         or
            --auto kills the task when main is done
            terminate;
         end select;
      end loop;
   end fake_Comp;
end fakeComp;