with Ada.Streams.Stream_IO;
package body mockcomp is
   task body mock_comp is
      Daemon : rib_daemon.RIB_Daemon;
      IPC_M : IPC_Manager.IPCP_Manager_T;
      DIF_C : DIF_manager.DIF.DIF_Vectors.Vector;
      RIB_O : RIB.RIB_Obj;
      RIB_O_O_B : RIB.RIB_Obj_Obj; --a blank object to simplify deletion process
      Name : Unbounded_String;

      --TODO : add functionality to add/remove DIFs and IPCPs
      --TODO : APNs
   begin
      loop
         select
            --input a string, change the name of the fake computer
            accept change_name(newName : Unbounded_String) do
               Name := newName;
               RIB_O.Comp_Connection := newName;
            end change_name;
         or
            --call everytime the computer would normally operate
            accept operate do
            --do DIF stuff
            
            --do IPCP stuff
            for I in IPC_M.Managed_IPCPs.First_Index .. IPC_M.Managed_IPCPs.Last_Index loop
               RIB_O.Obj_Obj_Type.Accessible_IPCPs.Append(IPC_M.Managed_IPCPs(I).Name);
            end loop;
            --TODO : APN loop
            for I in 1 .. 5 loop
               null;
            end loop;
            --tells the Daemon to update this computer's object in DIF entry it is present on
            for I in DIF_C.First_Index .. DIF_C.Last_Index loop
               Daemon.update(RIB_O, DIF_C(I).DIF_Name); -- updates the Daemon on the current RIB_Object
               Daemon.operate; -- has the Daemon operate using the previous information
            end loop;
            --clears the entry
            RIB_O.Obj_Obj_Type := RIB_O_O_B;
            end operate;
         or
            accept delete do
            --clear the RIB of this item's RIB entries
            for I in DIF_C.First_Index .. DIF_C.Last_Index loop
               Daemon.empty(DIF_C(I).DIF_Name, Name);
            end loop;
            --kill the Daemon
            Daemon.delete;
            --clear all data
            IPC_M.Managed_IPCPs.Clear;
            IPC_M.Name := To_Unbounded_String("");
            DIF_C.Clear;
            RIB_O.Comp_Connection := To_Unbounded_String("");
            RIB_O.Obj_Obj_Type.Accessible_IPCPs.Clear;
            RIB_O.Obj_Obj_Type.Active_APNs.Clear;
            Name := To_Unbounded_String("");
            --kills the task
            end delete;
            exit;
         or
            --auto kills the task when main is done
            terminate;
         end select;
      end loop;
   end mock_comp;
end mockcomp;