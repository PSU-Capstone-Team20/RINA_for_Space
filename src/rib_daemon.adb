with Ada.Text_IO; use Ada.Text_IO;

-- what was said in rib_daemon.ads 
package body rib_daemon is

   --TODO : add CDAP functionality
   task body RIB_Daemon is
      RIB_E : RIB.RIB_Entry;
      Last_Used_Name : Unbounded_String := To_Unbounded_String ("");
   begin
      loop
         select 
            --given a new RIB_Entry, updates the held entry
            accept update(RIB_E_U : RIB.RIB_Entry) do
               RIB_E := RIB_E_U;
            end update;
         or 
            --uses the held entry to update the RIB
            accept operate do
            --checks if the name changed and if it had a name before
            --  if not (RIB_E.Name = Last_Used_Name) then
            --     if not (Last_Used_Name = To_Unbounded_String("")) then
            --        RIB.Delete_Entry (Last_Used_Name);
            --     end if;
            --  end if;
            --updates the RIB
            if RIB.Find_Entry(RIB_E.Name) then
               RIB.Update_Entry (RIB_E.Name, RIB_E);
            else
               RIB.Add_Entry (RIB_E.Name);
               for i in RIB_E.Obj_Type.Comp_Connection.First_Index .. RIB_E.Obj_Type.Comp_Connection.Last_Index loop
                  RIB.Add_Comp (RIB_E.Name, RIB_E.Obj_Type.Comp_Connection(i));
               end loop;
               for i in RIB_E.Obj_Type.Accessible_IPCPs.First_Index .. RIB_E.Obj_Type.Accessible_IPCPs.Last_Index loop
                  RIB.Add_IPCP (RIB_E.Name, RIB_E.Obj_Type.Accessible_IPCPs(i));
               end loop;
               for i in RIB_E.Obj_Type.Active_APNs.First_Index .. RIB_E.Obj_Type.Active_APNs.Last_Index loop
                  RIB.Add_APN (RIB_E.Name, RIB_E.Obj_Type.Active_APNs(i));
               end loop;
            end if;
            
            --updates the last_used_name
            end operate;
         or
            --auto kills the task when main is done
            terminate;
         end select;
      end loop;
   end RIB_Daemon;
end rib_daemon;