with Ada.Text_IO; use Ada.Text_IO;

-- what was said in rib_daemon.ads 
-- this was created to act as a background process in tandem with the mockcomp to simulate a Daemon that automatically updates and maintains the Resource Information Base without need for explicit coding of such actions or user input
-- not in use, may be altered to work with the rest of the system without the need for the mockcomp package
package body rib_daemon is

   --TODO : add CDAP functionality
   task body RIB_Daemon is
      RIB_O : RIB.RIB_Obj;
      target_DIF : Unbounded_String;
      Last_Used_Name : Unbounded_String := To_Unbounded_String ("");
   begin
      loop
         select 
            --given a new RIB_Entry, updates the held entry
            accept update(RIB_O_I : RIB.RIB_Obj; DIF : Unbounded_String) do
               RIB_O := RIB_O_I;
               target_DIF := DIF;
            end update;
         or 
            --uses the held entry to update the RIB
            accept operate do
            --checks if the name changed and if it had a name before
            if not (RIB_O.Comp_Connection = Last_Used_Name) then
               if not (Last_Used_Name = To_Unbounded_String("")) then
                  --delete old Computer from RIB
                  RIB.Delete_Comp (target_DIF, Last_Used_Name);
              end if;
            end if;
            --updates the RIB
            --attempt to find the Computer
            --if found, update it with the new object
            if RIB.Find_Comp (target_DIF, RIB_O.Comp_Connection) then
               RIB.Update_Comp (target_DIF, RIB_O.Comp_Connection, RIB_O);
            --if not, create it
            else
               RIB.Add_Comp (target_DIF, RIB_O.Comp_Connection);
               for I in RIB_O.Obj_Obj_Type.Accessible_IPCPs.First_Index .. RIB_O.Obj_Obj_Type.Accessible_IPCPs.Last_Index loop
                  RIB.Add_IPCP (target_DIF, RIB_O.Comp_Connection, RIB_O.Obj_Obj_Type.Accessible_IPCPs(I));
               end loop;
               for I in RIB_O.Obj_Obj_Type.Active_APNs.First_Index .. RIB_O.Obj_Obj_Type.Active_APNs.Last_Index loop
                  RIB.Add_APN (target_DIF, RIB_O.Comp_Connection, RIB_O.Obj_Obj_Type.Active_APNs(I));
               end loop;
            end if;
            --updates the last_used_name
            end operate;
         or
            accept empty(DIF : Unbounded_String; Comp : Unbounded_String) do
               --deletes the object from the entry
               RIB.Delete_Comp (DIF, Comp);
            end empty;
         or
            accept delete do
               --clear data, exits the loop, and kills the task
               target_DIF := To_Unbounded_String("");
               RIB_O.Comp_Connection := To_Unbounded_String("");
               RIB_O.Obj_Obj_Type.Accessible_IPCPs.Clear;
               RIB_O.Obj_Obj_Type.Active_APNs.Clear;
               Last_Used_Name := To_Unbounded_String("");
            end delete;
            exit;
         or
            --auto kills the task when main is done
            terminate;
         end select;
      end loop;
   end RIB_Daemon;
end rib_daemon;