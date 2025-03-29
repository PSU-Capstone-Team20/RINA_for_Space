with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with IPC_Manager.IPCP; use IPC_Manager.IPCP;
with Ada.Unchecked_Deallocation;

package body IPC_Manager is

   procedure Free_IPCP is new Ada.Unchecked_Deallocation(IPCP_T, IPCP_Access);

   -- Create_IPCP allocates a new IPCP using Make_IPCP and adds it to the manager's list
   procedure Create_IPCP(
      Name               : Unbounded_String;
      --Address            : Unbounded_String;
      --Connected_Computer : Unbounded_String;
      Manager            : in out IPCP_Manager_T
   ) is
   begin
      if Find_IPCP(Manager, Name) /= null then
         Log.Error("Create Failed: IPCP '" & To_String(Name) & "' already exists.");
         return;
      end if;

      declare
         New_IPCP : IPCP_Access := new IPCP_T'(
            IPC_Manager.IPCP.Make_IPCP(Name)
         );
      begin
         Manager.Managed_IPCPs.Append(New_IPCP);
      exception
         when Storage_Error =>
            Log.Error("Out of memory while creating IPCP: " & To_String(Name));
      end;
   end Create_IPCP;

   procedure List_IPCPs(Manager : IPCP_Manager_T) is
   begin
      if Manager.Managed_IPCPs.Is_Empty then
         Log.Info("No IPCPs currently managed.");
      else
         for IPCP of Manager.Managed_IPCPs loop
            Log.Info("IPCP: " & To_String(IPCP.Name));
         end loop;
      end if;
   end List_IPCPs;

   -- Finds an IPCP instance by Name
   function Find_IPCP(Manager : IPCP_Manager_T; Name : Unbounded_String) return IPCP_Access is
   begin
      for IPCP_Instance of Manager.Managed_IPCPs loop
         if To_String(IPCP_Instance.Name) = To_String(Name) then
            return IPCP_Instance;
         end if;
      end loop;
      return null; -- IPCP not found
   end Find_IPCP;

   -- Procedure to delete an IPCP by name
   procedure Delete_IPCP(Manager : in out IPCP_Manager_T; Name : Unbounded_String) is
   begin
      for Index in Manager.Managed_IPCPs.First_Index .. Manager.Managed_IPCPs.Last_Index loop
         declare
            IPCP_Instance : IPCP_Access := Manager.Managed_IPCPs(Index);
         begin
            if To_String(IPCP_Instance.Name) = To_String(Name) then
               Free_IPCP(IPCP_Instance);
               Manager.Managed_IPCPs.Delete(Index);
               Log.Info("Deleted IPCP: " & To_String(Name));
               return;
            end if;
         end;
      end loop;

      Log.Warning("Delete Failed: IPCP '" & To_String(Name) & "' not found.");
   end Delete_IPCP;

   -- Allocates a new flow from a source IPCP to a destination IPCP
   -- TO-DO : Add in CDAP Logic
   procedure Allocate_Flow(
      Manager  : in out IPCP_Manager_T;
      Src_Name : Unbounded_String;
      Dst_Name : Unbounded_String;
      Flow     : Flow_Info_T
   ) is
      Src_IPCP : IPCP_Access := Find_IPCP(Manager, Src_Name);
      Dst_IPCP : IPCP_Access := Find_IPCP(Manager, Dst_Name);
   begin
      if Src_IPCP = null or else Dst_IPCP = null then
         Log.Error("One or both IPCPs not found for flow allocation.");
         return;
      end if;

      IPC_Manager.IPCP.Add_Flow(Src_IPCP.all, Flow);
      IPC_Manager.IPCP.Add_Flow(Dst_IPCP.all, Flow);

      Log.Info("Flow ID " & Flow.Flow_ID'Image & " allocated between "
         & To_String(Src_Name) & " and " & To_String(Dst_Name));
   end Allocate_Flow;

end IPC_Manager;
