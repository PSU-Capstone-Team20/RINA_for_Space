with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package body IPC_Manager is

   procedure Create_IPCP(Name : Unbounded_String; Manager : in out IPCP_Manager_T) is
      New_IPCP : IPCP_Access := IPCP.Create_IPCP(Name);
   begin
      Manager.Managed_IPCPs.Append(New_IPCP);
   end Create_IPCP;

   procedure List_IPCPs(Manager : IPCP_Manager_T) is
   begin
      for I in Manager.Managed_IPCPs.First_Index .. Manager.Managed_IPCPs.Last_Index loop
         Put_Line("IPCP Name: " & To_String(Manager.Managed_IPCPs(I).Name));
      end loop;
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

   -- Assign a PDU to an IPCP (avoids circular dependency)
   procedure Assign_PDU(IPCP_Instance : IPCP_Access; PDU : PDU_T) is
   begin
      Put_Line("Assigning PDU ID: " & PDU.ID & " to IPCP: " & To_String(IPCP_Instance.Name));
      IPCP_Instance.PDUs.Append(PDU);
   end Assign_PDU;

end IPC_Manager;
