with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package body IPC_Manager is

   procedure Create_IPCP(Name : Unbounded_String; ID : Unbounded_String; Manager : in out IPCP_Manager_T) is
      New_IPCP : IPCP_Access := IPCP.Create_IPCP(Name, ID);
   begin
      Manager.Managed_IPCPs.Append(New_IPCP);
   end Create_IPCP;

   procedure List_IPCPs(Manager : IPCP_Manager_T) is
   begin
      for I in Manager.Managed_IPCPs.First_Index .. Manager.Managed_IPCPs.Last_Index loop
         Put_Line("IPCP ID: " & To_String(Manager.Managed_IPCPs(I).ID) & ", Name: " & To_String(Manager.Managed_IPCPs(I).Name));
      end loop;
   end List_IPCPs;

   --  procedure Connect_IPCP_to_DIF(IPCP_ID : Unbounded_String; DIF : in out DIF_T; Manager : in out IPCP_Manager_T) is
   --  begin
   --     for I in Manager.Managed_IPCPs.First_Index .. Manager.Managed_IPCPs.Last_Index loop
   --        if Manager.Managed_IPCPs(I).ID = IPCP_ID then
   --           Manager.Managed_IPCPs(I).Connected_DIF := DIF'Access;
   --           DIF.Member_IPCPs.Append(Manager.Managed_IPCPs(I));
   --           Put_Line("IPCP " & To_String(Manager.Managed_IPCPs(I).Name) & " connected to DIF " & To_String(DIF.DIF_Name));
   --           exit;
   --        end if;
   --     end loop;
   --  end Connect_IPCP_to_DIF;

end IPC_Manager;
