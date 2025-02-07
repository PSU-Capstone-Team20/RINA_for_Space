with Ada.Text_IO; use Ada.Text_IO;
with IPCP;

package body IPCP.IPC_Manager is

   procedure Initialize_Manager(Manager : in out IPCP_List) is
   begin
      Manager := IPCP_Vector.Empty_Vector;
   end Initialize_Manager;

   procedure Add_IPCP(Manager : in out IPCP_List; IPCP_Instance : IPCP.IPCP_T) is
   begin
      Manager.Append(IPCP_Instance);
   end Add_IPCP;

   procedure Remove_IPCP(Manager : in out IPCP_List; ID : Integer) is
      Found : Boolean := False;
   begin
      for Index in 1 .. Manager.Length loop
         if Manager.Element(Index).ID = ID then
            Manager.Delete(Index);
            Found := True;
            exit;
         end if;
      end loop;

      if not Found then
         Put_Line("IPCP with ID " & ID'Image & " not found.");
      end if;
   end Remove_IPCP;

   function Locate_IPCP(Manager : IPCP_List; ID : Integer) return IPCP.IPCP_T is
   begin
      for Index in 1 .. Manager.Length loop
         if Manager.Element(Index).ID = ID then
            return Manager.Element(Index);
         end if;
      end loop;

      raise Program_Error with "IPCP with ID " & ID'Image & " not found.";
   end Locate_IPCP;

   function Find_DIF_Address(Manager : IPCP_List; DIF_ID : Integer) return RINA.Address_T is
   begin
      for Index in Manager.First .. Manager.Last loop
         if Manager.Element(Index).DIF_ID = DIF_ID then
            return Manager.Element(Index).Address;
         end if;
      end loop;
      raise Program_Error with "DIF_ID not found in the manager.";
   end Find_DIF_Address;
 
   procedure Display_All_IPCPs(Manager : IPCP_List) is
   begin
      for Index in 1 .. Manager.Length loop
         Put_Line("IPCP #" & Index'Image);
         Put_Line("ID: " & Manager.Element(Index).ID'Image);
         Put_Line("Name: " & Manager.Element(Index).Name'Image);
         Put_Line("Address - DIF_ID: " & Manager.Element(Index).Address.DIF_ID'Image);
         Put_Line("Address - App_Process_Name: " & To_String(Manager.Element(Index).Address.App_Process_Name));
         Put_Line("QoS Priority: " & Manager.Element(Index).QoS_Params.Priority'Image);

      if Manager.Length = 0 then
         Put_Line("No IPCPs available.");
         return;
      end if;
   end Display_All_IPCPs;

   procedure Display_All_Flows(Manager : IPCP_List) is
   begin
      for IPCP_Instance of Manager loop
         IPCP.Display_Flows(IPCP_Instance);
      end loop;
   end Display_All_Flows;

   procedure Display_All_Resources(Manager : IPCP_List) is
   begin
      for IPCP_Instance of Manager loop
         IPCP.Display_Resources(IPCP_Instance);
      end loop;
   end Display_All_Resources;


end IPCP.IPC_Manager;