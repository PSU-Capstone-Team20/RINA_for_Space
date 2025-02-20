with Ada.Text_IO; use Ada.Text_IO;
with Ada.Calendar; use Ada.Calendar;
with Ada.Containers;
with Ada.Containers.Indefinite_Hashed_Maps;
with IPCP; use IPCP;
with IPC_Manager; use IPC_Manager;

package body IPC_API is

   package Port_Map is new Ada.Containers.Indefinite_Hashed_Maps
     (Key_Type => Port_ID, Element_Type => IPCP_Access, Hash => Ada.Containers.Hash_Type'Input);
   Port_Table : Port_Map.Map;

   function Allocate (Src_Application : Unbounded_String;
                      Dst_Application : Unbounded_String;
                      QoS             : Priority_Level;
                      DIF_M           : in out DIF_MANAGER_T;
                      IPC_M           : in out IPCP_Manager_T) return Port_ID is
      Src_IPCP, Dst_IPCP : IPCP_Access;
      New_Port : Port_ID := Port_ID(Port_Table.Length + 1);
   begin
      Src_IPCP := IPC_Manager.Find_IPCP(IPC_M, Src_Application);
      Dst_IPCP := IPC_Manager.Find_IPCP(IPC_M, Dst_Application);

      if Src_IPCP /= null and Dst_IPCP /= null then
         Port_Table.Insert(New_Port, Dst_IPCP);
         return New_Port;
      else
         return 0;
      end if;
   end Allocate;

   procedure Send (Port : Port_ID; SDU : String; IPC_M : in out IPCP_Manager_T) is
      Dst_IPCP : IPCP_Access;
      PDU : PDU_T;
      PCI : PCI_T := (Seq_Num => 1, DRF_Flag => False, ECN_Flag => False, QoS_ID => 1);
   begin
      if Port_Table.Contains(Port) then
         Dst_IPCP := Port_Table.Element(Port);
         PDU := Create_PDU(ID => "PDU_" & Port'Image, P_Type => DT,
                           Src_Addr => "SrcApp", Dst_Addr => "DstApp",
                           PCI => PCI, SDU => SDU);
         Process_PDU(Dst_IPCP.all, PDU);
      end if;
   end Send;

   function Receive (Port : Port_ID; IPC_M : in out IPCP_Manager_T) return String is
      Dst_IPCP : IPCP_Access;
      Received_PDU : PDU_T;
   begin
      if Port_Table.Contains(Port) then
         Dst_IPCP := Port_Table.Element(Port);
         if Dst_IPCP.PDUs.Length > 0 then
            Received_PDU := Dst_IPCP.PDUs.First_Element;
            Dst_IPCP.PDUs.Delete_First;
            return Received_PDU.SDU;
         end if;
      end if;
      return "";
   end Receive;

   procedure Deallocate (Port : Port_ID; IPC_M : in out IPCP_Manager_T) is
   begin
      if Port_Table.Contains(Port) then
         Port_Table.Delete(Port);
      end if;
   end Deallocate;

end IPC_API;
