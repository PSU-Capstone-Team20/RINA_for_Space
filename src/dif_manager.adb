with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Containers.Vectors;
with Application; use Application;
with DIF_Manager.Dif; use DIF_Manager.Dif;
with IPC_Manager; use IPC_Manager;
with IPCP_Types; use IPCP_Types;
with IPC_Manager.IPCP; use IPC_Manager.IPCP;

package body DIF_Manager is

   package DIF_Vectors is new Ada.Containers.Vectors
      (Index_Type => Natural, Element_Type => DIF_Manager.Dif.DIF_Access);
   subtype DIF_Vector is DIF_Vectors.Vector;

   --  package IPCP_Vectors is new Ada.Containers.Vectors
   --    (Index_Type => Natural, Element_Type => DIF_Manager.Dif.IPCP_Access);
   
   --  IPCPs : IPCP_Vector;

   
   DIFs : DIF_Vector;

   procedure Create_DIF(ID : Integer) is
      New_DIF : DIF_Access := Create_DIF;
   begin
      New_DIF.DIF_ID := ID;
      DIFs.Append(New_DIF);
   end Create_DIF;

   function Create_Named_DIF(Name : Unbounded_String) return DIF_Access is
      New_DIF : DIF_Manager.Dif.DIF_Access := DIF_Manager.Dif.Create_Named_DIF(Name);
   begin
      --New_DIF.DIF_ID := ID;
      DIFs.Append(New_DIF);
      return New_DIF;
   end Create_Named_DIF;

   procedure Disconnect_DIF(Index : Integer) is
   begin
      DIFs.Delete(Index);
   end Disconnect_DIF;

   -- WIP:Create_IPCP and Make_IPCP need clarification. Access issues as it needs to 
   -- have the manager tagged to each instance of IPCP? 
   procedure Enroll_IPCP(Owner_DIF : in out DIF_T) is
      New_IPCP : IPCP_T := Make_IPCP.To_Unbounded_String(Owner_DIF.DIF_Name);
   begin
      
      Append(Owner_DIF, New_IPCP);
   end Enroll_IPCP;

   procedure List_DIFs is
   begin
      for Index in DIFs.First_Index .. DIFs.Last_Index loop
         Put_Line("DIF ID: " & Integer'Image(DIFs.Element(Index).DIF_ID) &
                ", Name: " & To_String(DIFs.Element(Index).DIF_Name));
      end loop;
   end List_DIFs;

end DIF_Manager;