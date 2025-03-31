with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Containers.Vectors;
with Application; use Application;
with DIF_Manager.Dif; use DIF_Manager.Dif;
with IPC_Manager; use IPC_Manager;

package body DIF_Manager is

   procedure Create_DIF(ID : Integer) is
      New_DIF : DIF_Access := createDIF;
   begin
      New_DIF.DIF_ID := ID;
      DIFs.Append(New_DIF);
   end Create_DIF;

   procedure Create_Named_DIF(ID : Integer; Name : Unbounded_String) is
      New_DIF : DIF_Access := createNamedDIF(Name);
   begin
      New_DIF.DIF_ID := ID;
      DIFs.Append(New_DIF);
   end Create_Named_DIF;

   procedure Disconnect_DIF(Index : Integer) is
   begin
      DIFs.Delete(Index);
   end Disconnect_DIF;

   procedure Enroll_IPCP(This : in out DIF_T; IPCP : IPCP_T) is 
   begin
      This.IPCPs.Append(IPCP);
   end Enroll_IPCP;

   procedure List_DIFs is
   begin
      for Index in DIFs.First_Index .. DIFs.Last_Index loop
         Put_Line("DIF ID: " & Integer'Image(DIFs.Element(Index).DIF_ID) &
                ", Name: " & To_String(DIFs.Element(Index).DIF_Name));
      end loop;
   end List_DIFs;

end DIF_Manager;