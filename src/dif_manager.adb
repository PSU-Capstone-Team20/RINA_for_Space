with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Containers.Vectors;
with Application; use Application;
with Dif; use Dif;

package body DIF_Manager is

   procedure Create_DIF(ID : Integer; Manager : in out DIF_MANAGER_T) is
      New_DIF : DIF_Access := createDIF;
   begin
      New_DIF.DIF_ID := ID;
      Manager.DIFs.Append(New_DIF);
   end Create_DIF;

   procedure Create_Named_DIF(ID : Integer; Name : Unbounded_String; Manager : in out DIF_MANAGER_T) is
      New_DIF : DIF_Access := createNamedDIF(Name);
   begin
      New_DIF.DIF_ID := ID;
      Manager.DIFs.Append(New_DIF);
   end Create_Named_DIF;

   procedure Disconnect_DIF(Index : Integer; Manager : in out DIF_MANAGER_T) is
   begin
      Manager.DIFs.Delete(Index);
   end Disconnect_DIF;

   procedure List_DIFs(Manager : DIF_MANAGER_T) is
   begin
      for Index in Manager.DIFs.First_Index .. Manager.DIFs.Last_Index loop
         Put_Line("DIF ID: " & Integer'Image(Manager.DIFs.Element(Index).DIF_ID) &
                ", Name: " & To_String(Manager.DIFs.Element(Index).DIF_Name));
      end loop;
   end List_DIFs;

end DIF_Manager;