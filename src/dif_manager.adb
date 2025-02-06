with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Containers.Vectors; use Ada.Containers.Vectors;
with application; use application;

package body dif_manager is

   procedure Create_DIF(ID : Integer; Manager : in out DIF_MANAGER) is
      New_DIF : DIF_Access := new DIF'(DIF_ID => ID);
   begin
      Manager.DIFs.Append(New_DIF);
   end Create_DIF;

   procedure Create_Named_DIF(ID : Integer; Name : Unbounded_String; Manager : in out DIF_MANAGER) is
      New_DIF : DIF_Access := new DIF'(DIF_ID => ID, DIF_Name => Name);
   begin
      Manager.DIFs.Append(New_DIF);
   end Create_Named_DIF;

   procedure Disconnect_DIF(ID : Integer; Manager : in out DIF_MANAGER) is
      Index : Natural := Manager.DIFs.First_Index;
   begin
      while Index <= Manager.DIFs.Last_Index loop
         if Manager.DIFs.Element(Index).ID = ID then
            Manager.DIFs.Delete(Index);
            exit;
         end if;
         Index := Index + 1;
      end loop;
   end Disconnect_DIF;

   procedure List_DIFs(Manager : DIF_MANAGER) is
   begin
      for Index in Manager.DIFs.First_Index .. Manager.DIFs.Last_Index loop
         Put_Line("DIF ID: " & Integer'Image(Manager.DIFs.Element(Index).ID) &
                ", Name: " & To_String(Manager.DIFs.Element(Index).Name));
      end loop;
   end List_DIFs;

end dif_manager;