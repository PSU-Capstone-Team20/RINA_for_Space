with Ada.Containers;
with Ada.Containers.Vectors;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Strings.Hash;
with DIF_Manager.Dif; use DIF_Manager.Dif;
with GNAT.Table;
with IPC_Manager.IPCP;
with application;


with Ada.Text_IO; use Ada.Text_IO;

package body RIB is

   -- TODO: logic into each procedure and functions 
   
    --add procedures for RIB_Entry/DIF/IPCP/APN
    procedure Add_Entry(Name : Unbounded_String) is
      item : RIB_Entry;
    begin
      if not map.Contains(Name) then
         item.Name := Name;
         map.Insert(Name, item);
         Put_Line("New RIB Entry: " & To_String(Name));
      else
         Put_Line("Existing RIB entry for: " & To_String(Name));
      end if;
      
    end Add_Entry;

   --   procedure Add_DIF(Name : Unbounded_String; dif : in out Unbounded_String) is
   --   begin
   --     if map.Contains(Name) then
   --        declare
   --           Capture : RIB_Entry renames map(Name);            
   --        begin
   --           Capture.Obj_Type.Connected_DIFs.Append(dif);
   --           Put_Line("Added DIF: " & To_String(dif) & " to " & To_String(Name));
   --        end;
   --     else
   --        Put_Line("RIB Entry could not be found for: " & To_String(Name));
   --     end if;
      
   --   end Add_DIF;
    
    procedure Add_IPCP(Name : Unbounded_String; CompName : Unbounded_String; ipcp : in out IPCP_obj) is
    begin
      if map.Contains(Name) then
         declare
            Capture : RIB_Entry renames map(Name);
         begin
            Capture.Obj_Type(CompName).Obj_Obj_Type.Accessible_IPCPs.Append(ipcp);
            Put_Line("Added IPCP: " & To_String(ipcp.IPCP) & " to " & To_String(Name));
         end;
      else
         Put_Line("RIB Entry could not be found for: " & To_String(Name));
      end if;
      
    end Add_IPCP;
    
    procedure Add_Comp(Name : Unbounded_String; Comp : in out Unbounded_String) is 
    begin
      if map.Contains(Name) then
         declare
            Capture : RIB_Entry renames map(Name);
            item : RIB_Obj;
         begin 
            if not Capture.Obj_Type.Contains(Name) then
               item.Comp_Connection := Comp;
               Capture.Obj_Type.Insert(Comp, item);
            else
               --  Put_Line("Existing RIB entry for: " & To_String(Name));
               null;
            end if;
            Put_Line("Added Computer connection: " & To_String(Comp));
         end;
      else
         Put_Line("RIB Entry could not be found for: " & To_String(Name));
      end if;
    end Add_Comp;

    procedure Add_APN(Name : Unbounded_String; CompName : Unbounded_String; APN : in out Unbounded_String) is
    begin
      if map.Contains(Name) then
         declare
            Capture : RIB_Entry renames map(Name);
         begin
            Capture.Obj_Type(CompName).Obj_Obj_Type.Active_APNs.Append(APN);
            Put_Line("Added APN: " & To_String(APN) & " to " & To_String(Name));
         end;
      else
         Put_Line("RIB Entry could not be found for: " & To_String(Name));
      end if;
      
    end Add_APN;

    --get functions for RIB_Entry/DIF/IPCP/APN
    function Get_Entry(Name: Unbounded_String) return RIB_Entry is
    begin
      if map.Contains(Name) then
         return map(Name);
      else
         raise Constraint_Error with "RIB Entry does not exist for: " & To_String(Name);
      end if;
    end Get_Entry;

    function Find_Entry(Name : Unbounded_String) return Boolean is
    begin
      if map.Contains (Name) then
         return true;
      end if;
      return false;
   end Find_Entry;

   --   function Get_DIF(index : Integer; item : RIB_Entry) return Unbounded_String is
   --   begin
   --     return item.Obj_Type.Connected_DIFs(index);
   --   end Get_DIF;
    function Get_IPCP(index : Integer; CompName : Unbounded_String; item : RIB_Entry) return IPCP_obj is
    begin
      return item.Obj_Type(CompName).Obj_Obj_Type.Accessible_IPCPs(index);
    end Get_IPCP;
    function Get_APN(index : Integer; CompName : Unbounded_String; item : RIB_Entry) return Unbounded_String is
    begin
      return item.Obj_Type(CompName).Obj_Obj_Type.Active_APNs(index);
    end Get_APN;

    --delete procedures for RIB_Entry/DIF/IPCP/APN
    procedure Delete_Entry(Name: Unbounded_String) is
    begin
      if map.Contains(Name) then
         map.Delete(Name);
         Put_Line("Deleted RIB Entry: " & To_String(Name));
      else
         Put_Line("There is no RIB Entry for: " & To_String(Name));
      end if;
      --  RIB_Hashed_Maps.Delete(map, Name);
      --  above line caused execution to freeze when the procedure was called successfully
    end Delete_Entry;

   --   procedure Delete_DIF(index : Integer; item : in out RIB_Entry) is
   --   begin
   --     item.Obj_Type.Connected_DIFs.Delete(index);
   --   end Delete_DIF;

    procedure Delete_IPCP(index : Integer; CompName : Unbounded_String; item : in out RIB_Entry) is
    begin
      item.Obj_Type(CompName).Obj_Obj_Type.Accessible_IPCPs.Delete(index);
    end Delete_IPCP;

    procedure Delete_APN(index : Integer; CompName : Unbounded_String; item : in out RIB_Entry) is
    begin
      item.Obj_Type(CompName).Obj_Obj_Type.Active_APNs.Delete(index);
    end Delete_APN;

    --update procedures for RIB_Entry/DIF/IPCP/APN
    procedure Update_Entry(Name: Unbounded_String; item : RIB_Entry) is
    begin
       map.Include(Name, item);
      --  if map.Contains(Name) then
      --     map(Name) := item;
      --  else
      --     Put_Line("No RIB Entry for: " & To_String(Name));
      --  end if;
    end Update_Entry;

   --   procedure Update_DIF(index : Integer; item : in out RIB_Entry; dif : Unbounded_String) is
   --   begin
   --     item.Obj_Type.Connected_DIFs(index) := dif;
   --   end Update_DIF;

    procedure Update_IPCP(index : Integer; CompName : Unbounded_String; item : in out RIB_Entry; ipcp : IPCP_obj) is
    begin
      item.Obj_Type(CompName).Obj_Obj_Type.Accessible_IPCPs(index) := ipcp;
    end Update_IPCP;
    
    procedure Update_APN(index : Integer; CompName : Unbounded_String; item : in out RIB_Entry; APN : Unbounded_String) is
    begin
      item.Obj_Type(CompName).Obj_Obj_Type.Active_APNs(index) := APN;
    end Update_APN;
    
    --prints the entire RIB hashed map 
    --iterates through RIB hashed map
    --iterates through Comp hashed map 
    procedure Display_Map is
      --  Iter : RIB_Hashed_Maps.Cursor := map.First;
      temp : Unbounded_String;
    begin
      Put_Line("Current Map is: ");
      --loop through the RIB_Entries
      for C in map.Iterate loop
         Put_Line (map(C).Name'Image);
         
         --loop through the RIB_Objs, done this way since Obj_Type is a hashed map using an Unbounded String as the hash
         for X in map(C).Obj_Type.Iterate loop
            Put_Line(map(C).Obj_Type(X).Comp_Connection'Image);
            --loop through the IPCPs in each RIB_Objs RIB_Obj_Obj
            for i in map(C).Obj_Type(X).Obj_Obj_Type.Accessible_IPCPs.First_Index .. map(C).Obj_Type(X).Obj_Obj_Type.Accessible_IPCPs.Last_Index loop
               Put_Line (map(C).Obj_Type(X).Obj_Obj_Type.Accessible_IPCPs(i).IPCP'Image);
            end loop;
            --loop through the APNs in each RIB_Objs RIB_Obj_Obj
            for i in map(C).Obj_Type(X).Obj_Obj_Type.Active_APNs.First_Index .. map(C).Obj_Type(X).Obj_Obj_Type.Active_APNs.Last_Index loop
               temp := map(C).Obj_Type(X).Obj_Obj_Type.Active_APNs(i);
               Put_Line(temp'Image);
            end loop;
         end loop;
      end loop;
      
    end Display_Map;
    --TODO: adding procedure for displaying specific map that is being searched.

    --gets the entire map
    function Get_map return RIB_Hashed_Maps.Map is
    begin
      return map;
    end Get_map;
 



end RIB;