with Ada.Containers;
with Ada.Containers.Vectors;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Strings.Hash;
with dif;
with ipcp;
with application;


with Ada.Text_IO; use Ada.Text_IO;

package body RIB is

   -- TODO: logic into each procedure and functions 

      map : RIB_Hashed_Maps.Map;
   
    --add procedures for RIB_Entry/DIF/IPCP/APN
    procedure Add_Entry(Name : Unbounded_String) is
      item : RIB_Entry;
    begin
      map.Include(Name, item);
    end Add_Entry;

    procedure Add_DIF(dif : Unbounded_String; item : out RIB_Entry) is
    begin
      item.Obj_Type.Connected_DIFs.Append(dif);
    end Add_DIF;
    
    procedure Add_IPCP(ipcp : Unbounded_String; item : out RIB_Entry) is
    begin
      item.Obj_Type.Accessible_IPCPs.Append(ipcp);
    end Add_IPCP;
    procedure Add_APN(APN : Unbounded_String; item : out RIB_Entry) is
    begin
      item.Obj_Type.Active_APNs.Append(APN);
    end Add_APN;

    --get functions for RIB_Entry/DIF/IPCP/APN
    function Get_Entry(Name: Unbounded_String) return RIB_Entry is
    begin
      return map(Name);
    end Get_Entry;
    function Get_DIF(index : Integer; item : RIB_Entry) return Unbounded_String is
    begin
      return item.Obj_Type.Connected_DIFs(index);
    end Get_DIF;
    function Get_IPCP(index : Integer; item : RIB_Entry) return Unbounded_String is
    begin
      return item.Obj_Type.Accessible_IPCPs(index);
    end Get_IPCP;
    function Get_APN(index : Integer; item : RIB_Entry) return Unbounded_String is
    begin
      return item.Obj_Type.Active_APNs(index);
    end Get_APN;

    --delete procedures for RIB_Entry/DIF/IPCP/APN
    procedure Delete_Entry(Name: Unbounded_String) is
    begin
      RIB_Hashed_Maps.Delete(map, Name);
    end Delete_Entry;
    procedure Delete_DIF(index : Integer; item : out RIB_Entry) is
    begin
      item.Obj_Type.Connected_DIFs.Delete(index);
    end Delete_DIF;
    procedure Delete_IPCP(index : Integer; item : out RIB_Entry) is
    begin
      item.Obj_Type.Accessible_IPCPs.Delete(index);
    end Delete_IPCP;
    procedure Delete_APN(index : Integer; item : out RIB_Entry) is
    begin
      item.Obj_Type.Active_APNs.Delete(index);
    end Delete_APN;

    --update procedures for RIB_Entry/DIF/IPCP/APN
    procedure Update_Entry(Name: Unbounded_String; item : RIB_Entry) is
    begin
      map(Name) := item;
    end Update_Entry;
    procedure Update_DIF(index : Integer; item : out RIB_Entry; dif : Unbounded_String) is
    begin
      item.Obj_Type.Connected_DIFs(index) := dif;
    end Update_DIF;
    procedure Update_IPCP(index : Integer; item : out RIB_Entry; ipcp : Unbounded_String) is
    begin
      item.Obj_Type.Accessible_IPCPs(index) := ipcp;
    end Update_IPCP;
    procedure Update_APN(index : Integer; item : out RIB_Entry; APN : Unbounded_String) is
    begin
      item.Obj_Type.Active_APNs(index) := APN;
    end Update_APN;
    
    --prints the entire hashed map
    procedure Display_Map is
    begin
      null;
    end Display_Map;
    --TODO: adding procedure for displaying specific map that is being searched.

    --gets the entire map
    function Get_map return RIB_Hashed_Maps.Map is
    begin
      return map;
    end Get_map;
 



end RIB;