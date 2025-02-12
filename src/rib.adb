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

      map : Map;
   
    --add procedures for RIB_Entry/DIF/IPCP/APN
    procedure Add_Entry(Name : Unbounded_String) is
      item : RIB_Entry;
    begin
      map.Include(Name'Image, item);
    end Add_Entry;

    procedure Add_DIF(dif : dif.DIF; item : RIB_Entry) is
    begin
      item.Obj_Type.Connected_DIFs.Append(dif);
    end Add_DIF;
    
    procedure Add_IPCP(ipcp : IPCP.ipcp; item : RIB_Entry);
    procedure Add_APN(APN : application.application; item : RIB_Entry);

    --get functions for RIB_Entry/DIF/IPCP/APN
    function Get_Entry(Name: Unbounded_String) return RIB_Entry;
    function Get_DIF(index : Integer; item : RIB_Entry) return dif.DIF;
    function Get_IPCP(index : Integer; item : RIB_Entry) return IPCP.ipcp;
    function Get_APN(index : Integer; item : RIB_Entry) return application.application;

    --delete procedures for RIB_Entry/DIF/IPCP/APN
    procedure Delete_Entry(Name: Unbounded_String);
    procedure Delete_DIF(index : Integer; item : RIB_Entry);
    procedure Delete_IPCP(index : Integer; item : RIB_Entry);
    procedure Delete_APN(index : Integer; item : RIB_Entry);

    --update procedures for RIB_Entry/DIF/IPCP/APN
    procedure Update_Entry(Name: Unbounded_String; item : RIB_Entry);
    procedure Update_DIF(index : Integer; item : RIB_Entry; dif : dif.DIF);
    procedure Update_IPCP(index : Integer; item : RIB_Entry; ipcp : IPCP.ipcp);
    procedure Update_APN(index : Integer; item : RIB_Entry; APN : application.application);
    
    --prints the entire hashed map
    procedure Display_Map;

    --gets the entire map
    function Get_map return Map;
 



end RIB;