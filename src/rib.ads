with Ada.Containers.Ordered_Multisets;
with Ada.Containers.Vectors;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Strings.Unbounded.Hash;
with dif;
with ipcp;
with application;

with Ada.Text_IO; use Ada.Text_IO;

package RIB is
    
    -- vectors that contain the applications/difs/ipcps that exist on each rib object
    package Application_Vectors is new Ada.Containers.Vectors
     (Index_Type => Natural, Element_Type => Ada.Strings.Unbounded.Unbounded_String);
    package DIF_Vectors is new Ada.Containers.Vectors
     (Index_Type => Natural, Element_Type => Ada.Strings.Unbounded.Unbounded_String);
    package IPCP_Vectors is new Ada.Containers.Vectors
     (Index_Type => Natural, Element_Type => Ada.Strings.Unbounded.Unbounded_String);

    -- represents a live computer in the system as a RIB object
    type RIB_Obj is record 
      Accessible_IPCPs : IPCP_Vectors.Vector; -- list of ipcps system has access to
      Connected_DIFs  : DIF_Vectors.Vector; -- list of difs system has access to
      Active_APNs  : Application_Vectors.Vector; -- applications running on system
    end record;

    type RIB_Entry is record
      --Name       : Unbounded_String;
      Obj_Type   : RIB_Obj;
    end record;

    package RIB_Hashed_Maps is new Ada.Containers.Indefinite_Hashed_Maps
    (
      Key_Type => Unbounded_String,
      Element_Type => RIB_Entry,
      Hash => Ada.Strings.Unbounded.Hash,
      Equivalent_Keys => "="
    );
    use RIB_Hashed_Maps;

   --test procedure: example for how to add and access 
    --  procedure test is
    --   tester : Map;
    --   RIB_Entry_test : RIB_Entry;
    --  begin
    --  tester.Include ("John's Computer", RIB_Entry_test);
    --  tester("John's Computer").Element.Obj_Type.Accessible_IPCPs(0);
    --  null;
    --  end test;

    --add procedures for RIB_Entry/DIF/IPCP/APN
    procedure Add_Entry(Name : Unbounded_String);
    procedure Add_DIF(dif : Unbounded_String; item : out RIB_Entry);
    procedure Add_IPCP(ipcp : Unbounded_String; item : out RIB_Entry);
    procedure Add_APN(APN : Unbounded_String; item : out RIB_Entry);

    --get functions for RIB_Entry/DIF/IPCP/APN
    function Get_Entry(Name: Unbounded_String) return RIB_Entry;
    function Get_DIF(index : Integer; item : RIB_Entry) return Unbounded_String;
    function Get_IPCP(index : Integer; item : RIB_Entry) return Unbounded_String;
    function Get_APN(index : Integer; item : RIB_Entry) return Unbounded_String;

    --delete procedures for RIB_Entry/DIF/IPCP/APN
    procedure Delete_Entry(Name: Unbounded_String);
    procedure Delete_DIF(index : Integer; item : out RIB_Entry);
    procedure Delete_IPCP(index : Integer; item : out RIB_Entry);
    procedure Delete_APN(index : Integer; item : out RIB_Entry);

    --update procedures for RIB_Entry/DIF/IPCP/APN
    procedure Update_Entry(Name: Unbounded_String; item : RIB_Entry);
    procedure Update_DIF(index : Integer; item : out RIB_Entry; dif : Unbounded_String);
    procedure Update_IPCP(index : Integer; item : out RIB_Entry; ipcp : Unbounded_String);
    procedure Update_APN(index : Integer; item : out RIB_Entry; APN : Unbounded_String);
    
    --prints the entire hashed map
    procedure Display_Map;
    --TODO: Display procedure for specific entries that are being searched.

    --gets the entire map
    function Get_map return RIB_Hashed_Maps.Map;
 
end RIB;