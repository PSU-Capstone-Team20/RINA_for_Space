with Ada.Containers.Ordered_Multisets;
with Ada.Containers.Vectors;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Strings.Unbounded.Hash;
with Ada.Text_IO; use Ada.Text_IO;


package RIB is
    
    -- vectors that contain the applications/difs/ipcps that exist on each rib object
    package Application_Vectors is new Ada.Containers.Vectors
     (Index_Type => Natural, Element_Type => Ada.Strings.Unbounded.Unbounded_String);
    package DIF_Vectors is new Ada.Containers.Vectors
     (Index_Type => Natural, Element_Type => Ada.Strings.Unbounded.Unbounded_String);
    package Comp_Vectors is new Ada.Containers.Vectors
     (Index_Type => Natural, Element_Type => Ada.Strings.Unbounded.Unbounded_String);
    package IPCP_Vectors is new Ada.Containers.Vectors
     (Index_Type => Natural, Element_Type => Ada.Strings.Unbounded.Unbounded_String);
      
   
      --this is a representation of the IPCPs and APNs that would be running on a computer, should only be used by RIB_Obj
    type RIB_Obj_Obj is record
      Active_APNs      : Application_Vectors.Vector; -- applications running on system
      Accessible_IPCPs : IPCP_Vectors.Vector; -- list of ipcps system has access to
      Connected_DIFs   : DIF_Vectors.Vector;
    end record;
   
    -- represents a live computer in the system as a RIB object
    type RIB_Obj is record
     -- Hash_ID          : Ada.Containers.Hash_Type; -- hash value for each entry
      --Accessible_IPCPs : IPCP_Vectors.Vector; -- list of ipcps system has access to
      --Connected_DIFs   : DIF_Vectors.Vector; -- list of difs system has access to
      --Active_APNs      : Application_Vectors.Vector; -- applications running on system

      --RIB_Obj is defined as a string representing the Computer and the IPCPs and APNs running on it
      Comp_Connection  : Unbounded_String; --SHOULD BE THE SAME AS THE HASH
      Obj_Obj_Type : RIB_Obj_Obj;
    end record;
    
    --map structure of the Obj_Type in RIB_Entry
    --holds RIB_Objs, hash is an Unbounded String,
    package Comp_Hashed_Maps is new Ada.Containers.Indefinite_Hashed_Maps
    (
      Key_Type => Unbounded_String,
      Element_Type => RIB_Obj,
      Hash => Ada.Strings.Unbounded.Hash,
      Equivalent_Keys => "="
    );

    type RIB_Entry is record
      Name       : Unbounded_String; --SHOULD BE THE SAME AS THE HASH
      --we shoved a map of the computer representations inside the RIB map
      Obj_Type   : Comp_Hashed_Maps.map;
    end record;

    -- function Hash (Key : Unbounded_String) return Ada.Containers.Hash_Type;

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

    --function Check_Is_Neighbor(A, B : Unbounded_String; Is_Entry : RIB_Entry) return Boolean;

    --add procedures for RIB_Entry/DIF/IPCP/APN
    procedure Add_Entry(Name : Unbounded_String);
   --   procedure Add_DIF(Name : Unbounded_String; dif : in out Unbounded_String);
    procedure Add_IPCP(Name : Unbounded_String; CompName : Unbounded_String; ipcp :  in out Unbounded_String);
    procedure Add_APN(Name : Unbounded_String; CompName : Unbounded_String; APN : in out Unbounded_String);
    procedure Add_Comp(Name : Unbounded_String; Comp : in out Unbounded_String);


    --get functions for RIB_Entry/DIF/IPCP/APN
    function Get_Entry(Name : Unbounded_String) return RIB_Entry;
   --   function Get_DIF(index : Integer; item : RIB_Entry) return Unbounded_String;
    function Get_IPCP(index : Integer; CompName : Unbounded_String; item : RIB_Entry) return Unbounded_String;
    function Get_APN(index : Integer; CompName : Unbounded_String; item : RIB_Entry) return Unbounded_String;
   --   function Get_Comp(index : Integer; item : RIB_Entry) return Unbounded_String;

   --finds if an entry exists
    function Find_Entry(Name :Unbounded_String) return Boolean;

    --delete procedures for RIB_Entry/DIF/IPCP/APN
    procedure Delete_Entry(Name : Unbounded_String);
   --   procedure Delete_DIF(index : Integer; item : in out RIB_Entry);
    procedure Delete_IPCP(index : Integer; CompName : Unbounded_String; item : in out RIB_Entry); 
    procedure Delete_IPCP_By_Name(DIF_Name : Unbounded_String; Comp_Name : Unbounded_String; IPCP_Name : Unbounded_String); 
    procedure Delete_APN(index : Integer; CompName : Unbounded_String; item : in out RIB_Entry);
   --   procedure Delete_Comp(index : Integer; item : in out RIB_Entry);
    procedure Delete_Comp(DIF_Name : Unbounded_String; Comp_Name : Unbounded_String);

    --update procedures for RIB_Entry/DIF/IPCP/APN
    procedure Update_Entry(Name : Unbounded_String; item : RIB_Entry);
   --   procedure Update_DIF(index : Integer; item : in out RIB_Entry; dif : Unbounded_String);
    procedure Update_IPCP(index : Integer; CompName : Unbounded_String; item : in out RIB_Entry; ipcp : Unbounded_String);
    procedure Update_APN(index : Integer; CompName : Unbounded_String; item : in out RIB_Entry; APN : Unbounded_String);
   --   procedure Update_Comp(index : Integer; item : in out RIB_Entry; Comp : Unbounded_String);


    --prints the entire hashed map
    procedure Display_Map;
    --TODO: Display procedure for specific entries that are being searched.

    --gets the entire map
    function Get_map return RIB_Hashed_Maps.Map;


    -- Helper functions for simulation to use
    function Get_All_DIFs return DIF_Vectors.Vector;
    function Get_All_Comps return Comp_Vectors.Vector;
    
    private
      map : RIB_Hashed_Maps.Map;

end RIB;