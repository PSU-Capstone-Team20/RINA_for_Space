with application; use application;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Containers.Vectors;
with Ada.Text_IO; use Ada.Text_IO;
with DIF; use DIF;

package dif_manager is
    type DIF_MANAGER_T is tagged;

    package DIF_Vectors is new Ada.Containers.Vectors
      (Index_Type => Natural, Element_Type => DIF_Access);
    subtype DIF_Vector is DIF_Vectors.Vector;

    type DIF_MANAGER_T is tagged record
        DIFs : DIF_Vector;
    end record;

    -- creates a DIF with specified ID and adds it to the manager
    procedure Create_DIF(ID : Integer; Manager : in out DIF_MANAGER_T);

    -- creates a DIF with argument for name and adds it to the manager
    procedure Create_Named_DIF(ID : Integer; Name : Unbounded_String; Manager : in out DIF_MANAGER_T);
    
    -- deletes the DIF that has the specified ID from the manager
    procedure Disconnect_DIF(Index : Integer; Manager : in out DIF_MANAGER_T);
    
    -- Lsts all DIFS
    procedure List_DIFs(Manager : DIF_MANAGER_T);

end dif_manager;