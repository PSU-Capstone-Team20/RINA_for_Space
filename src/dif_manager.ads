with application; use application;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Containers.Vectors;
with Ada.Text_IO; use Ada.Text_IO;
with DIF_Manager.Dif; use DIF_Manager.Dif;
with IPC_Manager; use IPC_Manager;

package DIF_Manager is

    package DIF_Vectors is new Ada.Containers.Vectors
      (Index_Type => Natural, Element_Type => DIF_Access);
    subtype DIF_Vector is DIF_Vectors.Vector;

   
    DIFs : DIF_Vector;
    

    -- creates a DIF with specified ID and adds it to the manager
    procedure Create_DIF(ID : Integer);

    -- creates a DIF with argument for name and adds it to the manager
    procedure Create_Named_DIF(ID : Integer; Name : Unbounded_String);
    
    -- deletes the DIF that has the specified ID from the manager
    procedure Disconnect_DIF(Index : Integer);

    --enroll IPCP
    procedure Enroll_IPCP(This : in out DIF_T; IPCP : IPCP_T);
    
    
    -- Lsts all DIFS
    procedure List_DIFs;

end dif_manager;