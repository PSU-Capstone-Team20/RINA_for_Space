with application; use application;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Containers.Vectors;
with Ada.Text_IO; use Ada.Text_IO;
limited with DIF_Manager.Dif; 
with IPC_Manager; use IPC_Manager;
limited with IPCP_Types;

package DIF_Manager is


    

    -- creates a DIF with specified ID and adds it to the manager
    procedure Create_DIF(ID : Integer);

    -- creates a DIF with argument for name and adds it to the manager
    function Create_Named_DIF(Name : Unbounded_String) return DIF_Manager.Dif.DIF_Access;
    
    -- deletes the DIF that has the specified ID from the manager
    procedure Disconnect_DIF(Index : Integer);

    --enroll IPCP
    procedure Enroll_IPCP(Owner_DIF : in out DIF_Manager.Dif.DIF_T);
    
    
    -- Lsts all DIFS
    procedure List_DIFs;

end DIF_Manager;