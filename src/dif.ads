Limited with IPCP;
with application; use application; 
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Containers.Vectors;
with Ada.Text_IO; use Ada.Text_IO;


package dif is
   type DIF is tagged;

   type DIF_Access is access all DIF;
   type IPCP_Access is access all IPCP.IPCP_T;

   package Application_Vectors is new Ada.Containers.Vectors
     (Index_Type => Natural, Element_Type => application.application);
   package DIF_Vectors is new Ada.Containers.Vectors
     (Index_Type => Natural, Element_Type => DIF_Access);
   -- package IPCP_Vectors is new Ada.Containers.Vectors
   --  (Index_Type => Natural, Element_Type => IPCP_Access);
   package computer_connections is new Ada.Containers.Vectors
      (Index_Type => Natural, Element_Type => Unbounded_String);

   subtype Application_Vector is Application_Vectors.Vector;
   subtype DIF_Vector is DIF_Vectors.Vector;
   subtype computer_connection is computer_connections.Vector;
   -- subtype IPCP_Vector is IPCP_Vectors.Vector;

   type DIF is tagged record
      DIF_ID          : Integer;
      DIF_Name        : Unbounded_String;
      --MemberIPCPs    : IPCP_Vector;        
      Applications    : Application_Vector; 
      AccessibleDIFs  : DIF_Vector; 
      Computers       : computer_connection;
      -- TODO: Will need to add policies here when branches are merged        
   end record;

   -- creates a DIF with specified ID and adds it to the vector
   function createDIF return DIF_Access;
   -- creates a DIF with argument for name and adds it to the vector.
   function createNamedDIF(name : Unbounded_String) return DIF_Access;
   -- returns the ID of the inputted DIF in a vector
   function getID(self : DIF_Access) return Integer;
   -- returns the name of the inputted DIF
   function getName(self : DIF_Access) return Unbounded_String;
   -- deletes the DIF that has the specified ID if possible
   procedure disconnectDIF(ID : Integer; vector : in out DIF_Vector);
   -- adds a pair of DIFs to each other's accessible DIFs
   -- TODO so this is using broadcasting to find other available DIFs - Needs tasking
   procedure pairDIF(first : in out DIF_Access; second : in out DIF_Access);
   -- lists the IDs of accessible DIFs
   procedure listAccessibleDIF(self : DIF_Access);

   -- TODO directory and pathing
   -- TODO Application Connection logic
   -- TODO Application Disconnect Logic
   -- TODO Application Distance Logic
   -- TODO Application Distance Broadcast Logic
   
   -- TODO Revist this when IPC_Manager is added in.
   -- lists all member IPCPs of a provided DIF
   -- procedure listIPCP(self : DIF_Access);
   -- deletes IPCP with given name from given DIF
   -- procedure disconnectIPCP(name : Unbounded_String; self : in out DIF_Access);

end dif;
