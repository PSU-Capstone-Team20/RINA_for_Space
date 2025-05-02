
with application; use application; 
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Containers.Vectors;
with Ada.Text_IO; use Ada.Text_IO;
with IPC_Manager.IPCP;
with IPCP_Types;
with Policy_Enforcement; use Policy_Enforcement;

package DIF_Manager.Dif is
   type DIF_T is tagged;

   type DIF_Access is access all DIF_T;
   type IPCP_Access is access all IPCP_Types.IPCP_T;

   package Application_Vectors is new Ada.Containers.Vectors
     (Index_Type => Natural, Element_Type => application.application);

   package DIF_Vectors is new Ada.Containers.Vectors
     (Index_Type => Natural, Element_Type => DIF_Access);

   package IPCP_Vectors is new Ada.Containers.Vectors
     (Index_Type => Natural, Element_Type => IPCP_Access);

   package computer_connections is new Ada.Containers.Vectors
      (Index_Type => Natural, Element_Type => Unbounded_String);

   subtype Application_Vector is Application_Vectors.Vector;
   subtype DIF_Vector is DIF_Vectors.Vector;
   subtype IPCP_Vector is IPCP_Vectors.Vector;
   subtype computer_connection is computer_connections.Vector;
   

   type DIF_T is tagged record
      DIF_ID          : Integer;
      DIF_Name        : Unbounded_String;
      Enrolled_IPCPs  : IPCP_Vector;        
      Applications    : Application_Vector; 
      AccessibleDIFs  : DIF_Vector; 
      Computers       : computer_connection;
      Policy          : DIF_Creation_Policy;      
   end record;

   -- creates a DIF with specified ID and adds it to the vector
   function Create_DIF return DIF_Access;
   -- creates a DIF with argument for name and adds it to the vector.
   function Create_Named_DIF(Name : Unbounded_String) return DIF_Access;
   -- returns the ID of the inputted DIF in a vector
   function Get_ID(self : DIF_Access) return Integer;
   -- returns the name of the inputted DIF
   function Get_Name(self : DIF_Access) return Unbounded_String;
   -- deletes the DIF that has the specified ID if possible
   procedure Disconnect_DIF(ID : Integer; vector : in out DIF_Vector);
   -- adds a pair of DIFs to each other's accessible DIFs
   
   procedure Pair_DIF(first : in out DIF_Access; second : in out DIF_Access);
   -- lists the IDs of accessible DIFs
   procedure List_Accessible_DIF(self : DIF_Access);

   -- TODO Application Connection logic
   -- TODO Application Disconnect Logic
   -- TODO Application Distance Logic
   -- TODO Application Distance Broadcast Logic

end DIF_Manager.Dif;
