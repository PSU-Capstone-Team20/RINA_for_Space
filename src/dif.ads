with ipcp;
with application; use application; 
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Containers.Vectors;
with Ada.Text_IO; use Ada.Text_IO;

package dif is
   type DIF is tagged;

   type DIF_Access is access all DIF;
   type IPCP_Access is access all ipcp.ipcp;

   package Application_Vectors is new Ada.Containers.Vectors
     (Index_Type => Natural, Element_Type => application.application);
   package DIF_Vectors is new Ada.Containers.Vectors
     (Index_Type => Natural, Element_Type => DIF_Access);
   package IPCP_Vectors is new Ada.Containers.Vectors
     (Index_Type => Natural, Element_Type => IPCP_Access);

   subtype Application_Vector is Application_Vectors.Vector;
   subtype DIF_Vector is DIF_Vectors.Vector;
   subtype IPCP_Vector is IPCP_Vectors.Vector;

   type DIF is tagged record
      DIF_ID          : Integer;
      MemberIPCPs    : IPCP_Vector;        
      Applications    : Application_Vector; 
      AccessibleDIFs : DIF_Vector; 
      -- TODO: Will need to add policies here when branches are merged        
   end record;

   -- creates a DIF with specified ID and adds it to the vector
   procedure createDIF(ID : Integer; vector : in out DIF_Vector);
   -- returns the ID of the inputted DIF in a vector
   function getID(self : DIF_Access) return Integer;
   -- deletes the DIF that has the specified ID if possible
   procedure deleteDIF(ID : Integer; vector : in out DIF_Vector);
   -- adds a pair of DIFs to each other's accessible DIFs
   procedure pairDIF(first : in out DIF_Access; second : in out DIF_Access);
   -- lists the IDs of accessible DIFs
   procedure listAccessibleDIF(self : DIF_Access);
   -- creates an IPCP with specified string and adds it to the provided DIF
   procedure createIPCP(name : Unbounded_String; self : in out DIF_Access);
   -- lists all member IPCPs of a provided DIF
   procedure listIPCP(self : DIF_Access);
   -- deletes IPCP with given name from given DIF
   procedure deleteIPCP(name : Unbounded_String; self : in out DIF_Access);

end dif;
