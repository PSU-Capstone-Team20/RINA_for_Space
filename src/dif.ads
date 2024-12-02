with ipcp;
with application; use application; 
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Containers.Vectors;

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
   
   procedure CreateDIF(Self : out DIF; ID : Integer)

end dif;
