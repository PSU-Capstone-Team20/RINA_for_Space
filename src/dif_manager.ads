with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Containers.Vectors;
with IPCP_Manager;          use IPCP_Manager;
use IPCP_Manager.IPCP_Name;

package dif_Manager is
  --  Max of 256 DIFs can exist in the system
  MAX_DIF_COUNT : constant Natural := 256;

  --  Max of 8 IPCs can be registered to a DIF
  MAX_IPC_COUNT : constant Natural := 8;

  type IPCP_Array is array (Natural range <>) of IPCP;

  -- DELETE???
  type DIF_Types is (Normal, Ethernet);

  type Procedure_Access is access procedure;

  type Application is record
    Name             : Unbounded_String;
    Proc             : Procedure_Access;
    Timeout_Intitial : Integer := 0;
    Timeout          : Integer := 0;
    Communicating    : Boolean;
  end record;

  package Application_Vectors is new Ada.Containers.Vectors
   (Index_Type => Positive, Element_Type => Application);

  subtype Application_Vector is Application_Vectors.Vector;

  type DIF is tagged record
    Name                 : Bounded_String;
    DIF_Type             : DIF_Types := Normal;
    IPCPs                : IPCP_Vector;
    IPCP_Count           : Natural   := 0;
    Applications         : Application_Vector;
    Enrollment_Constants : Enrollment_Info;
    Timeout_Intitial     : Integer := 0;
    Timeout              : Integer := 0;
  end record;

  type DIF_Access is access all DIF;

  --  Stores all DIFs in the system
  package DIF_Vectors is new Ada.Containers.Vectors
   (Index_Type => Positive, Element_Type => DIF_Access);

  subtype DIF_Vector is DIF_Vectors.Vector;

  DIF_List : DIF_Vector;

  --  Enrollment is the procedure by which an IPCP joins an existing DIF and is initialized
  --  with enough information to become a fully operational DIF member.
  procedure Enroll
   (Self : in out DIF; IPC_Process : IPCP); -- Flow_Req : Flow);

  -- verify that IPCP has minimum info to be useful in a DIF
  function Verify_IPCP (IPC_Process : in IPCP) return Boolean;

  --  Register an application to a DIF
  procedure Register
   (Self : in out DIF; Appl_Name : String; Proc : Procedure_Access);

  --  Get application from the DIF
  function Get_Application
   (Self : in out DIF; Name : String) return Application;

  --  Remove application from the DIF
  procedure Remove_Application
   (Self : in out DIF; Name : String);

  --  Verify application exists within the DIF
  function Application_Exists (Self : in out DIF; Name : String) return Boolean;

  --  Get IPCP from DIF
  function Get_IPCP (Self : in out DIF; Name : String) return IPCP;

  --  Remove IPCP from the DIF
  procedure Remove_IPCP
    (Self : in out DIF; Name : String);

  --  Verify IPCP exists within DIF
  function IPCP_Exists (Self : in out DIF; Name : String) return Boolean;

  --  Checks the set of all DIFs in the system for a DIF with the matching name and type
  function Get (Name : String; DIF_Type : DIF_Types) return DIF;

  --  Checks if a DIF exists with the same Name and Type parameters
  function Exists (Name : String; DIF_Type : DIF_Types) return Boolean;

  --  Create a DIF
  function Create (Name : String; DIF_Type : DIF_Types) return DIF_Access;

  --  Deletes a DIF
  procedure Delete
    (Name : String); 

  -- Decrements timeout value of all DIFs being managed
  procedure Clock_Tick;

  -- Resets the timeout clock on DIF
  procedure DIF_Clock_Reset 
    (Name : String);

  -- Resets the timeout clock on IPCP
  procedure IPCP_Clock_Reset 
    (Self : in out DIF; Name : String);

  -- Resets the timeout clock on Application
  procedure Application_Clock_Reset 
    (Self : in out DIF; Name : String);

end dif_Manager;
 
 