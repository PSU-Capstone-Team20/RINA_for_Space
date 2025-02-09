with Ada.Containers.Vectors;
with DIF; use DIF;

package IPCP is

   package IPCP_Vector is new Ada.Containers.Vectors(Index_type => Natural, Element_Type => IPCP_Record);

   type IPCP_ID is new Integer;
   type IPCP_State is (Initialized, Active, Terminated);
   
   -- IPCP record
   type IPCP_Record is record
      ID : IPCP_ID;
      State : IPCP_State;
      Name : String(1 .. 100);
      Address : String(1 .. 100);
      QoS_Params : String(1 .. 50);
      Connected_DIF : DIF.DIF_Access := null;  -- Optional DIF Connection
      -- Allows initialization without connecting to a DIF and support DIF connections when needed
   end record;

   -- IPCP Operations
   procedure Initialize_IPCP(IPCP_Instance : in out IPCP_Record; Name : String; Address : String; QoS_Params : String);
   procedure Activate_IPCP(IPCP_Instance : in out IPCP_Record);
   procedure Terminate_IPCP(IPCP_Instance : in out IPCP_Record);
   function Get_IPCP_State(IPCP_Instance : IPCP_record) return IPCP_State;

   -- DIF Communication
   procedure Connect_To_DIF(IPCP_Instance : in out IPCP_Record; Target_DIF : DIF_Access);
   procedure Disconnect_From_DIF(IPCP_Instance : in out IPCP_Record);
   function Get_Connected_DIF(IPCP_Instance : IPCP_Record) return DIF_Access;

   -- Communication and Acknowledgement for a single IPCP Instance
   procedure Retry_Failed_Communication(IPCP_Instance : in out IPCP_Record; Max_Retries : Integer := 10); 
   procedure Store_Data_Temporarily(IPCP_Instance : in out IPCP_Record; Data : String);
   procedure Transmit_Stored_Data(IPCP_Instance : in out IPCP_Record);

end IPCP;
