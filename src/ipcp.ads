with Ada.Containers.Vectors;
with Ada.Calendar;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with DIF; use DIF;

package IPCP is

   type IPCP_ID is new Integer;
   type IPCP_State is (Initialized, Active, Terminated);
   type Priority_Level is range 0 .. 2; -- 0 = Bulk, 1 = Normal, 2 = Expedited

   type PDU_ID is new Integer;
   type PDU_Type is (Data_Transfer, Control, Acknowledgement, Error_Control);

   -- Protocol Data Units(PDUs)
   type PDU_T is record
      ID             : PDU_ID;
      PDU_T_Field    : PDU_Type;
      Src            : String(1 .. 50);
      Dst           : String(1 .. 50);
      PCI            : String(1 .. 128); -- Need actual PCI type record
      SDU            : String(1 .. 1024); -- Payload
      Timestamp      : Ada.Calendar.Time;
      --  EFCP_Flags     : String(1 .. 32);
      --  Multiplex_Info : String(1 .. 64);
      --  Security_Check : String(1 .. 64);
   end record;

   package PDU_Vector is new Ada.Containers.Vectors(Index_Type => Natural, Element_Type => PDU_T);
   
   -- IPCP record
   type IPCP_T is record 
      ID            : IPCP_ID;
      State         : IPCP_State;
      Name          : Unbounded_String;
      Address       : Unbounded_String;
      QoS_Params    : Priority_Level;
      Connected_DIF : DIF.DIF_Access := null;  -- Optional DIF Connection
      -- Allows initialization without connecting to a DIF and support DIF connections when needed
      PDU_Buffer    : PDU_Vector.Vector; -- PDU Queue for storing pending transmissions
   end record;

   package IPCP_Vector is new Ada.Containers.Vectors(Index_type => Natural, Element_Type => IPCP_T);

   -- IPCP Operations
   function Get_Next_IPCP_ID return IPCP_ID; -- Function to get the next unique IPCP ID
   procedure Initialize_IPCP(IP : in out IPCP_T; Name : String; Address : String; QoS_Params : Priority_Level);
   procedure Activate_IPCP(IP : in out IPCP_T);
   procedure Terminate_IPCP(IP : in out IPCP_T);
   function Get_IPCP_State(IP : IPCP_T) return IPCP_State;

   -- DIF Communication
   procedure Connect_To_DIF(IP : in out IPCP_T; Target_DIF : DIF_Access);
   --  procedure Disconnect_From_DIF(IP : in out IPCP_T);
   --  function Get_Connected_DIF(IP : IPCP_T) return DIF_Access;

   -- PDU Handling Functions
   --  procedure Encapsulate_PDU(SDU : String; Src : String; Dst : String; PDU_I : out PDU_T);
   --  procedure Queue_PDU(IP : in out IPCP_T; PDU_I : PDU_T);
   --  procedure Transmit_PDU(IP : in out IPCP_T);
   --  procedure Process_Received_PDU(IP : in out IPCP_T; PDU_I : PDU_T);
   --  procedure Retry_Failed_PDU(IP : in out IPCP_T);
   --  procedure Apply_SDU_Protection(PDU_I : in out PDU_T);

end IPCP;
