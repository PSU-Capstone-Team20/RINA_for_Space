with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Containers.Vectors;
with Ada.Calendar; use Ada.Calendar;

package IPC_Manager.IPCP is

   type IPCP_State is (Initialized, Active, Inactive, Disconnected);
   type Priority_Level is range 0 .. 2;

   -- Define the PCI_T type
   type PCI_T is record
      Seq_Num   : Natural;                  -- Sequence number
      DRF_Flag  : Boolean;                  -- Data Run Flag
      ECN_Flag  : Boolean;                  -- Explicit Congestion Notification
      QoS_ID    : Natural;                  -- Quality of Service identifier
      -- need to add fields
   end record;

   -- Define the PDU_Type enumeration with representations
   type PDU_Type is (DT, CTL, ACK, ERR);

   -- Define the PDU_T type
   type PDU_T is tagged record
      ID        : String(1 .. 7);         -- Unique identifier for the PDU
      P_Type    : PDU_Type;                -- Type of PDU
      --Src_Addr  : String(1 .. 11);         -- Source address, IPv6 address length
      --RINA desires a complete lack of handshaking, this makes the Src_Addr uneccessary 
      Dst_EID   : String(1 .. 11);         -- End point ID address, IPv6 address length
      --Omit DIF only for the PDU address to allow a PDU to take any DIF to its destination regardless of what DIFs are active
      PCI       : PCI_T;                   -- Protocol Control Information
      SDU       : String(1 .. 1024);       -- The actual payload
      Timestamp : Ada.Calendar.Time;       -- Time of creation or reception
   end record;

   package PDU_Vector is new Ada.Containers.Vectors
     (Index_Type => Natural, Element_Type => PDU_T);
   subtype PDU_Buffer is PDU_Vector.Vector;

   type IPCP_T is tagged record
      State         : IPCP_State := Initialized;
      Name          : Unbounded_String;
      Address       : Unbounded_String;
      QoS_Params    : Priority_Level;
      Connected_Computer : Unbounded_String;  -- Optional DIF Connection
      PDUs          : PDU_Buffer; -- PDU Queue for storing pending transmissions
   end record;

   -- Creates a new IPCP instance
   function Make_IPCP(Name : Unbounded_String) return IPCP_T;

   -- Creates a new PDU instance
   function Create_PDU(ID        : String;
                       P_Type    : PDU_Type;
                       Dst_EID   : String;
                       PCI       : PCI_T;
                       SDU       : String) return PDU_T;

   -- Handles internal data flow within the IPCP
   procedure Process_PDU(IPCP : in out IPCP_T; PDU : in PDU_T);

   -- Safe Peek PDU to prevent null access
   function Peek_PDU(IPCP : IPCP_T) return PDU_T;
   -- Cleanup memory for PDU Buffer
   procedure Clear_PDU_Buffer(IPCP : in out IPCP_T);

end IPC_Manager.IPCP;
