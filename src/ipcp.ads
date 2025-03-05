limited with DIF;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Containers.Vectors;
with Ada.Calendar; use Ada.Calendar;

package IPCP is

   -- type DIF_Access is access all DIF.DIF;
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
   type PDU_T is record
      ID        : String(1 .. 7);         -- Unique identifier for the PDU
      P_Type    : PDU_Type;                -- Type of PDU
      Src_Addr  : String(1 .. 11);         -- Source address, IPv6 address length
      Dst_Addr  : String(1 .. 11);         -- Destination address, IPv6 address length
      PCI       : PCI_T;                   -- Protocol Control Information
      SDU       : String(1 .. 1024);       -- The actual payload
      Timestamp : Ada.Calendar.Time;       -- Time of creation or reception
   end record;

   package PDU_Vector is new Ada.Containers.Vectors
     (Index_Type => Natural, Element_Type => PDU_T);
   subtype PDU_Buffer is PDU_Vector.Vector;

   type IPCP_T is tagged record
      ID            : Unbounded_String;
      State         : IPCP_State := Initialized;
      Name          : Unbounded_String;
      Address       : Unbounded_String;
      QoS_Params    : Priority_Level;
      Connected_DIF : Unbounded_String;  -- Optional DIF Connection
      PDUs          : PDU_Buffer; -- PDU Queue for storing pending transmissions
   end record;

   type IPCP_Access is access all IPCP_T;

   -- Creates a new IPCP instance
   function Create_IPCP(Name : Unbounded_String; ID : Unbounded_String) return IPCP_Access;

   -- Creates a new PDU instance
   function Create_PDU(ID : String;
                       P_Type : PDU_Type;
                       Src_Addr : String;
                       Dst_Addr : String;
                       PCI      : PCI_T;
                       SDU      : String) return PDU_T;

   -- Handles internal data flow within the IPCP
   procedure Process_PDU(IPCP : in out IPCP_T; PDU : in PDU_T);

end IPCP;
