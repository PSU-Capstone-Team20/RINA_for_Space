with Ada.Calendar; use Ada.Calendar;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package Transport_Types is

   -- Define priority levels (used in QoS, flow, EFCP scheduling)
   type Priority_Lvl is (Low, Medium, High);
   for Priority_Lvl use
     (Low    => 0,
      Medium => 1,
      High   => 2);

   -- Define the PCI_T type
   type PCI_T is tagged record
      Src_CEP_ID  : Unbounded_String;          -- Local EID of the sending app/IPCP, CEP = Connection End Point
      Dst_CEP_ID  : Unbounded_String;          -- Local EID of the receiving app/IPCP
      Seq_Num     : Natural;                 -- Sequence number
      DRF_Flag    : Boolean;                  -- Data Run Flag
      ECN_Flag    : Boolean;                  -- Explicit Congestion Notification(flow control)
      QoS_ID      : Natural;                  -- Quality of Service identifier
      TTL         : Duration := 10.0;                  -- Time to Live, for SDU Protection to prevent infinite loops in network
      Ack_Req     : Boolean := False;
      Retransmit  : Boolean := False;
      Timestamp   : Time := Clock;
   end record;

   -- Define the PDU_Type enumeration with representations
   --type PDU_Type is (DT, CTL, ACK, ERR);

   type SDU_T is record
      PCI  : PCI_T;
      Data : Unbounded_String;
   end record;

   -- Define the PDU_T type
   type PDU_T is tagged record
      ID        : String(1 .. 7);          -- Unique identifier for the PDU
      PCI       : PCI_T;                   -- Protocol Control Information
      Data      : Unbounded_String;       -- The actual payload
      --P_Type    : PDU_Type;              -- Type of PDU
      --Omit DIF only for the PDU address to allow a PDU to take any DIF to its destination regardless of what DIFs are active
   end record;

end Transport_Types;