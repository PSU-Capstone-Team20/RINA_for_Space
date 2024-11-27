-- SDU       (Service Data Unit)
-- SKB       (Socket Buffer)
-- PCI       (Protocol Control Information)
-- DRF       (Data Run Flag)
-- ECN       (Explicit Congestion Notification)
with Ada.Text_IO; use Ada.Text_IO;
package Rina is

   Max_Header_Size : Positive;

   type U8_T is mod 2**8;

   type Header_T is array (Positive range<>) of U8_T; 

   subtype QoS_Id_T is U8_T;

   type Address_T is null record;

   type PCI_T (Header_Length : Natural) is record
      Header    : Header_T (1 .. Header_Length);
      Length    : Natural;
      DRF_Flag  : Boolean;         -- Data Run Flag
      ECN_Flag  : Boolean;         -- Explicit Congestion Notification
      -- Addressing
      Src_Addr  : Address_T;       -- Source address
      Dst_Addr  : Address_T;       -- Destination address
      -- Sequencing
      Seq_Num   : Natural;     -- Sequence number
      -- QoS
      QoS_ID    : QoS_Id_T;
   end record;

   --  type Data_Unit_T is record
   --     Configuration   : EFCP_Configuration;
   --     Control_Info    : PCI_T;
   --     SDU_Head        : SDU_T;
   --     SDU_Tail        : SDU_T;
   --     Payload_Buffer  : SKB_T;
   --  end record;

end Rina;