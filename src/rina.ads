-- SDU       (Service Data Unit)
-- SKB       (Socket Buffer)
-- PCI       (Protocol Control Information)
-- DRF       (Data Run Flag)
-- ECN       (Explicit Congestion Notification)
--with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
package Rina is

   Max_Header_Size : Positive;

   type U8_T is mod 2**8;

   type Header_T is array (Positive range<>) of U8_T; 

   type QoS_Parameter is record
      Priority       : Integer; 
      Latency        : Integer;
      Throughput     : Integer;
   end record;

   --Dynamic addressing 
   --type Address_T is record
      --DIF_ID            : Integer;
      --App_Process_Name  : Unbounded_String; -- application process name (this can be changed to endpointID)
   --end record;

   type Endpoint_ID is record
      App_Process_Name    : Unbounded_String;
      ID                  : Integer;
   end record;


   type PCI_T(Header_Length : Natural) is record
      Header        : Header_T(1 .. Header_Length);
      Length        : Natural;
      DRF_Flag      : Boolean;         -- Data Run Flag
      ECN_Flag      : Boolean;         -- Explicit Congestion Notification
      -- Addressing
      Src_Endpoint  : Endpoint_ID;       -- Source address
      Dst_Endpoint  : Endpoint_ID;       -- Destination address
      -- Sequencing
      Seq_Num       : Natural;     -- Sequence number
      -- QoS
      QoS           : QoS_Parameter;
      --Labeling for PDU category
      PDU_Type      : String(1 .. 255);
   end record;

   --SDU
   type SDU_T is record
      Data_Length    : Natural;
      Data_Payload   : String(1 .. 2048); 
   end record;

   --SKB
   type SKB_T is array(1 ..10) of SDU_T;

   --EFCP Config placeholder 
   type EFCP_Configuration is record
      Max_Window_Size   : Natural;
      Timeout           : Natural;  
   end record;

   type Data_Unit_T(Header_Length : Natural) is record
        Configuration   : EFCP_Configuration;
        Control_Info    : PCI_T(Header_Length);
        SDU_Head        : SDU_T;
        SDU_Tail        : SDU_T;
        Payload_Buffer  : SKB_T;
   end record;

end Rina;