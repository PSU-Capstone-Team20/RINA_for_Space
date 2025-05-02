with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Containers.Vectors;
with EFCP;
with Transport_Types;
with IPCP_Types;
with Interfaces; use Interfaces;with EFCP; use EFCP;


package IPC_Data_Transfer is 

   type Byte is mod 2**8;
   type Byte_Array is array (Positive range <>) of Byte;

   type PDU_List is array (Positive range  <>) of EFCP.PDU_S_T;
   Max_PDU_Size : constant := 128;
   type Flow_ID is new Natural;

   --EFCP both DTP and DTCP 
   procedure DTP(SDU : in Byte_Array; Fragment_PDU : out PDU_List);
   procedure DTCP(PDU : in out EFCP.PDU_S_T);

   --relaying pdu
   procedure Relay_PDU(PDU : in out EFCP.PDU_S_T);

   --multiplexing 
   procedure Multiplex_PDU(PDU : in EFCP.PDU_S_T; Lower_Flow_ID : out Flow_ID);

   --TODO: maybe, SDU protection

end IPC_Data_Transfer;