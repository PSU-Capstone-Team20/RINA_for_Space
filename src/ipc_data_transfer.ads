

package IPC_Data_Transfer is 

<<<<<<< Updated upstream
=======
type Byte is mod 2**8;
type Byte_Array is array (Positive range <>) of Byte;


type PDU_List is array (Positive range  <>) of EFCP.PDU_S_T;
Max_PDU_Size : constant := 128;
type Flow_ID is new Natural;


--SDU delmiting
--procedure Delimit_SDU(Raw_Data : in Byte_Array; SDU : out Byte_Array); 

--EFCP both DTP and DTCP 
procedure DTP(SDU : in Byte_Array; Fragment_PDU : out PDU_List);
procedure DTCP(PDU : in out EFCP.PDU_S_T);

--relaying pdu
procedure Relay_PDU(PDU : in out EFCP.PDU_S_T);

--multiplexing 
procedure Multiplex_PDU(PDU : in EFCP.PDU_S_T; Lower_Flow_ID : out Flow_ID);

--TODO: maybe, SDU protection





>>>>>>> Stashed changes
  

end IPC_Data_Transfer;