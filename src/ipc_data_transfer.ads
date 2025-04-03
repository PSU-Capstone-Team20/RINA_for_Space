with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Containers.Vectors;
with Transport_Types; use Transport_Types;
with IPCP_Types; use IPCP_Types;
with Rina_BP_Bundle; use Rina_BP_Bundle;

package IPC_Data_Transfer is 

   --function for SDU delimiting 
   function Delimit_SDU(Data : Unbounded_String) return SDU_T;

   --EFCP DTP functions
   function DTP_Fragment(SDU : SDU_T; Fragment_Size : Positive) return PDU_Buffer;
   function DTP_Reassemble(PDUs : PDU_Buffer) return SDU_T;

   --EFCP DTCP procedure
   procedure DTCP_Control(PDUs : in out PDU_Buffer);

   --relaying task 
   procedure Relay_PDU(PDU : PDU_T; Dest : Unbounded_String);

   --multiplexing
   procedure Multiplex_Flows(Flows : in Flow_List; PDUs : in out PDU_Buffer);

   --SDU prtect
   procedure Protect_SDU(SDU : in out SDU_T);

   --adding bundle protocol
   function Bundle_To_SDU(B: Bundle) return SDU_T;

   --getting sdu from bundle 
   function SDU_To_Bundle(S : SDU_T) return Bundle;





  

end IPC_Data_Transfer;