with Ada.Text_IO; use Ada.Text_IO;
with Transport_Types; use Transport_Types;

package EFCP is

   type Protocol_Control_Info is record
      Seq_Num      : Natural;
      Length       : Natural;
   end record;

   subtype PDU_S_T is Transport_Types.PDU_T;
   subtype SDU_S_T is Transport_Types.SDU_T;

   --for data transfer protocol DTP 
   procedure Fragment(S : in out SDU_S_T; Fragment_Size : String; Fragments : out PDU_S_T);
   procedure Reassemble(Packets : in PDU_S_T; Reassem_SDU : out SDU_S_T);
   procedure Concatenate(S1, S2 : in SDU_S_T; Result : out SDU_S_T);
   procedure Separation(S : in SDU_S_T; P1, P2 : out SDU_S_T);

   --for data transer control protocol DTCP 
   procedure Control_Transmit(P : in out PDU_S_T);
   procedure Retransmit(P : PDU_S_T);
   procedure Flow_Control(P : in PDU_S_T);

end EFCP;