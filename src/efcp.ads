with Ada.Text_IO; use Ada.Text_IO;

package EFCP is

   type Protocol_Control_Info is record
      Seq_Num      : Natural;
      Length       : Natural;
   end record;

   type SDU_T is record
      PCI  : Protocol_Control_Info;
      Data : String(1 .. 1024);
   end record;

   type PDU_T is record
      PCI  : Protocol_Control_Info;
      Data : String(1 ..1024);
   end record;

   --for data transfer protocol DTP 
   procedure Fragment(S : in out SDU_T; Fragment_Size : Natural; Fragments : out PDU_T);
   procedure Reassemble(Packets : in PDU_T; Reassem_SDU : out SDU_T);
   procedure Concatenate(S1, S2 : in SDU_T; Result : out SDU_T);
   procedure Separation(S : in SDU_T; P1, P2 : out SDU_T);

   --for data transer control protocol DTCP 
   procedure Control_Transmit(P : in out PDU_T);
   procedure Retransmit(P : PDU_T);
   procedure Flow_Control(P : in PDU_T);


end EFCP;