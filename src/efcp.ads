with Ada.Text_IO; use Ada.Text_IO;

package EFCP is

   type Protocol_Control_Info is record
      Seq_Num      : Natural;
      Length       : Natural;
   end record;

   type SDU is record
      PCI  : Protocol_Control_Info;
      Data : String(1 .. 1024);
   end record;

   type PDU is record
      PCI  : Protocol_Control_Info;
      Data : String(1 ..1024);
   end record;

   --for data transfer protocol DTP 
   procedure Fragment(S : in out SDU; Fragment_Size : Natural; Fragments : out PDU);
   procedure Reassemble(Packets : in PDU; Reassem_SDU : out SDU);
   procedure Concatenate(S1, S2 : in SDU; Result : out SDU);
   procedure Separation(S : in SDU; P1, P2 : out SDU);

   --for data transer control protocol DTCP 
   procedure Control_Transmit(P : in out PDU);
   procedure Retransmit(P : PDU);
   procedure Flow_Control(P : in PDU);


end EFCP;