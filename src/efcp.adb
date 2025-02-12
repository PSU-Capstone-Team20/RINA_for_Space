with Ada.Text_IO; use Ada.Text_IO;

--
--package for Error and Flow Control Protocol(EFCP) split into two protocol machines DTP and DTCP
--DTP : Data Transfer Protocol - fragmentation, reassembly, sequencing, concatenation, and separation
--DTCP : Data Transfer Control Protocol - mechanisms that are loosely coupled to transported SDU
--DTCP cont. - transmission control, retransmission control, and flow control
--
package body EFCP is

   --splitting SDUs into smaller PDUs given fragment size 
   --creates PDU fragment, PCI updated with fragment size, data copied into fragment up
   --to fragment size
   procedure Fragment(S : in out SDU; Fragment_Size : Natural; Fragments : out PDU) is 
   begin
      Fragments.PCI.Length := Fragment_Size;
      Fragments.Data := S.Data(1 .. Fragment_Size);
   end Fragment;

   --recontruction of SDU from received PDU
   --copies PCI from PDU to SDU, transfer data from PDU into SDU
   procedure Reassemble(Packets : in PDU; Reassem_SDU : out SDU) is
   begin
      Reassem_SDU.PCI := Packets.PCI;
      Reassem_SDU.Data := Packets.Data;
   end Reassemble;

   --merging two SDUs into one 
   --input two SDUs S1 and S2, combaines data fields and stores merged data into Result
   procedure Concatenate(S1, S2 : in SDU; Result : out SDU) is
   begin
      Result.Data := S1.Data & S2.Data;
   end Concatenate;

   --split SDU into two parts, takes SDU as input, and outputs two smaller SDUs 
   --Half is half of the SDU length 
   --P1 is the first half, P2 is the second half 
   procedure Separation(S : in SDU; P1, P2 : out SDU) is
      Half : Natural := S.PCI.Length / 2;
   begin
      P1.Data := S.Data(1 .. Half);
      P2.Data := S.Data(Half + 1 .. S.PCI.Length);
   end Separation;

   --manages the transmission sequence of PDU
   --incredment sequence number in PCI of PDU
   procedure Control_Transmit(P : in out PDU) is
   begin
      P.PCI.Seq_Num := P.PCI.Seq_Num + 1;
   end Control_Transmit;

   --handling of retransmission of PDU 
   --currently: only printing of message that PDU has been resent 
   procedure Retransmit(P : in PDU) is
   begin
      Put_Line("Retransmitting PDU " & P.PCI.Seq_Num'Image);
   end Retransmit;

   --management of the rate PDUs are sent
   --input PDU received
   --currently: only printing that flow control has been applied to the given PDU
   procedure Flow_Control(P : in PDU) is
   begin
      Put_Line("Flow control has been applied to PDU " & P.PCI.Seq_Num'Image);
   end Flow_Control;





end EFCP;