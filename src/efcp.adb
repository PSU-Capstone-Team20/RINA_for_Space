with Ada.Text_IO; use Ada.Text_IO;
with Transport_Types; use Transport_Types;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;


--
--package for Error and Flow Control Protocol(EFCP) split into two protocol machines DTP and DTCP
--DTP : Data Transfer Protocol - fragmentation, reassembly, sequencing, concatenation, and separation
--DTCP : Data Transfer Control Protocol - mechanisms that are loosely coupled to transported SDU
--DTCP cont. - transmission control, retransmission control, and flow control
--
package body EFCP is
   package Int_IO is new Ada.Text_IO.Integer_IO(Integer);

   --splitting SDUs into smaller PDUs given fragment size 
   --creates PDU fragment, PCI updated with fragment size, data copied into fragment up
   --to fragment size
   procedure Fragment(S : in out SDU_S_T; Fragment_Size : String; Fragments : out PDU_S_T) is
      Origin_Data : constant String := To_String(S.Data);
      Remains_Length : constant Natural := Origin_Data'Length;
      Size : Natural;
   begin
      declare 
         Parsed_Int : Integer;
         Last : Integer;
      begin
         Int_IO.Get(Fragment_Size, Parsed_Int, Last);
         if Parsed_int < 0 then
            raise Constraint_Error with "Fragment size cannot be negative";
         end if;
         Size := Natural'Min(Natural(Parsed_Int), Remains_Length);
      end;         
      Fragments.PCI := S.PCI;
      Fragments.PCI.Seq_Num := S.PCI.Seq_Num;
      Fragments.PCI.DRF_Flag := Size < Remains_Length;

      Fragments.Data := To_Unbounded_String(Origin_Data(1 .. Size));
      

      if Size + 1  <= Remains_Length then
        
         S.Data := To_Unbounded_String(Origin_Data(Size + 1 .. Remains_Length));
      else 
         
         S.Data := To_Unbounded_String(" "); 
      end if;
   

   end Fragment;

   --recontruction of SDU from received PDU
   --copies PCI from PDU to SDU, transfer data from PDU into SDU
   procedure Reassemble(Packets : in PDU_S_T; Reassem_SDU : out SDU_S_T) is
   begin
      Reassem_SDU.PCI := Packets.PCI;
      Reassem_SDU.Data := Packets.Data;
   end Reassemble;

   --merging two SDUs into one 
   --input two SDUs S1 and S2, combaines data fields and stores merged data into Result
   procedure Concatenate(S1, S2 : in SDU_S_T; Result : out SDU_S_T) is
   begin
      Result.Data := S1.Data & S2.Data;
   end Concatenate;

   --split SDU into two parts, takes SDU as input, and outputs two smaller SDUs 
   --Half is half of the SDU length 
   --P1 is the first half, P2 is the second half 
   procedure Separation(S : in SDU_S_T; P1, P2 : out SDU_S_T) is
      Full_Data : String := To_String(S.Data);
      Half : Natural := Full_Data'Length /2;
   begin
      P1.Data := To_Unbounded_String(Full_Data(1 .. Half));
      P2.Data := To_Unbounded_String(Full_Data(Half + 1 .. Full_Data'Length));
   end Separation;

   --manages the transmission sequence of PDU
   --incredment sequence number in PCI of PDU
   procedure Control_Transmit(P : in out PDU_S_T) is
   begin
      P.PCI.Seq_Num := P.PCI.Seq_Num + 1;
   end Control_Transmit;

   --handling of retransmission of PDU 
   --currently: only printing of message that PDU has been resent 
   procedure Retransmit(P : in PDU_S_T) is
   begin
      Put_Line("Retransmitting PDU " & P.PCI.Seq_Num'Image);
   end Retransmit;

   --management of the rate PDUs are sent
   --input PDU received
   --currently: only printing that flow control has been applied to the given PDU
   procedure Flow_Control(P : in PDU_S_T) is
   begin
      Put_Line("Flow control has been applied to PDU " & P.PCI.Seq_Num'Image);
   end Flow_Control;





end EFCP;