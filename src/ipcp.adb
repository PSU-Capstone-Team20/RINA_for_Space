with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Calendar; use Ada.Calendar;
with Ada.Calendar.Formatting; use Ada.Calendar.Formatting;

package body IPCP is

   function Create_IPCP(Name : Unbounded_String; ID : Unbounded_String) return IPCP_Access is
   begin
      return new IPCP_T'(ID => ID, Name => Name, others => <>);
   end Create_IPCP;

   -- Function to create a new PDU instance
   function Create_PDU(ID : String;
                       PDU_Type : PDU_Type;
                       Src_Addr : String;
                       Dst_Addr : String;
                       PCI      : PCI_T;
                       SDU      : String) return PDU_T is
   begin
      return (ID => ID,
              PDU_Type => PDU_Type,
              Src_Addr => Src_Addr,
              Dst_Addr => Dst_Addr,
              PCI => PCI,
              SDU => SDU,
              Timestamp => Clock);
   end Create_PDU;

   -- Procedure to process a PDU
   procedure Process_PDU(IPCP : in out IPCP_T; PDU : in PDU_T) is
      Time_Diff : Duration;
   begin
      -- Extracting Information
      Put_Line("Processing PDU ID: " & PDU.ID);
      Put_Line("PDU Type: " & PDU_Type'Image(PDU.PDU_Type));
      Put_Line("Source Address: " & PDU.Src_Addr);
      Put_Line("Destination Address: " & PDU.Dst_Addr);

      -- Calculate the elapsed time
      Time_Diff := To_Duration(Clock - PDU.Timestamp);
      Put_Line("Timestamp: " & Duration'Image(Time_Diff) & " seconds ago");

      -- Processing the PCI
      Put_Line("Sequence Number: " & Natural'Image(PDU.PCI.Seq_Num));
      Put_Line("DRF Flag: " & Boolean'Image(PDU.PCI.DRF_Flag));
      Put_Line("ECN Flag: " & Boolean'Image(PDU.PCI.ECN_Flag));
      Put_Line("QoS ID: " & Natural'Image(PDU.PCI.QoS_ID));

      -- Store the PDU in the buffer
      IPCP.PDUs.Append(PDU);
   end Process_PDU;

end IPCP;
