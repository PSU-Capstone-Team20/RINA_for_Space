with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Calendar; use Ada.Calendar;
with Ada.Calendar.Formatting; use Ada.Calendar.Formatting;
with Ada.Calendar.Time_Zones; use Ada.Calendar.Time_Zones;

package body IPCP is

   -- Create a new IPCP Instance
   function Create_IPCP(Name : Unbounded_String) return IPCP_Access is
   begin
      return new IPCP_T'(Name => Name, others => <>);
   end Create_IPCP;

   -- Function to create a new PDU instance
   function Create_PDU(ID : String;
                       P_Type : PDU_Type;
                       
                       Dst_EID : String;
                       PCI      : PCI_T;
                       SDU      : String) return PDU_T is
   begin
      return (ID => ID,
              P_Type => P_Type,
              
              Dst_EID => Dst_EID,
              PCI => PCI,
              SDU => SDU,
              Timestamp => Clock);
   end Create_PDU;

   -- Procedure to process an incoming PDU
   procedure Process_PDU(IPCP : in out IPCP_T; PDU : in PDU_T) is
      Time_Image : constant String := Image(PDU.Timestamp, Include_Time_Fraction => False, Time_Zone => 0);
   begin
      Put_Line("Processing PDU ID: " & PDU.ID);
      Put_Line("PDU Type: " & PDU_Type'Image(PDU.P_Type));
      
      Put_Line("Destination Address: " & PDU.Dst_EID);

      Put_Line("Timestamp: " & Time_Image);

      Put_Line("Sequence Number: " & Natural'Image(PDU.PCI.Seq_Num));
      Put_Line("DRF Flag: " & Boolean'Image(PDU.PCI.DRF_Flag));
      Put_Line("ECN Flag: " & Boolean'Image(PDU.PCI.ECN_Flag));
      Put_Line("QoS ID: " & Natural'Image(PDU.PCI.QoS_ID));

      -- Store the PDU in the buffer
      IPCP.PDUs.Append(PDU);
   end Process_PDU;



end IPCP;
