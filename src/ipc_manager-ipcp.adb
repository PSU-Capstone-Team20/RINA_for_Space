with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Calendar; use Ada.Calendar;
with Ada.Calendar.Formatting; use Ada.Calendar.Formatting;
with Ada.Calendar.Time_Zones; use Ada.Calendar.Time_Zones;

package body IPC_Manager.IPCP is

   -- Make_IPCP returns an initialize IPCP_T record with the given name
   function Make_IPCP(Name : Unbounded_String) return IPCP_Access is
   begin
      Log.Info("Creating IPCP with name: " & To_String(Name));
      return new IPCP_T'(Name => Name, others => <>);
   end Make_IPCP;

   -- Function to create a new PDU instance
   function Create_PDU(ID        : String;
                       P_Type    : PDU_Type;
                       Dst_EID   : String;
                       PCI       : PCI_T;
                       SDU       : String) return PDU_T is
   begin
      return (ID        => ID,
              P_Type    => P_Type,
              Dst_EID   => Dst_EID,
              PCI       => PCI,
              SDU       => SDU,
              Timestamp => Clock);
   end Create_PDU;

   -- Procedure to process an incoming PDU
   procedure Process_PDU(IPCP : in out IPCP_T; PDU : in PDU_T) is
      Max_Queue_Size : constant := 1000;
      Time_Image : constant String :=
        Ada.Calendar.Formatting.Image(PDU.Timestamp,
                                       Include_Time_Fraction => False,
                                       Time_Zone => 0);
   begin
      Log.Debug("Processing PDU: " & PDU.ID);
      Log.Debug("Destination: " & PDU.Dst_EID);
      Log.Debug("Timestamp: " & Time_Image);
      Log.Debug("QoS ID: " & Natural'Image(PDU.PCI.QoS_ID));

      -- Overflow protection
      if IPCP.PDUs.Length >= Max_Queue_Size then
      Log.Warning("PDU dropped: queue full for IPCP " & To_String(IPCP.Name));
      return;
      end if;

      IPCP.PDUs.Append(PDU);
      Log.Info("PDU appended to IPCP: " & To_String(IPCP.Name));
   end Process_PDU;

   -- Assign a PDU to an IPCP
   procedure Assign_PDU(IPCP_Instance : IPCP_Access; PDU : PDU_T) is
   begin
      Put_Line("Assigning PDU ID: " & PDU.ID & " to IPCP: " & To_String(IPCP_Instance.Name));
      IPCP_Instance.PDUs.Append(PDU);
   end Assign_PDU;

   function Peek_PDU(IPCP : IPCP_T) return PDU_T is
   begin
      if IPCP.PDUs.Is_Empty then
         Log.Warning("Peek_PDU: No PDUs available in IPCP " & To_String(IPCP.Name));
         raise Constraint_Error with "PDU buffer is empty.";
      end if;
      return IPCP.PDUs.First_Element;

   end Peek_PDU;

   procedure Clear_PDU_Queue(IPCP : in out IPCP_T) is
   begin
      IPCP.PDUs.Clear;
      Log.Info("Cleared PDU queue for IPCP: " & To_String(IPCP.Name));
   end Clear_PDU_Queue;


end IPC_Manager.IPCP;
