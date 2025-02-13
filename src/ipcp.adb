with Ada.Calendar;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded; 
with DIF;

package body IPCP is

    -- Global Counter for Unique IPCP IDs
   Next_IPCP_ID : IPCP_ID := 1;  -- Start counting from 1

   -- Protected type for thread safety (ensures atomic access)
   protected type IPCP_Lock is
      function Get_ID return IPCP_ID;
   end IPCP_Lock;

   protected body IPCP_Lock is
      function Get_ID return IPCP_ID is
      begin
         Next_IPCP_ID := Next_IPCP_ID + 1;
         return Next_IPCP_ID - 1;
      end Get_ID;
   end IPCP_Lock;

   IPCP_Lock_I : IPCP_Lock; 

   -- Function to return a unique IPCP ID
   function Get_Next_IPCP_ID return IPCP_ID is
   begin
      return IPCP_Lock_I.Get_ID;
   end Get_Next_IPCP_ID;
   
   -- Initialize an IPCP instance
   -- IP is IPCP Instance
   procedure Initialize_IPCP(IP : in out IPCP_T; Name : String; Address : String; QoS_Params : Priority_Level) is
   begin
      IP.ID := Get_Next_IPCP_ID;
      IP.State := Initialized;
      IP.Name := Name;
      IP.Address := Address;
      IP.QoS_Params := QoS_Params;
      IP.PDU_Buffer.Clear;
   end Initialize_IPCP;

   -- Activate an IPCP
   procedure Activate_IPCP(IP : in out IPCP_T) is
   begin
      IP.State := Active;
      Put_Line("IPCP " & IP.Name & " is now Active.");
   end Activate_IPCP;

   -- Terminate an IPCP
   procedure Terminate_IPCP(IP : in out IPCP_T) is
   begin
      IP.State := Terminated;
      Put_Line("IPCP " & IP.Name & " is now Terminated.");
   end Terminate_IPCP;

   -- Get current state of an IPCP
   function Get_IPCP_State(IP : IPCP_T) return IPCP_State is
   begin
      return IP.State;
   end Get_IPCP_State;
   
   -- Connect IPCP to a DIF
   procedure Connect_To_DIF(IP : in out IPCP_T; Target_DIF : DIF_Access) is
   begin
      IP.Connected_DIF := Target_DIF;
      Put_Line("IPCP " & IP.Name & " connected to DIF " & To_String(Target_DIF.DIF_Name));
   end Connect_To_DIF;

   -- Disconnect IPCP from a DIF
   procedure Disconnect_From_DIF(IP : in out IPCP_T) is
   begin
      if IP.Connected_DIF /= null then
         Put_Line("IPCP " & IP.Name & " disconnected from DIF " & To_String(IP.Connected_DIF.DIF_Name));
         IP.Connected_DIF := null;
      else
         Put_Line("IPCP " & IP.Name & " is not connected to any DIF.");
      end if;
   end Disconnect_From_DIF;

   -- Retrieve the connected DIF
   function Get_Connected_DIF(IP : IPCP_T) return DIF_Access is
   begin
      return IP.Connected_DIF;
   end Get_Connected_DIF;

   -- Encapsulate an SDU into a PDU
   --  procedure Encapsulate_PDU(IP : in out IPCP_T; SDU : String; Src : String; Dest : String) is
   --     PDU_I : PDU_T; -- PDU Instance
   --  begin
   --     PDU_I.ID := PDU_ID'First; 
   --     PDU_I.PDU_T_Field := Data_Transfer;
   --     PDU_I.Src := Src;
   --     PDU_I.Dest := Dest;
   --     PDU_I.PCI := "Header Info";
   --     PDU_I.SDU := SDU;
   --     PDU_I.Timestamp := Ada.Calendar.Clock;
   --     Queue_PDU(IP, PDU_I);
   --  end Encapsulate_PDU;

   --  -- Queue PDU in IPCP Buffer
   --  procedure Queue_PDU(IP : in out IPCP_T; PDU_I : PDU_T) is
   --  begin
   --     IP.PDU_Buffer.Append(PDU_I);
   --     Put_Line("PDU Queued in IPCP: " & IP.Name);
   --  end Queue_PDU;

   --  -- Transmit all stored PDUs
   --  procedure Transmit_PDU(IP : in out IPCP_T) is
   --  begin
   --     if IP.PDU_Buffer.Is_Empty then
   --        Put_Line("No PDUs to transmit.");
   --        return;
   --     end if;

   --     for P of IP.PDU_Buffer loop
   --        Put_Line("Transmitting PDU from " & P.Src & " to " & P.Dest);
   --     end loop;

   --     IP.PDU_Buffer.Clear;
   --  end Transmit_PDU;

   --  -- Process Received PDU
   --  procedure Process_Received_PDU(IP : in out IPCP_T; PDU_I : PDU_T) is
   --  begin
   --     Put_Line("Processing Received PDU at IPCP: " & IP.Name);
   --     Put_Line("Payload: " & PDU_I.SDU);
   --  end Process_Received_PDU;

   --  -- Retry Failed PDU Transmission
   --  procedure Retry_Failed_PDU(IP : in out IPCP_T) is
   --  begin
   --     Put_Line("Retrying failed PDUs...");
   --  end Retry_Failed_PDU;

   --  -- Apply Security & Error Checks on SDU
   --  procedure Apply_SDU_Protection(PDU_I : in out PDU_T) is
   --  begin
   --     PDU_I.PCI := PDU_I.PCI & " | IntegrityCheck=OK";
   --  end Apply_SDU_Protection;


end IPCP;
