package body IPCP is
   
   -- Initialize an IPCP
   procedure Initialize_IPCP(IPCP_Instance : in out IPCP_Record; Name : String; Address : String; QoS_Params : String) is
   begin
      IPCP_Instance.ID := Integer'First;
      IPCP_Instance.State := Initialized;
      IPCP_Instance.Name := Name;
      IPCP_Instance.Address := Address;
      IPCP_Instance.QoS_Params := QoS_Params;
   end Initialize_IPCP;

   -- Activate an IPCP
   procedure Activate_IPCP(IPCP_Instance : in out IPCP_Record) is
   begin
      IPCP_Instance.State := Active;
   end Activate_IPCP;

   -- Terminate an IPCP
   procedure Terminate_IPCP(IPCP_Instance : in out IPCP_Record) is
   begin
      IPCP_Instance.State := Terminated;
   end Terminate_IPCP;

   -- Get current state of an IPCP
   function Get_IPCP_State(IPCP_Instance : IPCP_record) return IPCP_State is
   begin
      return IPCP_Instance.State;
   end Get_IPCP_State;
   
   -- Connect IPCP to a DIF
   procedure Connect_To_DIF(IPCP_Instance : in out IPCP_Record; Target_DIF : DIF_Access) is
   begin
      IPCP_Instance.Connected_DIF := Target_DIF;
      Put_Line("IPCP " & IPCP_Instance.Name & " connected to DIF " & To_String(Target_DIF.DIF_Name));
   end Connect_To_DIF;

   -- Disconnect IPCP from a DIF
   procedure Disconnect_From_DIF(IPCP_Instance : in out IPCP_Record) is
   begin
      if IPCP_Instance.Connected_DIF /= null then
         Put_Line("IPCP " & IPCP_Instance.Name & " disconnected from DIF " & To_String(IPCP_Instance.Connected_DIF.DIF_Name));
         IPCP_Instance.Connected_DIF := null;
      else
         Put_Line("IPCP " & IPCP_Instance.Name & " is not connected to any DIF.");
      end if;
   end Disconnect_From_DIF;

   -- Retrieve the connected DIF
   function Get_Connected_DIF(IPCP_Instance : IPCP_Record) return DIF_Access is
   begin
      return IPCP_Instance.Connected_DIF;
   end Get_Connected_DIF;

   -- Retry Failed Communication
   procedure Retry_Failed_Communication(IPCP_Instance : in out IPCP_Record; Max_Retries : Integer := 10) is
      Retry_Count : Integer := 0;
      Success : Boolean := False;
   begin
      while Retry_Count < Max_Retries and then not Success loop
         Put_Line("Attempt " & Integer'Image(Retry_Count + 1) & ": Retrying communication from IPCP " & IPCP_Instance.Name);
         -- TO-DO: Add actual retry logic
         -- Simulate success after 3 attempts for demonstration
         if Retry_Count = 2 then
            Success := True;
            Put_Line("Communication successful on attempt " & Integer'Image(Retry_Count + 1));
         end if;
         Retry_Count := Retry_Count + 1;
      end loop;

      if not Success then
         Put_Line("Max retries reached for IPCP " & IPCP_Instance.Name);
      end if;
   end Retry_Failed_Communication;

   -- Store Data Temporarily
   -- Storing data in DIF's data buffer/locally (no DIF connected) temporarily during network failures or transmission delays
   procedure Store_Data_Temporarily(IPCP_Instance : in out IPCP_Record; Data : String) is
   begin
      if IPCP_Instance.Connected_DIF /= null then
         if IPCP_Instance.Connected_DIF.Data_Buffer = null then
            IPCP_Instance.Connected_DIF.Data_Buffer := new Data_Buffer.Vector;
         end if;
         IPCP_Instance.Connected_DIF.Data_Buffer.Append(Data);
         Put_Line("Data stored temporarily for IPCP " & IPCP_Instance.Name);
      else
         Put_Line("Error: IPCP " & IPCP_Instance.Name & " is not connected to any DIF. Storing data locally.");
      end if;
   end Store_Data_Temporarily;

   -- Transmit Stored Data
   -- Sends buffered data if connected to a DIF, clears the buffer after, or logs an error if disconnected or buffer is empty
   procedure Transmit_Stored_Data(IPCP_Instance : in out IPCP_Record) is
   begin
      if IPCP_Instance.Connected_DIF /= null and then IPCP_Instance.Connected_DIF.Data_Buffer /= null then
         for I in IPCP_Instance.Connected_DIF.Data_Buffer'Range loop
            -- TO-DO: Add actual transmission logic
            Put_Line("Transmitting stored data: " & IPCP_Instance.Connected_DIF.Data_Buffer(I));
         end loop;
         IPCP_Instance.Connected_DIF.Data_Buffer.Clear;
         Put_Line("All stored data transmitted for IPCP " & IPCP_Instance.Name);
      else
         Put_Line("Error: No stored data to transmit or IPCP " & IPCP_Instance.Name & " is not connected to any DIF.");
      end if;
   end Transmit_Stored_Data;
end IPCP;
