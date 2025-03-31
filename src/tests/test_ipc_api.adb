with Ada.Assertions;      use Ada.Assertions;
with Ada.Exceptions;      use Ada.Exceptions;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO;         use Ada.Text_IO;
with Test_Utils;          use Test_Utils;
with IPC_API;           use IPC_API;
with IPC_Manager;       use IPC_Manager;
with IPCP_Types;      use IPCP_Types;

procedure Test_IPC_API is
   pragma Assertion_Policy (Check);

   Mgr : IPCP_Manager_T;
   IPCP_Name : constant Unbounded_String := To_Unbounded_String("Mock_IPCP");
   Flow : IPCP_Types.Flow_Info_T;
   Msg_Out : constant Unbounded_String := To_Unbounded_String("Hello Rina!");
   Msg_In : Unbounded_String;

begin
   Put_Line("Creating IPCP: " & To_String(IPCP_Name));
   IPC_Manager.Create_IPCP (IPCP_Name, Mgr);
   Assert(IPC_Manager.Find_IPCP(Mgr, IPCP_Name) /= null, "IPCP not created");
   Put_Line("Created IPCP '" & To_String(IPCP_Name) & "' successfully.");

   Put_Line("Allocating flow for IPCP '" & To_String(IPCP_Name) & "' with QoS=1 and Remote_CEP_ID='SRC_CEP001'");
   IPC_API.Allocate_Flow (Mgr, IPCP_Name, To_Unbounded_String("SRC_CEP001"), 1, Flow);
   Assert(Flow.Flow_ID > 0, "Flow ID mismatch");
   Put_Line("Allocated flow: ID=" & Flow.Flow_ID'Image &
            ", Port=" & Flow.Port_ID'Image &
            ", QoS=" & Flow.QoS_ID'Image &
            ", Remote_CEP_ID=" & To_String(Flow.Remote_CEP_ID));


   Put_Line("Sending message: " & To_String(Msg_Out));
   IPC_API.Send (Mgr, IPCP_Name, Flow, Msg_Out);
   Put_Line("Message sent.");

   declare
      IPCP : IPCP_Access := IPC_Manager.Find_IPCP(Mgr, IPCP_Name);
   begin
      IPCP.Incoming_PDUs.Append(IPCP.Outgoing_PDUs.First_Element);
      IPCP.Outgoing_PDUs.Clear;
      Put_Line("Simulated loopback: moved outgoing PDU to incoming buffer.");
   end;

   Put_Line("Receiving message...");
   IPC_API.Receive (Mgr, IPCP_Name, Flow, Msg_In);
   Assert(Msg_In = Msg_Out, "Message mismatch");
   Put_Line("Received message: " & To_String(Msg_In));

   Put_Line("Deallocating flow ID=" & Flow.Flow_ID'Image);
   IPC_API.Deallocate_Flow (Mgr, IPCP_Name, Flow);
   Put_Line("Flow deallocated successfully.");

   Put_Line("All tests completed");
      
end Test_IPC_API;