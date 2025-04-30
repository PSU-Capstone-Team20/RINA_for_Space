with Ada.Text_IO; use Ada.Text_IO;
with Ada.Assertions; use Ada.Assertions;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Containers; use Ada.Containers;
with RIB; use RIB;
with RINA; use RINA;
with EFCP; use EFCP;
with Rina_BP_Bundle; use Rina_BP_Bundle;
with Transport_Types; use Transport_Types;
with Test_Utils;  use Test_Utils;

procedure Test_Transmission_Recovery is
   pragma Assertion_Policy(Check);

   Src_Addr, Dst_Addr : Address_Vectors.Vector;
   Path : Path_Vectors.Vector;
   Src_EID, Dst_EID : PDU_T;
   B : Bundle;
   Payload : constant String := "Hello, Mars!";
   E : Address_Element;

   -- RIB identifiers
   ISP_DIF     : Unbounded_String := +"ISP DIF";
   MGVD_DIF    : Unbounded_String := +"Mars Ground Vehicle DIF";
   Bridge_DIF  : Unbounded_String := +"Relay DIF";

   ISP_Server  : Unbounded_String := +"ISP Server";
   Mars_Rover  : Unbounded_String := +"Perseverance Rover";
   Relay_Node  : Unbounded_String := +"RelayNode";

   Relay_IPCP  : Unbounded_String := +"RelayLink";
   Telemetry   : Unbounded_String := +"Telemetry App";

   Obj : RIB_Obj;
begin
   Put_Line("Starting Data Transmission Test with Recovery...");

   -- RIB Setup
   RIB.Add_Entry(ISP_DIF);
   RIB.Add_Entry(MGVD_DIF);
   RIB.Add_Entry(Bridge_DIF);

   RIB.Add_Comp(ISP_DIF, ISP_Server);
   RIB.Add_Comp(MGVD_DIF, Mars_Rover);
   RIB.Add_Comp(Bridge_DIF, Relay_Node);
   RIB.Add_Comp(ISP_DIF, Relay_Node);
   RIB.Add_Comp(MGVD_DIF, Relay_Node);

   RIB.Add_IPCP(ISP_DIF, Relay_Node, Relay_IPCP);
   RIB.Add_IPCP(MGVD_DIF, Relay_Node, Relay_IPCP);
   RIB.Add_IPCP(Bridge_DIF, Relay_Node, Relay_IPCP);

   RIB.Add_APN(ISP_DIF, ISP_Server, Telemetry);
   RIB.Add_APN(MGVD_DIF, Mars_Rover, Telemetry);

   -- Populate neighbor info
   declare
      O : RIB_Obj;
   begin
      O := RIB.Get_Entry(ISP_DIF).Obj_Type(ISP_Server);
      O.Obj_Obj_Type.Accessible_IPCPs.Append(Relay_IPCP);
      O.Obj_Obj_Type.Active_APNs.Append(Telemetry);
      RIB.Update_Comp(ISP_DIF, ISP_Server, O);

      O := RIB.Get_Entry(MGVD_DIF).Obj_Type(Mars_Rover);
      O.Obj_Obj_Type.Accessible_IPCPs.Append(Relay_IPCP);
      O.Obj_Obj_Type.Active_APNs.Append(Telemetry);
      RIB.Update_Comp(MGVD_DIF, Mars_Rover, O);

      O := RIB.Get_Entry(ISP_DIF).Obj_Type(Relay_Node);
      O.Obj_Obj_Type.Connected_DIFs.Append(Bridge_DIF);
      O.Obj_Obj_Type.Accessible_IPCPs.Append(Relay_IPCP);
      RIB.Update_Comp(ISP_DIF, Relay_Node, O);

      O := RIB.Get_Entry(MGVD_DIF).Obj_Type(Relay_Node);
      O.Obj_Obj_Type.Connected_DIFs.Append(Bridge_DIF);
      O.Obj_Obj_Type.Accessible_IPCPs.Append(Relay_IPCP);
      RIB.Update_Comp(MGVD_DIF, Relay_Node, O);

      O := RIB.Get_Entry(Bridge_DIF).Obj_Type(Relay_Node);
      O.Obj_Obj_Type.Connected_DIFs.Append(ISP_DIF);
      O.Obj_Obj_Type.Connected_DIFs.Append(MGVD_DIF);
      O.Obj_Obj_Type.Accessible_IPCPs.Append(Relay_IPCP);
      RIB.Update_Comp(Bridge_DIF, Relay_Node, O);
   end;

   -- Source address
   E := (Name => ISP_DIF, Address_Type => +"DIF"); Src_Addr.Append(E);
   E := (Name => ISP_Server, Address_Type => +"Computer"); Src_Addr.Append(E);
   E := (Name => Telemetry, Address_Type => +"APN"); Src_Addr.Append(E);

   -- Destination address
   E := (Name => MGVD_DIF, Address_Type => +"DIF"); Dst_Addr.Append(E);
   E := (Name => Mars_Rover, Address_Type => +"Computer"); Dst_Addr.Append(E);
   E := (Name => Telemetry, Address_Type => +"APN"); Dst_Addr.Append(E);

   -- Initial path calculation
   Path := D_Star_Lite(Src_Addr, Dst_Addr);
   Assert(Path.Length > 0, "Initial path calculation failed.");
   Put_Line("Initial flow established through" & Path.Length'Image & " IPCP transitions across DIFs.");

   Src_EID.PCI.Src_Address := Src_Addr;
   Dst_EID.PCI.Dst_Address := Dst_Addr;

   B := Create_Bundle(
      Version         => 7,
      Processing_Flag => 0,
      Block_Length    => Payload'Length,
      Src_EID         => Src_EID,
      Dst_EID         => Dst_EID,
      Payload         => Payload,
      Path            => Path
   );

   Send_Bundle(B);
   Put_Line("Bundle sent via initial path.");

   -- Simulate failure
   Put_Line("Simulating Path Disruption...");
   RIB.Delete_IPCP_By_Name(Bridge_DIF, Relay_Node, Relay_IPCP);
   RIB.Delete_Comp(Bridge_DIF, Relay_Node);
   Put_Line("RelayNode removed from Relay DIF.");

   -- Attempt reroute
   Put_Line("Recalculating Path...");
   Path := D_Star_Lite(Src_Addr, Dst_Addr);
   Assert(Path.Length > 0, "Rerouted path calculation failed.");
   Put_Line("Recovery path recalculated with" & Path.Length'Image & " IPCP transitions after DIF disruption.");

   B := Create_Bundle(
      Version         => 7,
      Processing_Flag => 0,
      Block_Length    => Payload'Length,
      Src_EID         => Src_EID,
      Dst_EID         => Dst_EID,
      Payload         => Payload & " (Resent after outage)",
      Path            => Path
   );

   Send_Bundle(B);
   Put_Line("Bundle resent via rerouted path.");
end Test_Transmission_Recovery;
