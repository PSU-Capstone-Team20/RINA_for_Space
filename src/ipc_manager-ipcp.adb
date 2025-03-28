with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

-- IPCP (Inter-Process Communication Protocol) package
-- This package is the handler and local controller of an individual IPCP_T instance
-- It implements procedures that manage its internal state and buffer operations.
package body IPC_Manager.IPCP is

   -- Make_IPCP create a single IPCP instance and returns an initialize IPCP_T record 
   function Make_IPCP(Name : Unbounded_String) return IPCP_T is
      New_IPCP : IPCP_T;
   begin
      New_IPCP.Name               := Name;
      --New_IPCP.Address            := Address;
      --New_IPCP.Connected_Computer := Connected_Computer;
      New_IPCP.State              := Initialized;

      New_IPCP.Outgoing_PDUs      := PDU_Vector.Empty_Vector;
      New_IPCP.Incoming_PDUs      := PDU_Vector.Empty_Vector;
      New_IPCP.Active_Flows       := Flow_Vector.Empty_Vector;

      return New_IPCP;
   end Make_IPCP;

   procedure Assign_PDU(IPCP_Instance : in out IPCP_T; PDU : PDU_T; To_Outgoing : Boolean := True) is
   begin
      if To_Outgoing then
         IPCP_Instance.Outgoing_PDUs.Append(PDU);
      else
         IPCP_Instance.Incoming_PDUs.Append(PDU);
      end if;
   end Assign_PDU;

   function Get_PDU(IPCP : in out IPCP_T; From_Outgoing : Boolean := True) return PDU_T is
   begin
      if From_Outgoing then
         return IPCP.Outgoing_PDUs.First_Element;
      else
         return IPCP.Incoming_PDUs.First_Element;
      end if;
   end Get_PDU;

   function Pop_PDU(IPCP : in out IPCP_T; From_Outgoing : Boolean := True) return PDU_T is
      PDU : PDU_T;
   begin
      if From_Outgoing then
         PDU := IPCP.Outgoing_PDUs.First_Element;
         IPCP.Outgoing_PDUs.Delete(1);
      else
         PDU := IPCP.Incoming_PDUs.First_Element;
         IPCP.Incoming_PDUs.Delete(1);
      end if;
      return PDU;
   end Pop_PDU;

   procedure Clear_PDU_Buffer(IPCP : in out IPCP_T; Clear_Incoming : Boolean := True; Clear_Outgoing : Boolean := True) is
   begin
      if Clear_Incoming then
         IPCP.Incoming_PDUs.Clear;
      end if;

      if Clear_Outgoing then
         IPCP.Outgoing_PDUs.Clear;
      end if;
   end Clear_PDU_Buffer;


end IPC_Manager.IPCP;
