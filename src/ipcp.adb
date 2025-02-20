with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package body IPCP is

   function Create_IPCP(Name : Unbounded_String; ID : Unbounded_String) return IPCP_Access is
      New_IPCP : constant IPCP_Access := new IPCP_T'(ID, Initialized, Name, To_Unbounded_String(""), Medium, null, PDU_Buffer'(PDU_Vector.Empty));
   begin
      return New_IPCP;
   end Create_IPCP;

   procedure Process_PDU(IPCP : in out IPCP_T; PDU : in Unbounded_String) is
   begin
      IPCP.PDUs.Append(PDU);
   end Process_PDU;

end IPCP;
