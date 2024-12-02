with Rina;
with dif;
with ipcp;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

procedure Rina_For_Space is
   test1 : dif.DIF_Vector;
   num : Integer;
begin
   dif.createDIF(1, test1);
   num := dif.getID(test1(0));
   dif.createIPCP(To_Unbounded_String("Test"), test1(0));
   dif.listIPCP(test1(0));
   dif.deleteIPCP(To_Unbounded_String("Test"), test1(0));
   if test1(0).MemberIPCPs.Is_Empty then
      Put_Line("Successful deletion of IPCP");
   end if;
end Rina_For_Space;
