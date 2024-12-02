with Rina;
with dif;
with ipcp;
with Ada.Text_IO; use Ada.Text_IO;

procedure Rina_For_Space is
   test1 : dif.DIF_Vector;
   num : Integer;
begin
   dif.createDIF(1, test1);
   num := dif.getID(test1.First_Element);
   Put_Line(num'Image);
   dif.deleteDIF(1, test1);
   if test1.Is_Empty then
      Put_Line("Successful Deletion");
   end if;
end Rina_For_Space;
