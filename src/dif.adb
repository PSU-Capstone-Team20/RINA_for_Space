package body dif is
   procedure createDIF(ID : Integer; vector : in out DIF_Vector) is
      New_DIF : constant DIF_Access := new DIF;
      begin
      New_DIF.DIF_ID := ID;
      New_DIF.AccessibleDIFs.Clear;
      New_DIF.AccessibleDIFs.Set_Length(0);
      New_DIF.Applications.Clear;
      New_DIF.Applications.Set_Length(0);
      New_DIF.MemberIPCPs.Clear;
      New_DIF.MemberIPCPs.Set_Length(0);
      vector.append(New_DIF);
   end createDIF;

   function getID(self : DIF_Access) return Integer is
      begin
         return self.DIF_ID;
   end getID;

   procedure deleteDIF(ID : Integer; vector : in out DIF_Vector) is
      begin
         for I in vector.First_Index .. vector.Last_Index loop
            if (getID(vector(I)) = ID) then
               vector.Delete(I);
            end if;
         end loop;
   end deleteDIF;
end dif;