package body dif is

   -- creates a DIF with specified ID and adds it to the vector
   function createDIF return DIF_Access is
      New_DIF : constant DIF_Access := new DIF;
      begin

      -- TODO Add IPCP_Manager Logic Here

      New_DIF.DIF_ID := 0;
      New_DIF.AccessibleDIFs.Clear;
      New_DIF.AccessibleDIFs.Set_Length(0);
      New_DIF.Applications.Clear;
      New_DIF.Applications.Set_Length(0);

      -- New_DIF.MemberIPCPs.Clear;
      -- New_DIF.MemberIPCPs.Set_Length(0);

      return New_DIF;
   end createDIF;

   function createNamedDIF(name : Unbounded_String) return DIF_Access is
      New_DIF : constant DIF_Access := new DIF;
      begin

      -- TODO Add IPCP_Manager Logic Here

      New_DIF.DIF_ID := 0;
      New_DIF.DIF_Name := name;
      New_DIF.AccessibleDIFs.Clear;
      New_DIF.AccessibleDIFs.Set_Length(0);
      New_DIF.Applications.Clear;
      New_DIF.Applications.Set_Length(0);
      
      -- New_DIF.MemberIPCPs.Clear;
      -- New_DIF.MemberIPCPs.Set_Length(0);
      
      return New_DIF;
   end createNamedDIF;

   -- returns the ID of the inputted DIF in a vector
   function getID(self : DIF_Access) return Integer is
      begin
         return self.DIF_ID;
   end getID;

   function getName(self : DIF_Access) return Unbounded_String is
      begin
         return self.DIF_Name;
   end getName;

   -- deletes the DIF that has the specified ID if possible
   procedure disconnectDIF(ID : Integer; vector : in out DIF_Vector) is
      begin
         for I in vector.First_Index .. vector.Last_Index loop
            if (getID(vector(I)) = ID) then
               vector.Delete(I);
               exit;
            end if;
         end loop;
   end disconnectDIF;

   -- adds a pair of DIFs to each other's accessible DIFs
   procedure pairDIF(first : in out DIF_Access; second : in out DIF_Access) is
      begin
      first.AccessibleDIFs.Append(second);
      second.AccessibleDIFs.Append(first);
   end pairDIF;

   -- lists the IDs of accessible DIFs
   procedure listAccessibleDIF(self : DIF_Access) is
      begin
      for i in self.AccessibleDIFs.First_Index .. self.AccessibleDIFs.Last_Index loop
         Put(getID(self.AccessibleDIFs(i))'Image);
         Put_Line(" " & Unbounded_String'Image(getName(self.AccessibleDIFs(i))));
      end loop;
   end listAccessibleDIF;
   
   -- lists all member IPCPs of a provided DIF
   --  procedure listIPCP(self : DIF_Access) is
   --     begin
   --     for I in self.MemberIPCPs.First_Index .. self.MemberIPCPs.Last_Index loop
   --        Put_Line(self.MemberIPCPs(I).ipcpName'Image);
   --     end loop;
   --  end listIPCP;
   
   -- deletes IPCP with given name from given DIF
   --  procedure disconnectIPCP(name : Unbounded_String; self : in out DIF_Access) is
   --     begin
   --     for I in self.MemberIPCPs.First_Index .. self.MemberIPCPs.Last_Index loop
   --        if self.MemberIPCPs(I).ipcpName = name then
   --           self.MemberIPCPs.Delete(I);
   --           exit;
   --        end if;
   --     end loop;
   --  end disconnectIPCP;

end dif;