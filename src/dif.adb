with Ada.Text_IO; use Ada.Text_IO;

package body dif is
   function addIPCP(Self : in out dif; ipcp : in out IPCP) return Boolean is

   index : Natural;
   flag : Boolean;

   begin   
      index := 0;
      flag := false;

      -- Find first empty spot in the array
      while flag loop
         if dif.memberIPCPs(index) = null then
            flag := true;
         elsif index = Natural'Last then
            return false;
         else
            index := index + 1;
         end if;
      end loop;

      -- Assigns IPCP id to open index value
      ipcp.setID(index);

      -- Set the first empty spot to the IPCP value
      dif.memberIPCPs(index) := ipcp;

      return true;
   end addIPCP;

   function removeIPCP(Self : in out dif; ipcpID : Natural) return Boolean is

   index : Natural;
   flag : Boolean;

   begin   
      index := 0;
      flag := false;

      -- Find element with matching ID
      while flag loop
         if dif.memberIPCPs(index) /= null and then dif.memberIPCPs(index).getID = ipcpID then
               flag := true;
         elsif index = Natural'Last then
            return false;
         else
            index := index + 1;
         end if;
      end loop;

      -- Clears IPCP from found index value
      dif.memberIPCPs(index) := null;

      return true;
   end removeIPCP;

   -- Discovers accescible DIFs
   -- TODO: Figure out and implement logic
   function discoverDIFs(Self : in out dif) return Boolean is
   begin
      null;
      return true;
   end discoverDIFs;

   function listMemberIPCPs(self : in dif) return Boolean is
   
   index : Natural;

   begin
      index := 0;

      for index in Natural'First .. Natural'Last loop
         if dif.memberIPCPs(index) /= null then
            Put_Line(dif.memberIPCPs(index).getName);       
         end if;
      end loop;

      return true;
   end listMemberIPCPs;

end dif;