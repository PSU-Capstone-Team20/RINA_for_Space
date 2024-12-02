with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package ipcp is
   
   type ipcp is tagged record
      ipcpID : Natural;
      ipcpName : Unbounded_String;
      state : Unbounded_String;
   end record;

   
end ipcp;