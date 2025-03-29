with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
package rina is
   
   -- address is an array of 3 unbounded strings as follows
   -- index 0 : DIF
   -- index 1 : Computer
   -- index 2 : APN/IPCP
   type address is array (0 .. 2) of Unbounded_String;

end rina;