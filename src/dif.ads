with IPCP; use IPCP;
with application; use application;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package dif is
   
   type IPCPArray is array (Natural range <>) of IPCP;
   type ApplicationArray is array (Natural range <>) of application;
   type DIFArray is array (Natural range <>) of dif;
   
   type dif is tagged record
      difID : Integer;
      memberIPCPs : IPCPArray;
      applications : ApplicationArray;
      -- TODO: Implement policies when all branches are merged
      -- policies :
      accessibleDIFS : DIFArray;
   end record;

   -- Adds an IPCP to the DIF 
   -- This will be called by the IPCP when it wants to join
   function addIPCP(Self : in out dif; ipcp : in out IPCP) return Boolean;

   -- Removes IPCP from DIF
   function removeIPCP(Self : in out dif; ipcpID : Natural) return Boolean;

   -- Discover connectable DIFs
   function discoverDIFs(Self : in out dif) return Boolean;

   -- Lists all member IPCPs
   function listMemberIPCPs(self : in dif) return Boolean;

end dif;