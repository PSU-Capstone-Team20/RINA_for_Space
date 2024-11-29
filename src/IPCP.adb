package body ipcp is

   -- Creates IPCP and assigns ID to user specified value
   procedure setName (Self : in out IPCP; name : String) is
   begin
      IPCP.ipcpName := name;
   end setName;

   -- Returns the name of the IPCP
   function getName (Self : in IPCP) is
   begin
      return IPCP.ipcpName;
   end getName;

   -- Joins a DIF
   function JoinDIF (Self : in out IPCP; dif : in DIF) is
   begin
      IPCP.associatedDIF := dif;

      -- TODO: Call the DIF add IPCP function
      return true;
   end JoinDIF;

   -- Leaves a DIF
   function LeaveDIF (Self : in out IPCP) is
   begin
      IPCP.associatedDIF := null;

      -- TODO: Call the DIF remove IPCP function
      return true;
   end LeaveDIF;

   -- Finds available DIFs
   -- TODO: Implement search here.
   function DiscoverDIF (Self : in out IPCP) is
   begin
      null;
      return true;
   end DiscoverDIF;

   -- Gets the current state of IPCP
   function getState (Self : in IPCP) is
   begin
      return IPCP.state;
   end getState;

   -- Sets the state of IPCP and returns new value
   function setState (Self : in out IPCP; state : String) is
   begin
      IPCP.state := state;
      return IPCP.state = state;
   end setState;

   -- Gets the current ID of IPCP
   function getID (Self : in IPCP) is
   begin
      return IPCP.ipcpID;
   end getID;

   -- Sets the ID of the IPCP
   function setID (Self : in out IPCP; id : Natural) is
   begin
      IPCP.ipcpID := id;
      return IPCP.ipcpID = id;
   end setID;

end ipcp;