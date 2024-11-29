with DIF;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package ipcp is
   
   type IPCP is tagged record
      ipcpID : Natural;
      ipcpName : String;
      associatedDIF : DIF;
      state : String;
   end record;

   -- Creates IPCP instance
   procedure setName (Self : in out IPCP; name : String);

   -- Gets the IPCP name
   function getName (Self : in IPCP) return String;

   -- Joins a DIF
   function JoinDIF (Self : in out IPCP; dif : in DIF) return Boolean;

   -- Leaves a DIF
   function LeaveDIF (Self : in out IPCP) return Boolean;

   -- Discovers DIFs
   function DiscoverDIF (Self : in out IPCP) return Boolean;

   -- Gets the current state of the IPCP
   function getState (Self : in IPCP) return String;

   -- Sets the state of the IPCP
   function setState (Self : in out IPCP; state : String) return Boolean;

   -- Gets the ID of IPCP
   function getID (Self : in IPCP) return Natural;

   -- Sets the ID of IPCP
   function setID (Self : in out IPCP; id : Natural) return Boolean;

   -- TODO: Some sort of handshake/keep alive logic to flag state if falls inactive
   -- TODO: Have abilitiy to hold and manage bundles
   -- TODO: Have interface between DIF level send/receive data

end ipcp;