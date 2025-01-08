with RINA_Policies;

package RINA_Policies.DTN_Bundle_Protocol is 

   --primary block 
   type Primary_Block is record 
      Version     : Integer;
      Processing_Flag : RINA_Policies.SDNV;
      Creation_Timestamp : RINA_Policies.SDNV; 
      Lifetime : RINA_Policies.SDNV;
      Source_EID : String(1 .. 256);
      Destination_EID : String (1 .. 256);
   end record;

   -- payload blcok
   type Payload_Block is record 
      Data_Length : RINA_Policies.SDNV;
      Data : String(1 .. 1024);
   end record;
   
   --metadata bundle 
   type Bundle is record 
      Primary : Primary_Block;
      Payload : Payload_Block;
   end record;

   --procedures for bundle managing 
   procedure Create_Bundle(Flow : in RINA_Policies.Flow_ID; Bundle : out Bundle);
   procedure Process_Bundle(Flow : in RINA_Policies.Flow_ID; Bundle : in out Bundle);
   procedure Transmit_Bundle(Flow : in RINA_Policies.Flow_ID; Bundle : in Bundle);
   
end RINA_Policies.DTN_Bundle_Protocol;