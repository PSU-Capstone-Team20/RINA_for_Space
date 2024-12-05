with RINA_Policies;

package RINA_Policies.DTN_Bundle_Protocol is 

	type Bundle_ID is new Integer;
	type Custody_Status is (Pending, Accepted, Rejected);

	procedure Create_Bundle (Flow : Flow_ID; Bundle : out Bundle_ID);

	procedure Send_Bundle (Flow : in Flow_ID; Bundle : in Bundle_ID);

	procedure Receive_Bundle (Flow : in Flow_ID; Bundle : out Bundle_ID);

	--custody transfer, still working 
	procedure Handle_Custody (Bundle : in Bundle_ID; Status : in Custody_Status);

end RINA_Policies.DTN_Bundle_Protocol;