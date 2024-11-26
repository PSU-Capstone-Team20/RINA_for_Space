
package body RINA_Policies.DTN_Bundle_Protocol is 
	
	procedure Create_Bundle (
		Flow : in Flow_ID;
		Bundle : out Bundle_ID
	) is 
	begin
		
		Put_Line("Creating a new Bundle for Flow " & Integer'Image(Flow));
		Bundle := 1; 
	end Create_Bundle;

	procedure Send_Bundle (
		Flow : in Flow_ID; 
		Bundle : in Bundle_ID
	) is
	begin
		Put_Line("Sending Bundle " & Integer'Image(Bundle) & " over Flow " & Integer'Image(Flow));
	end Send_Bundle;

	procedure Receive_Bundle (
		Flow : in Flow_ID;
		Bundle : out Bundle_ID
	) is
	begin
		Put_Line("Receiving a bundle from Flow " & Integer'Image(Flow));
		Bundle := 2;
	end Receive_Bundle;

	procedure Handle_Custody (
		Bundle : in Bundle_ID;
		Status : in Custody_Status
	) is
	begin
		case Status is 
			when Pending => 
				Put_Line("Custody for Bundle " & Integer'Image(Bundle) & ": Pending");
			when Accepted =>
				Put_Line("Custody for Bundle " & Integer'Image(Bundle) & ": Accepted");
			when Rejected => 
				Put_Line("Custody for Bundle " & Integer'Image(Bundle) & ": Rejected");
			
		end case;
	end Handle_Custody;
end RINA_Policies.DTN_Bundle_Protocol;