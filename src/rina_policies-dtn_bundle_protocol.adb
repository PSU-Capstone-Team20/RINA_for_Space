with Ada.Text_IO; use Ada.Text_IO;
with RINA_Policies;

package body RINA_Policies.DTN_Bundle_Protocol is 
	
   Min_Flow_ID : constant Integer := 0;
   Max_Bundle_ID : constant Integer := 9998;

	procedure Create_Bundle (Flow : in Flow_ID; Bundle : out Bundle_ID) is 
	begin
		if Integer(Flow) < Min_Flow_ID then
         Put_Line(Integer'Image(Integer(Flow)) & " Flow ID is not allowed.");
         Bundle := 0; 
      else 
		   Put_Line("Creating a new Bundle for Flow " & Integer'Image(Integer(Flow)));
		 Bundle := 1; --place holder for bundle ID generation
      end if;
	end Create_Bundle;

	procedure Send_Bundle(Flow : in Flow_ID; Bundle : in Bundle_ID) is
	begin
      if Integer(Flow) < Min_Flow_ID then
         Put_Line(Integer'Image(Integer(Flow)) & " Flow ID is not allowed.");
      elsif Integer(Bundle) > Max_Bundle_ID then 
         Put_Line(Integer'Image(Integer(Bundle)) & " Bundle ID is out of bounds.");
      else
		   Put_Line("Sending Bundle " & Integer'Image(Integer(Bundle)));
		   Put_Line(" over Flow " & Integer'Image(Integer(Flow)));
      end if;
	end Send_Bundle;

	procedure Receive_Bundle (Flow : in Flow_ID; Bundle : out Bundle_ID) is
	begin
      if Integer(Flow) < Min_Flow_ID then 
         Put_Line("Error: Flow ID " & Integer'Image(Integer(Flow)) & " is not allowed.");
      else
		   Put_Line("Receiving a bundle from Flow " & Integer'Image(Integer(Flow)));
		   Bundle := 2;
      end if;
	end Receive_Bundle;

	procedure Handle_Custody (Bundle : in Bundle_ID; Status : in Custody_Status) is
	begin
      if Integer(Bundle) > Max_Bundle_ID then 
         Put_Line("Error: Bundle ID " & Integer'Image(Integer(Bundle)) & " is not allowed");
      else
		   case Status is 
			   when Pending => 
				   Put_Line("Custody for Bundle " & Integer'Image(Integer(Bundle)));
				   Put_Line("is: Pending");
			   when Accepted =>
				   Put_Line("Custody for Bundle " & Integer'Image(Integer(Bundle)));
				   Put_Line("is: Accepted");
			   when Rejected => 
				   Put_Line("Custody for Bundle " & Integer'Image(Integer(Bundle)));
				   Put_Line("is: Rejected");
			
		   end case;
      end if;
	end Handle_Custody;
end RINA_Policies.DTN_Bundle_Protocol;