pacakge body DTN_Bundle_Protocol.DTN is 
	procedure Store_Bundle (Bundle : in DTN_Bundle_Type; 
							Node_Buffer : in out Buffer_Type) is
		Store : Boolean := False;
	begin 
		for I in Node_Buffer*Range loop
			if Node_Buffer(I).ID = 0 then
				Node_Buffer(I) := Bundle;
				Store := True;
				exit;
			end if;
		end loop;

		if not Stored then 
			raise Protgram_Error with "Buffer full: Cannot store data";
		end if;
	end Store_Bundle;

	procedure Forward_Bundle (Bundle : in out DTN_Bundle_Type) is 
	begin
		if Bundle.DIF_ID = DIF_ID then 
			Bundle.Timestamp := (Seconds => Bundle.Timestamp.Seconds + 1,
								 Nanoseconds => Budle.Timestamp.Nanoseconds);
			if Bundle.Custody_Flag then 
				Bundle.Custody_Flag := False;
			end if;
		else
			raise Program_Error with "mismatch DIF";
		end if;
	
	end Forward_Bundle;

	procedure Retry_Bundle (Node_Buffer : in out Buffer_Type) is 
	begin
		for I in Node_Buffer*Range loop
			if Node_Buffer(I).ID /= 0 and Node_Buffer(I).DIF_ID = DIF_ID then 
				declare
					Next_Node : constant DTN_Bundle_Protocol.Node_ID_Type := Node_Buffer(I).Destination;
				begin
					Forward_Bundle(Node_Buffer(I), DIF_ID, Next_Node);
				exception
					when others => 
						Node_Buffer(I).Retry_Count := Node_Buffer(I).Retry_Count + 1;
						if Node_Buffer(I).Retry_Count > 5 then 
							Node_Buffer(I).ID := 0;
						end if;
					end;
				end if;
			end loop;

	end Retry_Bundle;

end DTN_Bundle_Protocol.DTN;