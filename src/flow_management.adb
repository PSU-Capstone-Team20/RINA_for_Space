with Ada.Text_IO; use Ada.Text_IO;

package body Flow_Management is
   procedure Display_Flow(Flow : Flow_T) is
   begin
      Put_Line("Flow ID: " & Flow.Flow_ID'Image);
      Put_Line("Source: " & To_String(Flow.Source_Address.App_Process_Name));
      Put_Line("Destination: " & To_String(Flow.Destination_Address.App_Process_Name));
      Put_Line("State: " & Flow.State'Image);
   end Display_Flow;
end Flow_Management;
