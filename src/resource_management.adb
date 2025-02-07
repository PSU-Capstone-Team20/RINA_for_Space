with Ada.Text_IO; use Ada.Text_IO;

package body Resource_Management is
   procedure Display_Resource(Resource : Resource_T) is
   begin
      Put_Line("Resource ID: " & Resource.Resource_ID'Image);
      Put_Line("Bandwidth: " & Resource.Allocated_Bandwidth'Image & " kbps");
      Put_Line("CPU Allocation: " & Resource.CPU_Allocation'Image & "%");
   end Display_Resource;
end Resource_Management;
