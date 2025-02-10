with Ada.Text_IO; use Ada.Text_IO;

package body IPCP.Resource_Management is

   -- Allocate a new resource
   procedure Allocate_Resource(Resources : in out Resource_List; Name : String) is
      New_ID : Resource_ID := Resource_ID(Resources.Length + 1);
      New_Resource : Resource_Record := (ID => New_ID, Name => Name, State => Allocated);
   begin
      Resources.Append(New_Resource);
      Put_Line("Resource '" & Name & "' allocated with ID: " & New_ID'Image);
   end Allocate_Resource;

   -- Release a resource by ID
   procedure Release_Resource(Resources : in out Resource_List; ID : Resource_ID) is
   begin
      for I in Resources.First_Index .. Resources.Last_Index loop
         if Resources(I).ID = ID then
            Resources(I).State := Free;
            Put_Line("Resource with ID " & ID'Image & " has been released.");
            exit;
         end if;
      end loop;
   end Release_Resource;

   -- Get the current status of a resource
   function Get_Resource_Status(Resources : Resource_List; ID : Resource_ID) return Resource_State is
   begin
      for Resource of Resources loop
         if Resource.ID = ID then
            return Resource.State;
         end if;
      end loop;
      return Free; 
   end Get_Resource_Status;

   -- Display all resources
   procedure Display_Resources(Resources : Resource_List) is
   begin
      Put_Line("Listing all resources:");
      for Resource of Resources loop
         Put_Line("ID: " & Resource.ID'Image & ", Name: " & Resource.Name & ", State: " & Resource_State'Image(Resource.State));
      end loop;
   end Display_Resources;

end IPCP.Resource_Management;

