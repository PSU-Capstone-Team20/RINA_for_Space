with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package application is

    type application is tagged record
       appName : Unbounded_String;
       appID   : Integer;
    end record;

   -- TODO Function to initialize application
   -- TODO Function to get ApplicationName
   -- TODO Function to get ApplicationID  
   -- TODO Get "Running on a task"
      
end application;