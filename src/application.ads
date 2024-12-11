with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package Application is

   type application is tagged record
      appName : Unbounded_String;
      appID   : Integer;
   end record;

end Application;
