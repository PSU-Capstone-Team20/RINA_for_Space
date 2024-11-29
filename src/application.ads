with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package application is
    type application is tagged record
       appName : String;
       appID   : Integer;
    end record;
end application;