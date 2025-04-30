with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with mockcomp; use mockcomp;

procedure Test_Mock_Comp is
   -- Create an instance of the mock_comp task
   Comp : mockcomp.mock_comp;
begin

   -- Test change name of the mock computer
   Comp.change_name(To_Unbounded_String("SimulatedNode"));

   -- Test simulate normal operation
   Put_Line("Calling operate...");
   Comp.operate;

   -- Clean up the mock component
   Put_Line("Calling delete...");
   Comp.delete;

end Test_Mock_Comp;
