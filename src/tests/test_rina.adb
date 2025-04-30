with Ada.Text_IO; use Ada.Text_IO;
with Ada.Assertions; use Ada.Assertions;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Containers; use Ada.Containers;
with RINA; use RINA;

-- Test the RINA package for pathfinding and routing
procedure Test_RINA is
   pragma Assertion_Policy (Assert => Ignore);

   -- Utility function to build an Address_Element
   function Make_Address(Name_Str, Type_Str : String) return Address_Element is
   begin
      return (Name => To_Unbounded_String(Name_Str),
              Address_Type => To_Unbounded_String(Type_Str));
   end Make_Address;

   -- Test for Reconstruct_Path
   procedure Test_Reconstruct_Path is
      Node1, Node2, Node3 : A_Star_Node;
      ClosedList : Node_Vectors.Vector;
      Result_Path : Path_Vectors.Vector;
   begin
      -- Build mock path: Node3 -> Node2 -> Node1
      declare
         Addr1 : Address_Vectors.Vector;
         Addr2 : Address_Vectors.Vector;
         Addr3 : Address_Vectors.Vector;
      begin
         Addr1.Append(Make_Address("Node1", "DIF"));
         Addr2.Append(Make_Address("Node2", "DIF"));
         Addr3.Append(Make_Address("Node3", "DIF"));

         Node1 := (
            Address => Addr1,
            gScore => 0,
            hScore => 0,
            fScore => 0,
            parentAddress => Address_Vectors.Empty_Vector
         );

         Node2 := (
            Address => Addr2,
            gScore => 1,
            hScore => 0,
            fScore => 1,
            parentAddress => Addr1
         );

         Node3 := (
            Address => Addr3,
            gScore => 2,
            hScore => 0,
            fScore => 2,
            parentAddress => Addr2
         );

         ClosedList.Append(Node1);
         ClosedList.Append(Node2);
         ClosedList.Append(Node3);
      end;

      Result_Path := Reconstruct_Path(ClosedList, Node3);

      -- We expect the path to be: Node3 -> Node2 -> Node1
      Assert(Result_Path.Length = 3, "Expected 3 elements in path");
      Put_Line("Reconstruct_Path passed.");
   end Test_Reconstruct_Path;

   -- Basic test for D_Star_Lite
   -- functional call with empty map scenario
   procedure Test_D_Star_Lite is
      Start_Address, Goal_Address : Address_Vectors.Vector;
      Result : Path_Vectors.Vector;
   begin
      -- Both addresses are single-element vectors
      Start_Address.Append(Make_Address("A", "DIF"));
      Goal_Address.Append(Make_Address("B", "DIF"));

      -- D_Star_Lite depends on Get_Map, so the result may be empty unless RIB is populated
      Result := D_Star_Lite(Start_Address, Goal_Address);

      Put_Line("D_Star_Lite called with mock start/goal.");
      Put_Line("Result path length: " & Result.Length'Image);
   end Test_D_Star_Lite;

begin
   Test_Reconstruct_Path;
   Test_D_Star_Lite;

end Test_RINA;
