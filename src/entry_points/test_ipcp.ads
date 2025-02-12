with AUnit.Test_Cases; use AUnit.Test_Cases;
with IPCP; use IPCP;

package Test_IPCP is
   -- Define a new test case type
   type Test_Case is new Test_Case_Type with null record;

   -- Override the procedure to register tests
   overriding procedure Register_Tests(Test: in out Test_Case);

   -- Override the function to return the name of the test case
   overriding function Name(Test: Test_Case) return Test_String;

private
   -- Declare an instance of the test case
   Test_1: aliased Test_Case;
end Test_IPCP;
