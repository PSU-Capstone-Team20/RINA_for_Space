with AUnit.Assertions; use AUnit.Assertions;
with AUnit.Test_Cases; use AUnit.Test_Cases;

with AUnit;
with AUnit.Run;
with AUnit.Reporter.Text;
with IPCP; use IPCP;
with Ada.Text_IO; use Ada.Text_IO;

package body Test_IPCP is

   procedure Register_Tests(Test: in out Test_Case) is
   begin
      Register_Routine(Test, Test_Initialize_IPCP'Access, "Test_Initialize_IPCP");
      Register_Routine(Test, Test_Activate_IPCP'Access, "Test_Activate_IPCP");
      Register_Routine(Test, Test_Terminate_IPCP'Access, "Test_Terminate_IPCP");
      Register_Routine(Test, Test_Get_IPCP_State'Access, "Test_Get_IPCP_State");
      Register_Routine(Test, Test_Connect_To_DIF'Access, "Test_Connect_To_DIF");
      Register_Routine(Test, Test_Disconnect_From_DIF'Access, "Test_Disconnect_From_DIF");
      Register_Routine(Test, Test_Get_Connected_DIF'Access, "Test_Get_Connected_DIF");
   end Register_Tests;

   function Name(Test: Test_Case) return Test_String is
   begin
      return Format("Test_IPCP");
   end Name;

   procedure Test_Initialize_IPCP(Test: in out Test_Case'Class) is
      IPCP_Instance: IPCP_Record;
   begin
      Initialize_IPCP(IPCP_Instance, "Test_IPCP", "127.0.0.1", "High");
      Assert(IPCP_Instance.State = Initialized, "IPCP should be initialized");
   end Test_Initialize_IPCP;

   procedure Test_Activate_IPCP(Test: in out Test_Case'Class) is
      IPCP_Instance: IPCP_Record;
   begin
      Initialize_IPCP(IPCP_Instance, "Test_IPCP", "127.0.0.1", "High");
      Activate_IPCP(IPCP_Instance);
      Assert(IPCP_Instance.State = Active, "IPCP should be active");
   end Test_Activate_IPCP;

   procedure Test_Terminate_IPCP(Test: in out Test_Case'Class) is
      IPCP_Instance: IPCP_Record;
   begin
      Initialize_IPCP(IPCP_Instance, "Test_IPCP", "127.0.0.1", "High");
      Activate_IPCP(IPCP_Instance);
      Terminate_IPCP(IPCP_Instance);
      Assert(IPCP_Instance.State = Terminated, "IPCP should be terminated");
   end Test_Terminate_IPCP;

   procedure Test_Get_IPCP_State(Test: in out Test_Case'Class) is
      IPCP_Instance: IPCP_Record;
   begin
      Initialize_IPCP(IPCP_Instance, "Test_IPCP", "127.0.0.1", "High");
      Assert(Get_IPCP_State(IPCP_Instance) = Initialized, "IPCP state should be initialized");
   end Test_Get_IPCP_State;

   procedure Test_Connect_To_DIF(Test: in out Test_Case'Class) is
      IPCP_Instance: IPCP_Record;
      DIF_Instance: DIF_Access := new DIF_Record;
   begin
      Initialize_IPCP(IPCP_Instance, "Test_IPCP", "127.0.0.1", "High");
      Connect_To_DIF(IPCP_Instance, DIF_Instance);
      Assert(IPCP_Instance.Connected_DIF = DIF_Instance, "IPCP should be connected to DIF");
   end Test_Connect_To_DIF;

   procedure Test_Disconnect_From_DIF(Test: in out Test_Case'Class) is
      IPCP_Instance: IPCP_Record;
      DIF_Instance: DIF_Access := new DIF_Record;
   begin
      Initialize_IPCP(IPCP_Instance, "Test_IPCP", "127.0.0.1", "High");
      Connect_To_DIF(IPCP_Instance, DIF_Instance);
      Disconnect_From_DIF(IPCP_Instance);
      Assert(IPCP_Instance.Connected_DIF = null, "IPCP should be disconnected from DIF");
   end Test_Disconnect_From_DIF;

   procedure Test_Get_Connected_DIF(Test: in out Test_Case'Class) is
      IPCP_Instance: IPCP_Record;
      DIF_Instance: DIF_Access := new DIF_Record;
   begin
      Initialize_IPCP(IPCP_Instance, "Test_IPCP", "127.0.0.1", "High");
      Connect_To_DIF(IPCP_Instance, DIF_Instance);
      Assert(Get_Connected_DIF(IPCP_Instance) = DIF_Instance, "Connected DIF should be retrieved");
   end Test_Get_Connected_DIF;


   procedure Run_Tests is
      Test_Suite : aliased Test_Case;
      Reporter   : AUnit.Reporter.Text.Text_Reporter;
   begin
      AUnit.Run.Run(Test_Suite, Reporter);
   end Run_Tests;

begin
   Run_Tests;
end Test_IPCP;
