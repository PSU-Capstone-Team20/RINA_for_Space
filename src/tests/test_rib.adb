with Ada.Assertions; use Ada.Assertions;
with Ada.Exceptions;   use Ada.Exceptions;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;
with Test_Utils; use Test_Utils;
with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Strings.Hash;
with GNAT.Table;
with RIB; use RIB;

procedure Test_RIB is
   pragma Assertion_Policy (Assert => Ignore);

   Name1 : constant Unbounded_String := +"Entry1";
   Name2 : constant Unbounded_String := +"Entry2";
   DIF1  : constant Unbounded_String := +"DIF1";
   DIF2  : constant Unbounded_String := +"DIF2";
   APN1  : constant Unbounded_String := +"APN1";
   APN2  : constant Unbounded_String := +"APN2";
   IPCP1 : IPCP_obj := (IPCP => +"IPCP1");
   IPCP2 : IPCP_obj := (IPCP => +"IPCP2");

procedure Assert_DIF_In_Entry(Name : Unbounded_String; DIF : Unbounded_String) is 
   Mock_Entry : RIB_Entry := Create_Mock_RIB_Entry(Name);
begin
   Mock_Entry.Obj_Type.Connected_DIFs.Append(DIF);

   for D of Mock_Entry.Obj_Type.Connected_DIFs loop
      if D = DIF then
         return;  
      end if;
   end loop;

   Assert(False, "DIF" & (+DIF) & " should be in entry " & (+Name));
end Assert_DIF_In_Entry;

begin

   Assert_DIF_In_Entry (Name1, DIF1);


end Test_RIB;