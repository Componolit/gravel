with AUnit;
with AUnit.Test_Cases;

package Test_Parse is
   pragma Elaborate_Body;

   type Test is new AUnit.Test_Cases.Test_Case with null record;

   function Name (T : Test) return AUnit.Message_String;

   procedure Register_Tests (T : in out Test);

private

   procedure Test_Parse_Strong_Binder (T : in out AUnit.Test_Cases.Test_Case'Class);
   procedure Test_Parse_Weak_Handle (T : in out AUnit.Test_Cases.Test_Case'Class);
   procedure Test_Parse_Weak_Handle_With_Offset (T : in out AUnit.Test_Cases.Test_Case'Class);
   procedure Test_Resolve_Invalid_Source (T : in out AUnit.Test_Cases.Test_Case'Class);
   procedure Test_Resolve_Invalid_Dest (T : in out AUnit.Test_Cases.Test_Case'Class);
   procedure Test_Resolve_Invalid_Node (T : in out AUnit.Test_Cases.Test_Case'Class);
   procedure Test_Resolve_Missing_Handle (T : in out AUnit.Test_Cases.Test_Case'Class);
   procedure Test_Resolve_Missing_Node (T : in out AUnit.Test_Cases.Test_Case'Class);
   procedure Test_Resolve_Handle_To_Binder (T : in out AUnit.Test_Cases.Test_Case'Class);
   procedure Test_Resolve_Binder_To_Handle (T : in out AUnit.Test_Cases.Test_Case'Class);
   procedure Test_Resolve_Handle_To_Handle (T : in out AUnit.Test_Cases.Test_Case'Class);

end Test_Parse;
