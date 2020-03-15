with AUnit;
with AUnit.Test_Cases;

package Test_DB is
   pragma Elaborate_Body;

   type Test is new AUnit.Test_Cases.Test_Case with null record;

   function Name (T : Test) return AUnit.Message_String;

   procedure Register_Tests (T : in out Test);

private

   procedure Test_Basic_Insert (T : in out AUnit.Test_Cases.Test_Case'Class);
   procedure Test_Basic_Delete (T : in out AUnit.Test_Cases.Test_Case'Class);
   procedure Test_Overflow (T : in out AUnit.Test_Cases.Test_Case'Class);
   procedure Test_Search_Value (T : in out AUnit.Test_Cases.Test_Case'Class);
   procedure Test_Search_Partial_Value (T : in out AUnit.Test_Cases.Test_Case'Class);

end Test_DB;
