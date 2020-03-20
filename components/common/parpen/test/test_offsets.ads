with AUnit;
with AUnit.Test_Cases;

package Test_Offsets is
   pragma Elaborate_Body;

   type Test is new AUnit.Test_Cases.Test_Case with null record;

   function Name (T : Test) return AUnit.Message_String;

   procedure Register_Tests (T : in out Test);

private

   procedure Test_Parse_Offset_List (T : in out AUnit.Test_Cases.Test_Case'Class);
   procedure Test_Empty_Offset_List (T : in out AUnit.Test_Cases.Test_Case'Class);
   procedure Test_Single_Offset (T : in out AUnit.Test_Cases.Test_Case'Class);
   procedure Test_Multiple_Offsets (T : in out AUnit.Test_Cases.Test_Case'Class);
   procedure Test_Multiple_Offsets_Mixed (T : in out AUnit.Test_Cases.Test_Case'Class);
   procedure Test_Out_Of_Range_Offset (T : in out AUnit.Test_Cases.Test_Case'Class);

end Test_Offsets;
