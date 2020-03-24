with AUnit;
with AUnit.Test_Cases;

package Test_Message is
   pragma Elaborate_Body;

   type Test is new AUnit.Test_Cases.Test_Case with null record;

   function Name (T : Test) return AUnit.Message_String;

   procedure Register_Tests (T : in out Test);

private

   procedure Test_Register_Service (T : in out AUnit.Test_Cases.Test_Case'Class);

end Test_Message;
