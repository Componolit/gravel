with Test_NameDB;
with Test_DB;
with Test_Parse;
with Test_Unique_Map;
with Test_Offsets;
with Test_Message;

package body Test_Suite is

   NameDB_Tests     : aliased Test_NameDB.Test;
   DB_Tests         : aliased Test_DB.Test;
   Parse_Tests      : aliased Test_Parse.Test;
   Unique_Map_Tests : aliased Test_Unique_Map.Test;
   Offset_Tests     : aliased Test_Offsets.Test;
   Message_Tests    : aliased Test_Message.Test;

   function Suite return Access_Test_Suite is
      Result           : constant Access_Test_Suite := new AUnit.Test_Suites.Test_Suite;
   begin
      Result.Add_Test (NameDB_Tests'Access);
      Result.Add_Test (DB_Tests'Access);
      Result.Add_Test (Parse_Tests'Access);
      Result.Add_Test (Unique_Map_Tests'Access);
      Result.Add_Test (Offset_Tests'Access);
      Result.Add_Test (Message_Tests'Access);
      return Result;
   end Suite;

end Test_Suite;
