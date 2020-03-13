with Test_DB;
with Test_Unique_Map;
with Test_NameDB;
with Test_Parse;

package body Test_Suite is

   function Suite return Access_Test_Suite is                                                                           
      Result : constant Access_Test_Suite := new AUnit.Test_Suites.Test_Suite;
   begin
      Result.Add_Test (new Test_NameDB.Test);
      Result.Add_Test (new Test_DB.Test);
      Result.Add_Test (new Test_Parse.Test);
      Result.Add_Test (new Test_Unique_Map.Test);
      return Result;
   end Suite;

end Test_Suite;
