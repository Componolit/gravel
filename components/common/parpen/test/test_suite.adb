with Test_DB;
with Test_NameDB;

package body Test_Suite is

   function Suite return Access_Test_Suite is                                                                           
      Result : constant Access_Test_Suite := new AUnit.Test_Suites.Test_Suite;
   begin
      Result.Add_Test (new Test_NameDB.Test);
      Result.Add_Test (new Test_DB.Test);
      return Result;
   end Suite;

end Test_Suite;
