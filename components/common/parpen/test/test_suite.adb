with Test_Name_Service;

package body Test_Suite is

   function Suite return Access_Test_Suite is                                                                           
      Result : constant Access_Test_Suite := new AUnit.Test_Suites.Test_Suite;
   begin
      Result.Add_Test (new Test_Name_Service.Test);
      return Result;
   end Suite;

end Test_Suite;
