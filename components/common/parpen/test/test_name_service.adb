with AUnit.Assertions; use AUnit.Assertions;
with Parpen.Name_Service;

package body Test_Name_Service is

   function Name (T : Test) return AUnit.Message_String is
      pragma Unreferenced (T);
   begin
      return AUnit.Format ("Parpen name service");
   end Name;

   procedure Test_Simple_Add (T : in out Aunit.Test_Cases.Test_Case'Class)
   is
      use Parpen;
      use type Parpen.Name_Service.Status;
      DB     : Name_Service.Database (100);
      Result : Name_Service.Status;
   begin
      DB.Init;
      DB.Add (14, "DB", Result);

      Assert (Result = Name_Service.Status_OK, "Adding entry into emptry database failed");
      Assert (DB.Exists ("DB"), "Element does not exist after add");
      Assert (not DB.Exists ("Unrelated"), "Non-existing element found");

      DB.Add (21, "DB", Result);
      Assert (Result = Name_Service.Status_In_Use, "Double insersion not detected");
   end Test_Simple_Add;

   procedure Test_Simple_Get (T : in out Aunit.Test_Cases.Test_Case'Class)
   is
      use Parpen;
      use type Parpen.Name_Service.Status;
      use type Parpen.Name_Service.Result;
      DB     : Name_Service.Database (100);
      Result : Name_Service.Result;
      Status : Name_Service.Status;

      Key   : String  := "Some Long Key";
      Value : Natural := 123456;
   begin
      DB.Init;
      DB.Add (Value, Key, Status);
      Assert (Status = Name_Service.Status_OK, "Adding entry into emptry database failed");
      DB.Get (Key, Result);
      Assert (Result.Valid, "Get was invalid");
      Assert (Result.Elem = Value, "Get returned invalid value");
      DB.Get ("Non-existing key", Result);
      Assert (not Result.Valid, "Get for non-existing element was valid");
   end Test_Simple_Get;

   procedure Test_Overflow (T : in out Aunit.Test_Cases.Test_Case'Class)
   is
      use Parpen;
      use type Parpen.Name_Service.Status;
      use type Parpen.Name_Service.Result;
      DB     : Name_Service.Database (10);
      Status : Name_Service.Status;
   begin
      DB.Init;
      for I in 1 .. 10 loop
         DB.Add (I, I'Img, Status);
         Assert (Status = Name_Service.Status_OK, "Adding entry" & I'Img & " into emptry database failed");
      end loop;
      DB.Add (12345, "Another key", Status);
      Assert (Status = Name_Service.Status_Out_Of_Memory, "Overflow undetected");
   end Test_Overflow;

   procedure Register_Tests (T : in out Test) is
      use AUnit.Test_Cases.Registration;
   begin
      Register_Routine (T, Test_Simple_Add'Access, "Simple add");
      Register_Routine (T, Test_Simple_Get'Access, "Simple get");
      Register_Routine (T, Test_Overflow'Access, "Overflow");
   end Register_Tests;

end Test_Name_Service;
