with AUnit.Assertions; use AUnit.Assertions;
with Parpen.Unique_Map;

package body Test_Unique_Map is

   type Element is
   record
      Left  : Natural;
      Right : Natural;
   end record;

   type Key is new Natural range 0 .. 100;

   package DB is new Parpen.Unique_Map (Element      => Element,
                                        Null_Element => (0, 0),
                                        Key          => Key);

   use type DB.Status;
   use type DB.Option;

   function Name (T : Test) return AUnit.Message_String is
      pragma Unreferenced (T);
   begin
      return AUnit.Format ("Fixed map");
   end Name;

   procedure Test_Basic_Insert (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
      Database   : DB.Database;
      Result     : DB.Option;
      New_Result : DB.Option;
   begin
      Database.Initialize;

      Result := Database.Get (K => 14);
      Assert (Result.Result = DB.Status_Not_Found, "Element found in empty database");

      Database.Insert (K => 14, E => (100, 200));
      Result := Database.Get (K => 14);
      Assert (Result.Result = DB.Status_OK, "Unexpected result");
      Assert (Result.Data = (100, 200), "Invalid element");

      Result := Database.Get (K => 14);
      Assert (Result.Result = DB.Status_OK, "Element not found in database");

      New_Result := Database.Get (K => 14);
      Assert (New_Result = Result, "Result differ between requests");

      Result := Database.Get (K => 15);
      Assert (Result.Result = DB.Status_Not_Found, "Non-existing element found in database");
   end Test_Basic_Insert;

   procedure Test_Basic_Delete (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
      Database : DB.Database;
      Result   : DB.Option;
   begin
      Database.Initialize;

      Result := Database.Get (K => 14);
      Assert (Result.Result = DB.Status_Not_Found, "Element found in empty database");

      Database.Insert (K => 14, E => (100, 200));
      Result := Database.Get (K => 14);
      Assert (Result.Result = DB.Status_OK, "Unexpected element");
      Assert (Result.Data = (100, 200), "Invalid element");

      Database.Delete (K => 14);
      Result := Database.Get (K => 14);
      Assert (Result.Result = DB.Status_Not_Found, "Element found after deletion database");
   end Test_Basic_Delete;

   procedure Test_Search_Value (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
      Database : DB.Database;
      Result   : DB.Option;
   begin
      Database.Initialize;

      Result := Database.Find (E => (43, 44));
      Assert (Result.Result = DB.Status_Not_Found, "Value found in empty database");

      Database.Insert (K => 14, E => (43, 44));
      Result := Database.Find (E => (43, 44));
      Assert (Result.Result = DB.Status_OK, "Value not found database");
      Assert (Result.Data = (43, 44), "Invalid element for result of Search_Value");
   end Test_Search_Value;

   procedure Test_Search_Partial_Value (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
      Database : DB.Database;
      Result   : DB.Option;

      function Match (L, R : Element) return Boolean is (L.Left = R.Left);
      function Search_Partial is new DB.Generic_Find (Match);
   begin
      Database.Initialize;

      Result := Database.Find (E => (43, 44));
      Assert (Result.Result = DB.Status_Not_Found, "Value found in empty database");

      Database.Insert (K => 14, E => (43, 44));
      Result := Search_Partial (Database, E => (43, 44));
      Assert (Result.Result = DB.Status_OK, "Value not found database");
      Result := Database.Get (K => 14);
      Assert (Result.Result = DB.Status_OK, "Value not found database");
      Assert (Result.Data = (43, 44), "Invalid element for result of Search_Value");
   end Test_Search_Partial_Value;

   procedure Test_Nested_Insert (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
      package Inner is new Parpen.Unique_Map (Element      => Element,
                                              Null_Element => (0, 0),
                                              Key          => Key);

      package Outer is new Parpen.Unique_Map (Element      => Inner.Database,
                                              Null_Element => Inner.Null_DB,
                                              Key          => Key);
      Outer_Result : Outer.Option;
      Database     : Outer.Database;

      procedure Set_Value (DB : in out Inner.Database);
      procedure Check_Value (DB : in out Inner.Database);

      procedure Set_Value (DB : in out Inner.Database)
      is
         Result : Inner.Option;
         use type Inner.Status;
      begin
         Result := DB.Get (K => 11);
         Assert (Result.Result = Inner.Status_Not_Found, "Element found in empty database");
         DB.Insert (K => 11, E => (1234, 5678));
      end Set_Value;

      procedure Check_Value (DB : in out Inner.Database)
      is
         Result : Inner.Option;
         use type Inner.Status;
      begin
         Result := DB.Get (K => 11);
         Assert (Result.Result = Inner.Status_OK, "Element not found");
         Result := DB.Get (K => 11);
         Assert (Result.Result = Inner.Status_OK, "Element not found");
         Assert (Result.Data = (1234, 5678), "Invalid value");
      end Check_Value;

      procedure Set_Value is new Outer.Generic_Apply (Operation => Set_Value);
      procedure Check_Value is new Outer.Generic_Apply (Operation => Check_Value);

      use type Outer.Status;
   begin
      Database.Initialize;

      Outer_Result := Database.Get (K => 14);
      Assert (Outer_Result.Result = Outer.Status_Not_Found, "Element found in empty database");
      Database.Insert (K => 14, E => Inner.Null_DB);

      Set_Value (DB => Database, K => 14);
      Check_Value (DB => Database, K => 14);
   end Test_Nested_Insert;

   procedure Register_Tests (T : in out Test) is
      use AUnit.Test_Cases.Registration;
   begin
      Register_Routine (T, Test_Basic_Insert'Access, "Basic insert");
      Register_Routine (T, Test_Basic_Delete'Access, "Basic delete");
      Register_Routine (T, Test_Search_Value'Access, "Search value");
      Register_Routine (T, Test_Search_Partial_Value'Access, "Search partial value");
      Register_Routine (T, Test_Nested_Insert'Access, "Nested insert");
   end Register_Tests;

end Test_Unique_Map;
