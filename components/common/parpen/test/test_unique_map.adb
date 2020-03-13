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
                                        Key          => Key,
                                        Null_Key     => 0);

   use type DB.Status;
   use type DB.Option;

   function Name (T : Test) return AUnit.Message_String is
      pragma Unreferenced (T);
   begin
      return AUnit.Format ("Fixed map");
   end Name;

   procedure Test_Basic_Insert (T : in out Aunit.Test_Cases.Test_Case'Class)
   is
      Database   : DB.Database;
      Cursor     : DB.Option;
      New_Cursor : DB.Option;
   begin
      Database.Initialize;

      Cursor := Database.Find (K => 14);
      Assert (Cursor.Result = DB.Status_Not_Found, "Element found in empty database");

      Database.Insert (K => Cursor.Cursor, E => (100, 200));
      Assert (Database.Get (K => Cursor.Cursor) = (100, 200), "Invalid element");

      Cursor := Database.Find (K => 14);
      Assert (Cursor.Result = DB.Status_OK, "Element not found in database");

      New_Cursor := Database.Find (K => 14);
      Assert (New_Cursor = Cursor, "Cursor differ between requests");

      Cursor := Database.Find (K => 15);
      Assert (Cursor.Result = DB.Status_Not_Found, "Non-existing element found in database");
   end Test_Basic_Insert;

   procedure Test_Basic_Delete (T : in out Aunit.Test_Cases.Test_Case'Class)
   is
      Database : DB.Database;
      Cursor   : DB.Option;
   begin
      Database.Initialize;

      Cursor := Database.Find (K => 14);
      Assert (Cursor.Result = DB.Status_Not_Found, "Element found in empty database");

      Database.Insert (K => Cursor.Cursor, E => (100, 200));
      Assert (Database.Get (K => Cursor.Cursor) = (100, 200), "Invalid element");

      Database.Delete (K => Cursor.Cursor);

      Cursor := Database.Find (K => 14);
      Assert (Cursor.Result = DB.Status_Not_Found, "Element found after deletion database");
   end Test_Basic_Delete;

   procedure Test_Search_Value (T : in out Aunit.Test_Cases.Test_Case'Class)
   is
      Database : DB.Database;
      Cursor   : DB.Option;
   begin
      Database.Initialize;

      Cursor := Database.Search_Value (E => (43, 44));
      Assert (Cursor.Result = DB.Status_Not_Found, "Value found in empty database");

      Database.Insert (K => Cursor.Cursor, E => (43, 44));
      Cursor := Database.Search_Value (E => (43, 44));
      Assert (Cursor.Result = DB.Status_OK, "Value not found database");
      Assert (Database.Get (K => Cursor.Cursor) = (43, 44), "Invalid element for result of Search_Value");
   end Test_Search_Value;

   procedure Test_Search_Partial_Value (T : in out Aunit.Test_Cases.Test_Case'Class)
   is
      Database : DB.Database;
      Cursor   : DB.Option;

      function Match (L, R: Element) return Boolean is (L.Left = R.Left);
      function Search_Partial is new DB.Search (Match);
   begin
      Database.Initialize;

      Cursor := Database.Search_Value (E => (43, 44));
      Assert (Cursor.Result = DB.Status_Not_Found, "Value found in empty database");

      Database.Insert (K => Cursor.Cursor, E => (43, 44));
      Cursor := Search_Partial (Database, E => (43, 44));
      Assert (Cursor.Result = DB.Status_OK, "Value not found database");
      Assert (Database.Get (K => Cursor.Cursor) = (43, 44), "Invalid element for result of Search_Value");
   end Test_Search_Partial_Value;

   procedure Register_Tests (T : in out Test) is
      use AUnit.Test_Cases.Registration;
   begin
      Register_Routine (T, Test_Basic_Insert'Access, "Basic insert");
      Register_Routine (T, Test_Basic_Delete'Access, "Basic delete");
      Register_Routine (T, Test_Search_Value'Access, "Search value");
      Register_Routine (T, Test_Search_Partial_Value'Access, "Search partial value");
   end Register_Tests;

end Test_Unique_Map;
