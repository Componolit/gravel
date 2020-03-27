with AUnit.Assertions; use AUnit.Assertions;
with Parpen.DB;

package body Test_DB is

   type Element is
   record
      Left  : Natural;
      Right : Natural;
   end record;

   package DB is new Parpen.DB (Element => Element,
                                Key     => Natural);

   use type DB.Status;
   use type DB.Cursor_Option;

   function Name (T : Test) return AUnit.Message_String is
      pragma Unreferenced (T);
   begin
      return AUnit.Format ("Parpen database");
   end Name;

   procedure Test_Basic_Insert (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
      Database   : DB.Database (100);
      Cursor     : DB.Cursor_Option;
      New_Cursor : DB.Cursor_Option;
   begin
      Database.Initialize;

      Cursor := Database.Find (Key => 14);
      Assert (Cursor.Status = DB.Status_Not_Found, "Element found in empty database");

      Database.Insert (Position => Cursor.Cursor, Key => 14, Element => (100, 200));
      Assert (Database.Get (Position => Cursor.Cursor) = (100, 200), "Invalid element");

      Cursor := Database.Find (Key => 14);
      Assert (Cursor.Status = DB.Status_OK, "Element not found in database");

      New_Cursor := Database.Find (Key => 14);
      Assert (New_Cursor = Cursor, "Cursor differ between requests");

      Cursor := Database.Find (Key => 15);
      Assert (Cursor.Status = DB.Status_Not_Found, "Non-existing element found in database");
   end Test_Basic_Insert;

   procedure Test_Basic_Delete (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
      Database : DB.Database (100);
      Cursor   : DB.Cursor_Option;
   begin
      Database.Initialize;

      Cursor := Database.Find (Key => 14);
      Assert (Cursor.Status = DB.Status_Not_Found, "Element found in empty database");

      Database.Insert (Position => Cursor.Cursor, Key => 14, Element => (100, 200));
      Assert (Database.Get (Position => Cursor.Cursor) = (100, 200), "Invalid element");

      Database.Delete (Position => Cursor.Cursor);
      Database.Insert (Position => Cursor.Cursor, Key => 15, Element => (200, 400));
      Assert (Database.Get (Position => Cursor.Cursor) = (200, 400), "Invalid element after deletion");

      Cursor := Database.Find (Key => 14);
      Assert (Cursor.Status = DB.Status_Not_Found, "Element found after deletion database");
   end Test_Basic_Delete;

   procedure Test_Overflow (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
      Database : DB.Database (73);
      Cursor   : DB.Cursor_Option;
   begin
      Database.Initialize;
      for I in Natural range 1 .. 73 loop
         Cursor := Database.Find (Key => I);
         Assert (Cursor.Status = DB.Status_Not_Found, "Element" & I'Img & " found in empty database");
         Database.Insert (Position => Cursor.Cursor, Key => I, Element => (I, I + 1));
         Assert (Database.Get (Position => Cursor.Cursor) = (I, I + 1), "Invalid element for iteration" & I'Img);
      end loop;

      Cursor := Database.Find (Key => 74);
      Assert (Cursor.Status = DB.Status_Overflow, "Overflow not detected");
   end Test_Overflow;

   procedure Test_Search_Value (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
      Database : DB.Database (20);
      Cursor   : DB.Cursor_Option;
   begin
      Database.Initialize;

      Cursor := Database.Search_Value (Element => (43, 44));
      Assert (Cursor.Status = DB.Status_Not_Found, "Value found in empty database");

      Database.Insert (Position => Cursor.Cursor, Key => 15, Element => (43, 44));
      Cursor := Database.Search_Value (Element => (43, 44));
      Assert (Cursor.Status = DB.Status_OK, "Value not found database");
      Assert (Database.Get (Position => Cursor.Cursor) = (43, 44), "Invalid element for result of Search_Value");
   end Test_Search_Value;

   procedure Test_Search_Partial_Value (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
      Database : DB.Database (20);
      Cursor   : DB.Cursor_Option;

      function Match (L, R : Element) return Boolean is (L.Left = R.Left);
      function Search_Partial is new DB.Search (Match);
   begin
      Database.Initialize;

      Cursor := Database.Search_Value (Element => (43, 44));
      Assert (Cursor.Status = DB.Status_Not_Found, "Value found in empty database");

      Database.Insert (Position => Cursor.Cursor, Key => 15, Element => (43, 44));
      Cursor := Search_Partial (Database, Element => (43, 44));
      Assert (Cursor.Status = DB.Status_OK, "Value not found database");
      Assert (Database.Get (Position => Cursor.Cursor) = (43, 44), "Invalid element for result of Search_Value");
   end Test_Search_Partial_Value;

   procedure Register_Tests (T : in out Test) is
      use AUnit.Test_Cases.Registration;
   begin
      Register_Routine (T, Test_Basic_Insert'Access, "Basic insert");
      Register_Routine (T, Test_Basic_Delete'Access, "Basic delete");
      Register_Routine (T, Test_Overflow'Access, "Overflow");
      Register_Routine (T, Test_Search_Value'Access, "Search value");
      Register_Routine (T, Test_Search_Partial_Value'Access, "Search partial value");
   end Register_Tests;

end Test_DB;
