with AUnit.Assertions; use AUnit.Assertions;
with Parpen.DB;

package body Test_DB is

   package DB is new Parpen.DB (Element      => Natural,
                                Null_Element => 0,
                                Key          => Natural,
                                Null_Key     => 0);

   use type DB.Status;
   use type DB.Curs;
   use type DB.Cursor_Option;

   function Name (T : Test) return AUnit.Message_String is
      pragma Unreferenced (T);
   begin
      return AUnit.Format ("Parpen database");
   end Name;

   procedure Test_Basic_Insert (T : in out Aunit.Test_Cases.Test_Case'Class)
   is
      Database   : DB.Database (100);
      Cursor     : DB.Cursor_Option;
      New_Cursor : DB.Cursor_Option;
   begin
      Database.Initialize;

      Cursor := Database.Find (K => 14);
      Assert (Cursor.Result = DB.Status_Not_Found, "Element found in empty database");

      Database.Insert (C => Cursor.Cursor, K => 14, E => 100);
      Assert (Database.Get (C => Cursor.Cursor) = 100, "Invalid element");

      Cursor := Database.Find (K => 14);
      Assert (Cursor.Result = DB.Status_OK, "Element not found in database");

      New_Cursor := Database.Find (K => 14);
      Assert (New_Cursor = Cursor, "Cursor differ between requests");

      Cursor := Database.Find (K => 15);
      Assert (Cursor.Result = DB.Status_Not_Found, "Non-existing element found in database");
   end Test_Basic_Insert;

   procedure Test_Basic_Delete (T : in out Aunit.Test_Cases.Test_Case'Class)
   is
      Database : DB.Database (100);
      Cursor   : DB.Cursor_Option;
   begin
      Database.Initialize;

      Cursor := Database.Find (K => 14);
      Assert (Cursor.Result = DB.Status_Not_Found, "Element found in empty database");

      Database.Insert (C => Cursor.Cursor, K => 14, E => 100);
      Assert (Database.Get (C => Cursor.Cursor) = 100, "Invalid element");

      Database.Delete (C => Cursor.Cursor);
      Database.Insert (C => Cursor.Cursor, K => 15, E => 200);
      Assert (Database.Get (C => Cursor.Cursor) = 200, "Invalid element after deletion");

      Cursor := Database.Find (K => 14);
      Assert (Cursor.Result = DB.Status_Not_Found, "Element found after deletion database");
   end Test_Basic_Delete;

   procedure Test_Overflow (T : in out Aunit.Test_Cases.Test_Case'Class)
   is
      Database : DB.Database (73);
      Cursor   : DB.Cursor_Option;
   begin
      Database.Initialize;
      for I in Natural range 1 .. 73 loop
         Cursor := Database.Find (K => I);
         Assert (Cursor.Result = DB.Status_Not_Found, "Element" & I'Img & " found in empty database");
         Database.Insert (C => Cursor.Cursor, K => I, E => I);
         Assert (Database.Get (C => Cursor.Cursor) = I, "Invalid element for iteration" & I'Img);
      end loop;

      Cursor := Database.Find (K => 74);
      Assert (Cursor.Result = DB.Status_Overflow, "Overflow not detected");
   end Test_Overflow;

   procedure Register_Tests (T : in out Test) is
      use AUnit.Test_Cases.Registration;
   begin
      Register_Routine (T, Test_Basic_Insert'Access, "Basic insert");
      Register_Routine (T, Test_Basic_Delete'Access, "Basic delete");
      Register_Routine (T, Test_Overflow'Access, "Overflow");
   end Register_Tests;

end Test_DB;
