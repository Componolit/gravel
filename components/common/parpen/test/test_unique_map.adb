with AUnit.Assertions; use AUnit.Assertions;
with Parpen.Unique_Map;

package body Test_Unique_Map is

   type Element is
   record
      Left  : Natural;
      Right : Natural;
   end record;

   type Key is new Natural range 0 .. 100;

   package DB is new Parpen.Unique_Map (Element => Element,
                                        Key     => Key);

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
      Database : DB.Database;
      Data     : DB.Option;
      New_Data : DB.Option;
   begin
      Database.Initialize;

      Data := Database.Get (Key => 14);
      Assert (Data.Status = DB.Status_Not_Found, "Element found in empty database");

      Data := (Status => DB.Status_Valid, Position => Data.Free, Data => (100, 200));
      Database.Insert (Data);
      Assert (Data.Status = DB.Status_Valid, "Result not valid after insert");

      Data := Database.Get (Key => 14);
      Assert (Data.Status = DB.Status_Valid, "Unexpected result");
      Assert (Data.Data = (100, 200), "Invalid element");

      New_Data := Database.Get (Key => 14);
      Assert (Data = New_Data, "Result differ between requests");

      Data := Database.Get (Key => 15);
      Assert (Data.Status = DB.Status_Not_Found, "Non-existing element found in database");
   end Test_Basic_Insert;

   procedure Test_Basic_Delete (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
      Database : DB.Database;
      Data     : DB.Option;
   begin
      Database.Initialize;

      Data := Database.Get (Key => 14);
      Assert (Data.Status = DB.Status_Not_Found, "Element found in empty database");

      Data := (Status => DB.Status_Valid, Position => Data.Free, Data => (100, 200));
      Database.Insert (Data);
      Data := Database.Get (Key => 14);
      Assert (Data.Status = DB.Status_Valid, "Unexpected element");
      Assert (Data.Data = (100, 200), "Invalid element");

      Database.Delete (Key => 14);
      Data := Database.Get (Key => 14);
      Assert (Data.Status = DB.Status_Not_Found, "Element found after deletion database");
   end Test_Basic_Delete;

   procedure Test_Search_Value (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
      Database : DB.Database;
      Data     : DB.Option;
   begin
      Database.Initialize;

      Data := Database.Find (Element => (43, 44));
      Assert (Data.Status = DB.Status_Not_Found, "Value found in empty database");

      Data := (Status => DB.Status_Valid, Position => Data.Free, Data => (43, 44));
      Database.Insert (Data);

      Data := Database.Find (Element => (43, 44));
      Assert (Data.Status = DB.Status_Valid, "Value not found database");
      Assert (Data.Data = (43, 44), "Invalid element for result of Search_Value");
   end Test_Search_Value;

   procedure Test_Search_Partial_Value (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
      Database : DB.Database;
      Data   : DB.Option;

      function Match (L, R : Element) return Boolean is (L.Left = R.Left);
      function Search_Partial is new DB.Generic_Find (Match);
   begin
      Database.Initialize;

      Data := Database.Find (Element => (43, 44));
      Assert (Data.Status = DB.Status_Not_Found, "Value found in empty database");

      Data := (Status => DB.Status_Valid, Position => Data.Free, Data => (43, 44));
      Database.Insert (Data);
      Data := Search_Partial (Database, Element => (43, 44));
      Assert (Data.Status = DB.Status_Valid, "Value not found database");
      Data := Database.Get (Key => Data.Position);
      Assert (Data.Status = DB.Status_Valid, "Value not found database");
      Assert (Data.Data = (43, 44), "Invalid element for result of Search_Value");
   end Test_Search_Partial_Value;

   procedure Test_Nested_Insert (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
      package Inner is new Parpen.Unique_Map (Element => Element,
                                              Key     => Key);

      package Outer is new Parpen.Unique_Map (Element => Inner.Database,
                                              Key     => Key);
      Outer_Result : Outer.Option;
      Database     : Outer.Database;
      Status       : Outer.Status;

      procedure Set_Value (DB     : in out Inner.Database;
                           Status :    out Outer.Status);
      procedure Set_Value (DB     : in out Inner.Database;
                           Status :    out Outer.Status)
      is
         Data : Inner.Option;
         use type Inner.Status;
      begin
         Data := DB.Get (Key => 11);
         Assert (Data.Status = Inner.Status_Not_Found, "Element found in empty database");
         Data := (Status => Inner.Status_Valid, Position => Data.Free, Data => (1234, 5678));
         DB.Insert (Data);
         Status := Outer.Status_Valid;
      end Set_Value;

      procedure Check_Value (DB     : in out Inner.Database;
                             Status :    out Outer.Status);
      procedure Check_Value (DB     : in out Inner.Database;
                             Status :    out Outer.Status)
      is
         Data : Inner.Option;
         use type Inner.Status;
      begin
         Data := DB.Get (Key => 11);
         Assert (Data.Status = Inner.Status_Valid, "Element not found");
         Data := DB.Get (Key => 11);
         Assert (Data.Status = Inner.Status_Valid, "Element not found");
         Assert (Data.Data = (1234, 5678), "Invalid value");
         Status := Outer.Status_Valid;
      end Check_Value;

      procedure Set_Value is new Outer.Generic_Apply (Operation => Set_Value);
      procedure Check_Value is new Outer.Generic_Apply (Operation => Check_Value);

      use type Outer.Status;
   begin
      Database.Initialize;

      Outer_Result := Database.Get (Key => 14);
      Assert (Outer_Result.Status = Outer.Status_Not_Found, "Element found in empty database");
      Outer_Result := (Status => Outer.Status_Valid, Position => Outer_Result.Free, Data => Inner.Null_DB);
      Database.Insert (Outer_Result);

      Set_Value (Database => Database, Key => 14, Status => Status);
      Assert (Status = Outer.Status_Valid, "Error setting value: " & Status'Img);
      Check_Value (Database => Database, Key => 14, Status => Status);
      Assert (Status = Outer.Status_Valid, "Error checking value: " & Status'Img);
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
