generic
   type Key is private;
   type Element is private;
package Parpen.DB with SPARK_Mode
is

   type Position is private;
   type Status is (Status_OK, Status_Not_Found, Status_Overflow);

   type Cursor_Option (Status : Parpen.DB.Status := Status_Overflow) is
   record
      case Status is
         when Status_OK | Status_Not_Found =>
            Cursor : Position;
         when Status_Overflow =>
            null;
      end case;
   end record;

   type Database (Size : Natural) is tagged private;

   function Initialized (Database : Parpen.DB.Database) return Boolean with Ghost;

   procedure Initialize (Database : out Parpen.DB.Database) with
      Post => Initialized (Database);

   function Find (Database : Parpen.DB.Database;
                  Key      : Parpen.DB.Key) return Cursor_Option with
      Pre => Initialized (Database);

   function Search_Value (Database : Parpen.DB.Database;
                          Element  : Parpen.DB.Element) return Cursor_Option with
      Pre => Initialized (Database);

   generic
      with function Match (Left, Right : Element) return Boolean;
   function Search (Database : Parpen.DB.Database;
                    Element  : Parpen.DB.Element) return Cursor_Option with
      Pre => Initialized (Database);

   function Get (Database : Parpen.DB.Database;
                 Position : Parpen.DB.Position) return Element with
      Pre => Initialized (Database);

   procedure Insert (Database : in out Parpen.DB.Database;
                     Position :        Parpen.DB.Position;
                     Key      :        Parpen.DB.Key;
                     Element  :        Parpen.DB.Element) with
      Pre => Initialized (Database);

   procedure Delete (Database : in out Parpen.DB.Database;
                     Position :        Parpen.DB.Position) with
      Pre => Initialized (Database);

private

   type Key_Option (Valid : Boolean := False) is record
      case Valid is
         when True =>
            Key : Parpen.DB.Key;
         when False =>
            null;
      end case;
   end record;

   type Element_Option (Valid : Boolean := False) is record
      case Valid is
         when True =>
            Element : Parpen.DB.Element;
         when False =>
            null;
      end case;
   end record;

   type Position is record
      Inner : Natural;
   end record;
   type Internal_Element is
   record
      Valid   : Boolean;
      Key     : Parpen.DB.Key_Option;
      Element : Parpen.DB.Element_Option;
   end record;
   Null_Internal_Element : constant Internal_Element := (False, (Valid => False), (Valid => False));

   type Element_Array is array (Natural range <>) of Internal_Element;
   type Database (Size : Natural) is tagged
   record
      Elements : Element_Array (1 .. Size);
   end record;
end Parpen.DB;
