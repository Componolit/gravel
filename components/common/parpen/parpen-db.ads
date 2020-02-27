generic
   type Key is private;
   Null_Key : Key;
   type Element is private;
   Null_Element : Element;
package Parpen.DB with SPARK_Mode
is

   type Curs is private;
   type Status is (Status_OK, Status_Not_Found, Status_Overflow);

   type Cursor_Option (Result : Status := Status_Overflow) is
   record
      case Result is
         when Status_OK | Status_Not_Found =>
            Cursor : Curs;
         when Status_Overflow =>
            null;
      end case;
   end record;

   type Database (Size : Natural) is tagged private;

   function Initialized (DB : Database) return Boolean with Ghost;

   procedure Initialize (DB : out Database) with
      Post => Initialized (DB);

   function Find (DB : Database; K : Key) return Cursor_Option with
      Pre => Initialized (DB);

   function Get (DB : Database; C : Curs) return Element with
      Pre => Initialized (DB);

   procedure Insert (DB : in out Database; C : Curs; K : Key; E : Element) with
      Pre => Initialized (DB);

   procedure Delete (DB : in out Database; C : Curs) with
      Pre => Initialized (DB);

private
   type Curs is record
      Inner : Natural;
   end record;
   type Internal_Element is
   record
      Valid : Boolean;
      Kee   : Key;
      Elem  : Element;
   end record;
   Null_Internal_Element : constant Internal_Element := (False, Null_Key, Null_Element);
   
   type Element_Array is array (Natural range <>) of Internal_Element;
   type Database (Size : Natural) is tagged
   record
      Elements : Element_Array (1 .. Size);
   end record;
end Parpen.DB;
