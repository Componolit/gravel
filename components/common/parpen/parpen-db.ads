generic
   type Key is private;
   Null_Key : Key;
   type Element is private;
   Null_Element : Element;
package Parpen.DB with SPARK_Mode
is

   type Cursor is private;
   type Status is (Status_OK, Status_Not_Found, Status_Overflow);

   type Cursor_Option (Result : Status := Status_Overflow) is
   record
      case Result is
         when Status_OK | Status_Not_Found =>
            C : Cursor;
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

   function Get (DB : Database; C : Cursor) return Element with
      Pre => Initialized (DB);

   procedure Insert (DB : in out Database; C : Cursor; K : Key; E : Element) with
      Pre => Initialized (DB);

   procedure Delete (DB : in out Database; C : Cursor) with
      Pre => Initialized (DB);

private
   type Cursor is record
      C : Natural;
   end record;
   type Internal_Element is
   record
      V : Boolean;
      K : Key;
      E : Element;
   end record;
   Null_Internal_Element : constant Internal_Element := (False, Null_Key, Null_Element);
   
   type Elements is array (Natural range <>) of Internal_Element;
   type Database (Size : Natural) is tagged
   record
      E : Elements (1 .. Size);
   end record;
end Parpen.DB;
