generic
   type Key is (<>);
   type Element is private;
   Null_Element : Element;
package Parpen.Unique_Map with SPARK_Mode
is

   type Status is (Status_OK, Status_Not_Found, Status_Invalid);

   type Option (Result : Status := Status_Invalid) is
   record
      case Result is
         when Status_OK | Status_Not_Found =>
            Data     : Element;
            Position : Key;
         when Status_Invalid =>
            null;
      end case;
   end record;

   type Database is tagged private;
   Null_DB : constant Database;

   function Initialized (DB : Database) return Boolean with Ghost;

   procedure Initialize (DB : out Database) with
      Post => Initialized (DB);

   function Get (DB : Database; K : Key) return Option with
      Pre => Initialized (DB);

   function Find (DB : Database; E : Element) return Option with
      Pre => Initialized (DB);

   generic
      with function Match (Left, Right : Element) return Boolean;
   function Generic_Find (DB : Database; E : Element) return Option with
      Pre => Initialized (DB);

   generic
      with procedure Operation (E : in out Element);
   procedure Generic_Apply (DB : in out Database; K : Key) with
      Pre => Initialized (DB);

   procedure Insert (DB : in out Database; E : in out Option) with
      Pre => Initialized (DB)
             and E.Result = Status_Not_Found;

   procedure Delete (DB : in out Database; K : Key) with
      Pre => Initialized (DB);

private
   type Internal_Element is
   record
      Valid : Boolean;
      Elem  : Element;
   end record;
   Null_Internal_Element : constant Internal_Element := (False, Null_Element);

   type Element_Array is array (Key) of Internal_Element;
   type Database is tagged
   record
      Elements : Element_Array;
   end record;
   Null_DB : constant Database := (Elements => (others => Null_Internal_Element));

end Parpen.Unique_Map;
