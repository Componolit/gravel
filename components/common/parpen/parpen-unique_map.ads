generic
   type Key is (<>);
   type Element is private;
package Parpen.Unique_Map with SPARK_Mode
is

   type Status is
      (Status_Valid,
       Status_Not_Found,
       Status_Invalid);

   type Option (Status : Parpen.Unique_Map.Status := Status_Invalid) is
   record
      case Status is
         when Status_Valid =>
            Data     : Element;
            Position : Key;
         when Status_Not_Found =>
            Free     : Key;
         when Status_Invalid =>
            null;
      end case;
   end record;

   type Database is tagged private;
   Null_DB : constant Database;

   function Initialized (Database : Parpen.Unique_Map.Database) return Boolean with Ghost;

   procedure Initialize (Database : out Parpen.Unique_Map.Database) with
      Post => Initialized (Database);

   function Get (Database : Parpen.Unique_Map.Database;
                 Key      : Parpen.Unique_Map.Key) return Option with
      Pre => Initialized (Database);

   function Find (Database : Parpen.Unique_Map.Database;
                  Element  : Parpen.Unique_Map.Element) return Option with
      Pre => Initialized (Database);

   generic
      with function Match (Left, Right : Element) return Boolean;
   function Generic_Find (Database : Parpen.Unique_Map.Database;
                          Element  : Parpen.Unique_Map.Element) return Option with
      Pre => Initialized (Database);

   generic
      with procedure Operation (E : in out Element);
   procedure Generic_Apply (Database : in out Parpen.Unique_Map.Database;
                            Key      :        Parpen.Unique_Map.Key) with
      Pre => Initialized (Database);

   procedure Insert (Database : in out Parpen.Unique_Map.Database;
                     Element  : in out Parpen.Unique_Map.Option) with
      Pre => Initialized (Database)
             and Element.Status = Status_Valid;

   procedure Delete (Database : in out Parpen.Unique_Map.Database;
                     Key      :        Parpen.Unique_Map.Key) with
      Pre => Initialized (Database);

private

   type Element_Option (Valid : Boolean := False) is record
      case Valid is
         when True =>
            Element : Parpen.Unique_Map.Element;
         when False =>
            null;
      end case;
   end record;

   type Internal_Element is
   record
      Valid   : Boolean;
      Element : Element_Option;
   end record;
   Null_Internal_Element : constant Internal_Element := (False, (Valid => False));

   type Element_Array is array (Key) of Internal_Element;
   type Database is tagged
   record
      Elements : Element_Array;
   end record;
   Null_DB : constant Database := (Elements => (others => Null_Internal_Element));

end Parpen.Unique_Map;
