package body Parpen.Unique_Map is

   generic
      with function Match (E : Internal_Element) return Boolean;
   function Internal_Find (Database : Parpen.Unique_Map.Database) return Option;

   function Internal_Find (Database : Parpen.Unique_Map.Database) return Option
   is
      Free       : Key;
      Free_Found : Boolean := False;
   begin
      for I in Database.Elements'Range
      loop
         if Match (Database.Elements (I)) then
            return Option'(Status   => Status_Valid,
                           Data     => Database.Elements (I).Element.Element,
                           Position => I);
         end if;
         if not Database.Elements (I).Valid and not Free_Found then
            Free := I;
            Free_Found := True;
         end if;
      end loop;
      if Free_Found then
         return Option'(Status => Status_Not_Found,
                        Free   => Free);
      end if;
      return Option'(Status => Status_Invalid);
   end Internal_Find;

   -----------------
   -- Initialized --
   -----------------

   function Initialized (Database : Parpen.Unique_Map.Database) return Boolean with
      SPARK_Mode => Off
   is
      pragma Unreferenced (Database);
   begin
      return False;
   end Initialized;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Database : out Parpen.Unique_Map.Database) is
   begin
      Database.Elements := (others => Null_Internal_Element);
   end Initialize;

   ---------
   -- Get --
   ---------

   function Get (Database : Parpen.Unique_Map.Database;
                 Key      : Parpen.Unique_Map.Key) return Option
   is
   begin
      return (if Database.Elements (Key).Valid
              then (Status   => Status_Valid,
                    Data     => Database.Elements (Key).Element.Element,
                    Position => Key)
              else (Status => Status_Not_Found,
                    Free    => Key));
   end Get;

   ----------
   -- Find --
   ----------

   function Find (Database : Parpen.Unique_Map.Database;
                  Element  : Parpen.Unique_Map.Element) return Option
   is
      function Match (Current : Internal_Element) return Boolean is
         (Current.Valid
          and then Current.Element.Valid
          and then Current.Element.Element = Element);
      function Search_Equal is new Internal_Find (Match);
   begin
      return Search_Equal (Database);
   end Find;

   ------------------
   -- Generic_Find --
   ------------------

   function Generic_Find (Database : Parpen.Unique_Map.Database;
                          Element  : Parpen.Unique_Map.Element) return Option
   is
      function Match_Internal (Current : Internal_Element) return Boolean is
         (Current.Valid
          and then Current.Element.Valid
          and then Match (Current.Element.Element, Element));
      function Search_Match is new Internal_Find (Match_Internal);
   begin
      return Search_Match (Database);
   end Generic_Find;

   -------------------
   -- Generic_Apply --
   -------------------

   procedure Generic_Apply (Database : in out Parpen.Unique_Map.Database;
                            Key      :        Parpen.Unique_Map.Key) is
   begin
      Operation (Database.Elements (Key).Element.Element);
   end Generic_Apply;

   ------------
   -- Insert --
   ------------

   procedure Insert (Database : in out Parpen.Unique_Map.Database;
                     Element  : in out Option) is
   begin
      Database.Elements (Element.Position) := (Valid   => True,
                                               Element => (Valid   => True,
                                                           Element => Element.Data));
      Element := (Status   => Status_Valid,
                  Position => Element.Position,
                  Data     => Element.Data);
   end Insert;

   ------------
   -- Delete --
   ------------

   procedure Delete (Database : in out Parpen.Unique_Map.Database;
                     Key      :        Parpen.Unique_Map.Key) is
   begin
      Database.Elements (Key) := Null_Internal_Element;
   end Delete;

end Parpen.Unique_Map;
