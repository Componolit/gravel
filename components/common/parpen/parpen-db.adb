package body Parpen.DB is

   generic
      with function Match (E : Internal_Element) return Boolean;
   function Search_Internal (Database : Parpen.DB.Database) return Cursor_Option;

   function Search_Internal (Database : Parpen.DB.Database) return Cursor_Option
   is
      Free       : Position;
      Free_Found : Boolean := False;
   begin
      for I in Database.Elements'Range
      loop
         if Match (Database.Elements (I)) then
            return Cursor_Option'(Status => Status_OK,
                                  Cursor => (Inner => I));
         end if;
         if not Database.Elements (I).Valid then
            Free := (Inner => I);
            Free_Found := True;
         end if;
      end loop;
      if Free_Found then
         return Cursor_Option'(Status => Status_Not_Found,
                               Cursor => Free);
      end if;
      return Cursor_Option'(Status => Status_Overflow);
   end Search_Internal;

   -----------------
   -- Initialized --
   -----------------

   function Initialized (Database : Parpen.DB.Database) return Boolean with
      SPARK_Mode => Off
   is
      pragma Unreferenced (Database);
   begin
      return False;
   end Initialized;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Database : out Parpen.DB.Database) is
   begin
      Database.Elements := (others => Null_Internal_Element);
   end Initialize;

   ----------
   -- Find --
   ----------

   function Find (Database : Parpen.DB.Database;
                  Key      : Parpen.DB.Key) return Cursor_Option
   is
      function Match (Current : Internal_Element) return Boolean is
         (Current.Valid and then Current.Key.Valid and then Current.Key.Key = Key);
      function Search_Key is new Search_Internal (Match);
   begin
      return Search_Key (Database);
   end Find;

   ------------------
   -- Search_Value --
   ------------------

   function Search_Value (Database : Parpen.DB.Database;
                          Element  : Parpen.DB.Element) return Cursor_Option
   is
      function Match (Current : Internal_Element) return Boolean is
         (Current.Valid and then Current.Element.Valid and then Current.Element.Element = Element);
      function Search_Equal is new Search_Internal (Match);
   begin
      return Search_Equal (Database);
   end Search_Value;

   ------------
   -- Search --
   ------------

   function Search (Database : Parpen.DB.Database;
                    Element  : Parpen.DB.Element) return Cursor_Option
   is
      function Match_Internal (Current : Internal_Element) return Boolean is
         (Current.Valid and then Current.Element.Valid and then Match (Current.Element.Element, Element));
      function Search_Match is new Search_Internal (Match_Internal);
   begin
      return Search_Match (Database);
   end Search;

   ---------
   -- Get --
   ---------

   function Get (Database : Parpen.DB.Database;
                 Position : Parpen.DB.Position) return Element
   is
   begin
      return Database.Elements (Position.Inner).Element.Element;
   end Get;

   ------------
   -- Insert --
   ------------

   procedure Insert (Database : in out Parpen.DB.Database;
                     Position :        Parpen.DB.Position;
                     Key      :        Parpen.DB.Key;
                     Element  :        Parpen.DB.Element)
   is
   begin
      Database.Elements (Position.Inner) := Internal_Element'(Valid   => True,
                                                              Key     => (Valid => True, Key     => Key),
                                                              Element => (Valid => True, Element => Element));
   end Insert;

   ------------
   -- Delete --
   ------------

   procedure Delete (Database : in out Parpen.DB.Database;
                     Position :        Parpen.DB.Position) is
   begin
      Database.Elements (Position.Inner) := Null_Internal_Element;
   end Delete;

end Parpen.DB;
