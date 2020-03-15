package body Parpen.Unique_Map is

   generic
      with function Match (E : Internal_Element) return Boolean;
   function Search_Internal (DB : Database) return Option;

   function Search_Internal (DB : Database) return Option
   is
      Free       : Key;
      Free_Found : Boolean := False;
   begin
      for I in DB.Elements'Range
      loop
         if Match (DB.Elements (I)) then
            return Option'(Result => Status_OK, Cursor => I);
         end if;
         if not DB.Elements (I).Valid then
            Free := I;
            Free_Found := True;
         end if;
      end loop;
      if Free_Found then
         return Option'(Result => Status_Not_Found, Cursor => Free);
      end if;
      return Option'(Result => Status_Invalid);
   end Search_Internal;

   -----------------
   -- Initialized --
   -----------------

   function Initialized (DB : Database) return Boolean with
      SPARK_Mode => Off
   is
      pragma Unreferenced (DB);
   begin
      return False;
   end Initialized;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (DB : out Database) is
   begin
      DB.Elements := (others => Null_Internal_Element);
   end Initialize;

   ----------
   -- Find --
   ----------

   function Find (DB : Database; K : Key) return Option
   is
   begin
      return (if DB.Elements (K).Valid
              then (Result => Status_OK, Cursor => K)
              else (Result => Status_Not_Found, Cursor => K));
   end Find;

   ------------------
   -- Search_Value --
   ------------------

   function Search_Value (DB : Database; E : Element) return Option
   is
      function Match (Current : Internal_Element) return Boolean is (Current.Valid and Current.Elem = E);
      function Search_Equal is new Search_Internal (Match);
   begin
      return Search_Equal (DB);
   end Search_Value;

   ------------
   -- Search --
   ------------

   function Search (DB : Database; E : Element) return Option
   is
      function Match_Internal (Current : Internal_Element) return Boolean is
         (Current.Valid and Match (Current.Elem, E));
      function Search_Match is new Search_Internal (Match_Internal);
   begin
      return Search_Match (DB);
   end Search;

   ---------
   -- Get --
   ---------

   function Get (DB : Database; K : Key) return Element is
   begin
      return DB.Elements (K).Elem;
   end Get;

   -----------
   -- Apply --
   -----------

   procedure Apply (DB : in out Database; K : Key) is
   begin
      Operation (DB.Elements (K).Elem);
   end Apply;

   ------------
   -- Insert --
   ------------

   procedure Insert (DB : in out Database; K : Key; E : Element) is
   begin
      DB.Elements (K) := Internal_Element'(Valid => True, Elem => E);
   end Insert;

   ------------
   -- Delete --
   ------------

   procedure Delete (DB : in out Database; K : Key) is
   begin
      DB.Elements (K) := Null_Internal_Element;
   end Delete;

end Parpen.Unique_Map;
