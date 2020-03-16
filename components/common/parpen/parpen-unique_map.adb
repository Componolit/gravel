package body Parpen.Unique_Map is

   generic
      with function Match (E : Internal_Element) return Boolean;
   function Internal_Find (DB : Database) return Option;

   function Internal_Find (DB : Database) return Option
   is
      Free       : Key;
      Free_Found : Boolean := False;
   begin
      for I in DB.Elements'Range
      loop
         if Match (DB.Elements (I)) then
            return Option'(Result => Status_OK, Data => DB.Elements (I).Elem);
         end if;
         if not DB.Elements (I).Valid and not Free_Found then
            Free := I;
            Free_Found := True;
         end if;
      end loop;
      if Free_Found then
         return Option'(Result => Status_Not_Found, Position => Free);
      end if;
      return Option'(Result => Status_Invalid);
   end Internal_Find;

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

   ---------
   -- Get --
   ---------

   function Get (DB : Database; K : Key) return Option
   is
   begin
      return (if DB.Elements (K).Valid
              then (Result => Status_OK, Data => DB.Elements (K).Elem)
              else (Result => Status_Not_Found, Position => K));
   end Get;

   ----------
   -- Find --
   ----------

   function Find (DB : Database; E : Element) return Option
   is
      function Match (Current : Internal_Element) return Boolean is (Current.Valid and Current.Elem = E);
      function Search_Equal is new Internal_Find (Match);
   begin
      return Search_Equal (DB);
   end Find;

   ------------------
   -- Generic_Find --
   ------------------

   function Generic_Find (DB : Database; E : Element) return Option
   is
      function Match_Internal (Current : Internal_Element) return Boolean is
         (Current.Valid and Match (Current.Elem, E));
      function Search_Match is new Internal_Find (Match_Internal);
   begin
      return Search_Match (DB);
   end Generic_Find;

   -------------------
   -- Generic_Apply --
   -------------------

   procedure Generic_Apply (DB : in out Database; K : Key) is
   begin
      Operation (DB.Elements (K).Elem);
   end Generic_Apply;

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
