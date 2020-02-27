package body Parpen.DB is

   -----------------
   -- Initialized --
   -----------------

   function Initialized (DB : Database) return Boolean with
      SPARK_Mode => Off
   is
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

   function Find (DB : Database; K : Key) return Cursor_Option
   is
      Free       : Curs;
      Free_Found : Boolean := False;
   begin
      for I in DB.Elements'Range
      loop
         if DB.Elements (I).Valid and then DB.Elements (I).Kee = K then
            return Cursor_Option'(Result => Status_OK, Cursor => (Inner => I));
         end if;
         if not DB.Elements (I).Valid then
            Free := (Inner => I);
            Free_Found := True;
         end if;
      end loop;
      if Free_Found then
         return Cursor_Option'(Result => Status_Not_Found, Cursor => Free);
      end if;
      return Cursor_Option'(Result => Status_Overflow);
   end Find;

   ---------
   -- Get --
   ---------

   function Get (DB : Database; C : Curs) return Element is
   begin
      return DB.Elements (C.Inner).Elem;
   end Get;

   ------------
   -- Insert --
   ------------

   procedure Insert (DB : in out Database; C : Curs; K : Key; E : Element) is
   begin
      DB.Elements (C.Inner) := Internal_Element'(Valid => True, Kee => K, Elem => E);
   end Insert;

   ------------
   -- Delete --
   ------------

   procedure Delete (DB : in out Database; C : Curs) is
   begin
      DB.Elements (C.Inner) := Null_Internal_Element;
   end Delete;

end Parpen.DB;
