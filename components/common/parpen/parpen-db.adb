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
      DB.E := (others => Null_Internal_Element);
   end Initialize;

   ----------
   -- Find --
   ----------

   function Find (DB : Database; K : Key) return Cursor_Option
   is
      Free       : Cursor;
      Free_Found : Boolean := False;
   begin
      for I in DB.E'Range
      loop
         if DB.E (I).V and then DB.E (I).K = K then
            return Cursor_Option'(Result => Status_OK, C => (C => I));
         end if;
         if not DB.E (I).V then
            Free := (C => I);
            Free_Found := True;
         end if;
      end loop;
      if Free_Found then
         return Cursor_Option'(Result => Status_Not_Found, C => Free);
      end if;
      return Cursor_Option'(Result => Status_Overflow);
   end Find;

   ---------
   -- Get --
   ---------

   function Get (DB : Database; C : Cursor) return Element is
   begin
      return DB.E (C.C).E;
   end Get;

   ------------
   -- Insert --
   ------------

   procedure Insert (DB : in out Database; C : Cursor; K : Key; E : Element) is
   begin
      DB.E (C.C) := Internal_Element'(V => True, K => K, E => E);
   end Insert;

   ------------
   -- Delete --
   ------------

   procedure Delete (DB : in out Database; C : Cursor) is
   begin
      DB.E (C.C) := Null_Internal_Element;
   end Delete;

end Parpen.DB;
