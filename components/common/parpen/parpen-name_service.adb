with LSC.SHA1_Generic;

package body Parpen.Name_Service is

   --  We use SHA-1 for indexing only.
   function Hash is new LSC.SHA1_Generic.Hash
      (Message_Index_Type => Positive,
       Message_Elem_type  => Character,
       Message_Type       => String,
       Hash_Index_Type    => Hash_Index,
       Hash_Elem_type     => Byte,
       Hash_type          => Hash_Type);

   ----------
   -- Init --
   ----------

   procedure Init (DB : out Database)
   is
   begin
      for E of DB.E
      loop
         E := Null_Internal_Element;
      end loop;
   end Init;

   ---------
   -- Add --
   ---------

   procedure Add
     (DB : in out Database; Elem : Element; Name : String; Result : out Status)
   is
      H          : Hash_Type := Hash (Name);
      Slot       : Natural;
      Slot_Found : Boolean := False;
   begin
      for I in DB.E'Range
      loop
         if DB.E (I).Valid and then DB.E (I).Hash = H then
            Result := Status_In_Use;
            return;
         end if;

         if not DB.E (I).Valid and not Slot_Found then
            Slot       := I;
            Slot_Found := True;
         end if;
      end loop;

      if not Slot_Found then
         Result := Status_Out_Of_Memory;
         return;
      end if;

      DB.E (Slot) := (Valid => True,
                      Hash  => H,
                      Elem  => Elem);
      Result := Status_OK;
   end Add;

   ------------
   -- Exists --
   ------------

   function Exists (DB : Database; Name : String) return Boolean
   is
      H : Hash_Type := Hash (Name);
   begin
      for E of DB.E
      loop
         if E.Valid and then E.Hash = H then
            return True;
         end if;
      end loop;
      return False;
   end Exists;

   ---------
   -- Get --
   ---------

   procedure Get (DB : Database; Name : String; Res : out Result)
   is
      H : Hash_Type := Hash (Name);
   begin
      for E of DB.E
      loop
         if E.Valid and then E.Hash = H then
            Res := (Valid => True, Elem => E.Elem);
            return;
         end if;
      end loop;
      Res := (Valid => False, Stat => Status_Not_Found);
   end Get;

end Parpen.Name_Service;
