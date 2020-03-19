with LSC.SHA1_Generic;

package body Parpen.NameDB is

   --  We use SHA-1 for indexing only.
   function Hash is new LSC.SHA1_Generic.Hash
      (Message_Index_Type => Query_Index,
       Message_Elem_type  => Query_Element,
       Message_Type       => Query_String,
       Hash_Index_Type    => Hash_Index,
       Hash_Elem_type     => Byte,
       Hash_type          => Hash_Type);

   ----------
   -- Init --
   ----------

   procedure Init (DB : out Database)
   is
   begin
      DB.Inner.Initialize;
   end Init;

   ---------
   -- Add --
   ---------

   procedure Add
     (DB : in out Database; Elem : Element; Query : Query_String; Result : out Status)
   is
      H : constant Hash_Type := Hash (Query);
      C : Name_DB.Cursor_Option;
   begin
      C := DB.Inner.Find (H);
      case C.State is
         when Name_DB.Status_OK =>
            Result := Status_In_Use;
         when Name_DB.Status_Overflow =>
            Result := Status_Out_Of_Memory;
         when Name_DB.Status_Not_Found =>
            DB.Inner.Insert (C => C.Cursor, K => H, E => Elem);
            Result := Status_OK;
      end case;
   end Add;

   ------------
   -- Exists --
   ------------

   function Exists (DB : Database; Query : Query_String) return Boolean
   is
      H : constant Hash_Type := Hash (Query);
      C : Name_DB.Cursor_Option;
      use type Name_DB.Status;
   begin
      C := DB.Inner.Find (H);
      if C.State = Name_DB.Status_OK then
         return True;
      end if;
      return False;
   end Exists;

   ---------
   -- Get --
   ---------

   procedure Get (DB : Database; Query : Query_String; Res : out Result)
   is
      H : constant Hash_Type := Hash (Query);
      C : Name_DB.Cursor_Option;
      use type Name_DB.Status;
   begin
      C := DB.Inner.Find (H);
      if C.State = Name_DB.Status_OK then
         Res := (Valid => True, Elem => DB.Inner.Get (C.Cursor));
         return;
      end if;
      Res := (Valid => False, Stat => Status_Not_Found);
   end Get;

end Parpen.NameDB;
