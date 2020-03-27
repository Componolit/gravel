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

   procedure Init (Database : out Parpen.NameDB.Database)
   is
   begin
      Database.Inner.Initialize;
   end Init;

   ---------
   -- Add --
   ---------

   procedure Add (Database : in out Parpen.NameDB.Database;
                  Element  :        Parpen.NameDB.Element;
                  Query    :        Query_String;
                  Status   :    out Parpen.NameDB.Status)
   is
      Hash   : constant Hash_Type := Parpen.NameDB.Hash (Query);
      Cursor : Name_DB.Cursor_Option;
   begin
      Cursor := Database.Inner.Find (Hash);
      case Cursor.Status is
         when Name_DB.Status_OK =>
            Status := Status_In_Use;
         when Name_DB.Status_Overflow =>
            Status := Status_Out_Of_Memory;
         when Name_DB.Status_Not_Found =>
            Database.Inner.Insert (Position => Cursor.Cursor,
                                   Key      => Hash,
                                   Element  => Element);
            Status := Status_OK;
      end case;
   end Add;

   ------------
   -- Exists --
   ------------

   function Exists (Database : Parpen.NameDB.Database;
                    Query    : Query_String) return Boolean
   is
      Hash   : constant Hash_Type := Parpen.NameDB.Hash (Query);
      Cursor : Name_DB.Cursor_Option;
      use type Name_DB.Status;
   begin
      Cursor := Database.Inner.Find (Hash);
      if Cursor.Status = Name_DB.Status_OK then
         return True;
      end if;
      return False;
   end Exists;

   ---------
   -- Get --
   ---------

   procedure Get (Database :        Parpen.NameDB.Database;
                  Query    :        Query_String;
                  Result   :    out Parpen.NameDB.Result)
   is
      Hash   : constant Hash_Type := Parpen.NameDB.Hash (Query);
      Cursor : Name_DB.Cursor_Option;
      use type Name_DB.Status;
   begin
      Cursor := Database.Inner.Find (Hash);
      if Cursor.Status = Name_DB.Status_OK then
         Result := (Valid   => True,
                    Element => Database.Inner.Get (Cursor.Cursor));
         return;
      end if;
      Result := (Valid  => False,
                 Status => Status_Not_Found);
   end Get;

end Parpen.NameDB;
