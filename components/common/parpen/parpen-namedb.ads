with Parpen.DB;

generic
   type Element is private;
   Null_Element : Element;
   type Query_Index is (<>);
   type Query_Element is (<>);
   type Query_String is array (Query_Index range <>) of Query_Element;
package Parpen.NameDB is

   type Database (Size : Natural) is tagged private;

   type Status is (Status_OK, Status_Not_Found, Status_In_Use, Status_Out_Of_Memory);

   type Result (Valid : Boolean := False) is
   record
      case Valid is
         when True =>
            Elem : Element;
         when False =>
            Stat : Status;
      end case;
   end record;

   procedure Init (DB : out Database);

   procedure Add (DB     : in out Database;
                  Elem   :        Element;
                  Query  :        Query_String;
                  Result :    out Status);

   function Exists (DB    :     Database;
                    Query :     Query_String) return Boolean;

   procedure Get (DB    :     Database;
                  Query :     Query_String;
                  Res   : out Result);

private

   type Byte is mod 2**8;
   subtype Hash_Index is Natural range 1 .. 20;
   type Hash_Type is array (Hash_Index) of Byte;

   package Name_DB is new Parpen.DB (Element      => Element,
                                     Null_Element => Null_Element,
                                     Key          => Hash_Type,
                                     Null_Key     => Hash_Type'(others => 0));

   type Database (Size : Natural) is tagged
   record
      Inner : Name_DB.Database (Size);
   end record;

end Parpen.NameDB;
