with Parpen.DB;

generic
   type Element is private;
   type Query_Index is (<>);
   type Query_Element is (<>);
   type Query_String is array (Query_Index range <>) of Query_Element;
package Parpen.NameDB is

   type Database (Size : Natural) is tagged private;

   type Status is
      (Status_OK,
       Status_Not_Found,
       Status_In_Use,
       Status_Out_Of_Memory);

   type Result (Valid : Boolean := False) is
   record
      case Valid is
         when True =>
            Element : Parpen.NameDB.Element;
         when False =>
            Status  : Parpen.NameDB.Status;
      end case;
   end record;

   procedure Init (Database : out Parpen.NameDB.Database);

   procedure Add (Database : in out Parpen.NameDB.Database;
                  Element  :        Parpen.NameDB.Element;
                  Query    :        Query_String;
                  Status   :    out Parpen.NameDB.Status);

   function Exists (Database :     Parpen.NameDB.Database;
                    Query    :     Query_String) return Boolean;

   procedure Get (Database :     Parpen.NameDB.Database;
                  Query    :     Query_String;
                  Result   : out Parpen.NameDB.Result);

private

   type Byte is mod 2**8;
   subtype Hash_Index is Natural range 1 .. 20;
   type Hash_Type is array (Hash_Index) of Byte;

   package Name_DB is new Parpen.DB (Element => Element,
                                     Key     => Hash_Type);

   type Database (Size : Natural) is tagged
   record
      Inner : Name_DB.Database (Size);
   end record;

end Parpen.NameDB;
