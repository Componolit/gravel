
with Componolit.Gneiss.Block;

package Block with
   SPARK_Mode
is

   type Byte is mod 2 ** 8;
   subtype Unsigned_Long is Long_Integer range 0 .. Long_Integer'Last;
   type Buffer is array (Unsigned_Long range <>) of Byte;

   type Request_Id is mod 2 ** 6;
   subtype Session_Id is Boolean;

   package Types is new Componolit.Gneiss.Block (Byte, Unsigned_Long, Buffer, Session_Id, Request_Id);

end Block;
