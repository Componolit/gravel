
with Componolit.Interfaces.Block;

package Block with
   SPARK_Mode
is

   type Byte is mod 2 ** 8;
   subtype Unsigned_Long is Long_Integer range 0 .. Long_Integer'Last;
   type Buffer is array (Unsigned_Long range <>) of Byte;

   package Types is new Componolit.Interfaces.Block (Byte, Unsigned_Long, Buffer);

end Block;
