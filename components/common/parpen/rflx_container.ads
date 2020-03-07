generic
   type Index is range <>;
   type Byte is (<>);
   type Bytes is array (Index range <>) of Byte;
   type Bytes_Ptr is access Bytes;
   Length : Index;
package RFLX_Container with
   SPARK_Mode,
   Elaborate_Body
is
   Ptr : Bytes_Ptr;
end RFLX_Container;
