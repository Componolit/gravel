
package body Buffer with
   SPARK_Mode
is

   Buf : aliased Net.RFLX_Builtin_Types.Bytes (1 .. Size);

begin
   pragma SPARK_Mode (Off);
   Ptr := Buf'Unrestricted_Access;
end Buffer;
