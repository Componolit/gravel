
with RFLX_Builtin_Types;

generic
   Size : RFLX_Builtin_Types.Length;
package Ping.Buffer with
   SPARK_Mode,
   Elaborate_Body
is

   Ptr : RFLX_Builtin_Types.Bytes_Ptr;

end Ping.Buffer;
