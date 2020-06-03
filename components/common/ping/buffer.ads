
with RFLX_Builtin_Types;

generic
   Size : RFLX_Builtin_Types.Length;
package Buffer with
   SPARK_Mode,
   Elaborate_Body
is

   subtype Bytes is RFLX_Builtin_Types.Bytes (1 .. Size);

   Ptr : RFLX_Builtin_Types.Bytes_Ptr;

end Buffer;
