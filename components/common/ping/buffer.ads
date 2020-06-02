
with Net.RFLX_Builtin_Types;

generic
   Size : Net.RFLX_Builtin_Types.Length;
package Buffer with
   SPARK_Mode,
   Elaborate_Body
is

   subtype Bytes is Net.RFLX_Builtin_Types.Bytes (1 .. Size);

   Ptr : Net.RFLX_Builtin_Types.Bytes_Ptr;

end Buffer;
