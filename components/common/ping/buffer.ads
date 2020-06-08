
with RFLX_Builtin_Types;

generic
   Size : RFLX_Builtin_Types.Length;
package Buffer with
   SPARK_Mode,
   Elaborate_Body
is

   type Bytes is record
      Data   : RFLX_Builtin_Types.Bytes_Ptr;
      Length : RFLX_Builtin_Types.Length;
   end record;

   Ptr : RFLX_Builtin_Types.Bytes_Ptr;

end Buffer;
