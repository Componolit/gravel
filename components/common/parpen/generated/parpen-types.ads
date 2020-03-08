pragma SPARK_Mode;
with Parpen.Generic_Types;
with Parpen.Builtin_Types;

package Parpen.Types is new Parpen.Generic_Types (Builtin_Types.Index, Builtin_Types.Byte, Builtin_Types.Bytes, Builtin_Types.Bytes_Ptr, Builtin_Types.Length, Builtin_Types.Bit_Length);
