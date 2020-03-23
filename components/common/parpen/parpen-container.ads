with Parpen.Generic_Types;

generic
   with package Types is new Parpen.Generic_Types (<>);
   Length : Types.Index;
package Parpen.Container with
   SPARK_Mode,
   Elaborate_Body
is
   Ptr : Types.Bytes_Ptr;
end Parpen.Container;
