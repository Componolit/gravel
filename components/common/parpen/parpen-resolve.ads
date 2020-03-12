with Parpen.Generic_Types;

generic
   with package Types is new Parpen.Generic_Types (<>);
package Parpen.Resolve
is
   type Result_Type is (Result_OK, Result_Invalid, Result_Needless);

   procedure Resolve_Handle (Buffer : in out Types.Bytes_Ptr;
                             Offset :        Types.Bit_Length;
                             Result :    out Result_Type);
end Parpen.Resolve;
