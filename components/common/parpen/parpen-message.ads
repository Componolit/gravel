with Parpen.Generic_Types;

generic
   type Client_ID is (<>);
   Null_Client_ID : Client_ID;
   with package Types is new Parpen.Generic_Types (<>);
package Parpen.Message
is
   pragma Unreferenced (Null_Client_ID);
   type Result_Type is
      (Result_OK,
       Result_Invalid);

   procedure Translate (Data    : in out Types.Bytes_Ptr;
                        Offsets :        Types.Bytes_Ptr;
                        Result  :    out Result_Type);

end Parpen.Message;
