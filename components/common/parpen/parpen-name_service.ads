with Parpen.Generic_Types;
with Parpen.Protocol;

generic
   with package Types is new Parpen.Generic_Types (<>);
   type Client_ID is (<>);
   Num_Entries : Natural;
package Parpen.Name_Service is

   type Result_Type is
      (Result_Valid,
       Result_Invalid_Method,
       Result_Invalid);

   procedure Process (Data           : in out Types.Bytes_Ptr;
                      Data_Offset    : in out Types.Bit_Length;
                      Data_Length    : in out Types.Bit_Length;
                      Offsets_Offset :    out Types.Bit_Length;
                      Offsets_Length :    out Types.Bit_Length;
                      Source_ID      :        Client_ID;
                      Method         :        Parpen.Protocol.Method;
                      Result         :    out Result_Type);
end Parpen.Name_Service;
