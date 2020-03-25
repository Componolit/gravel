with Parpen.Generic_Types;
with Parpen.Protocol;

generic
   with package Types is new Parpen.Generic_Types (<>);
   Num_Entries : Natural;
package Parpen.Name_Service is

   type Result_Type is
      (Result_Valid,
       Result_Invalid_Method,
       Result_Invalid);

   procedure Initialize;

   procedure Process (Data           : in out Types.Bytes_Ptr;
                      Data_Offset    : in out Types.Bit_Length;
                      Data_Length    : in out Types.Bit_Length;
                      Offsets_Offset :    out Types.Bit_Length;
                      Offsets_Length :    out Types.Bit_Length;
                      Method         :        Parpen.Protocol.Method;
                      Cookie         :        Parpen.Protocol.Cookie;
                      Result         :    out Result_Type);
end Parpen.Name_Service;
