with Parpen.Generic_Types;
with Parpen.Protocol;

generic
   with package Types is new Parpen.Generic_Types (<>);
   with procedure Trace (Message : String);
   Num_Entries : Natural;
package Parpen.Name_Service is

   type Status is
      (Status_Valid,
       Status_Invalid_Method,
       Status_Invalid_Binder,
       Status_Not_Found,
       Status_Invalid);

   procedure Initialize;

   procedure Process (Data           : in out Types.Bytes_Ptr;
                      Data_Offset    : in out Types.Bit_Length;
                      Data_Length    : in out Types.Bit_Length;
                      Offsets_Offset :    out Types.Bit_Length;
                      Offsets_Length :    out Types.Bit_Length;
                      Method         :        Parpen.Protocol.Method;
                      Cookie         :        Parpen.Protocol.Cookie;
                      Status         :    out Parpen.Name_Service.Status);
end Parpen.Name_Service;
