package body Parpen.Protocol.Generic_Contains with
  SPARK_Mode
is

   procedure Switch_To_Data (Protocol_Request_Context : in out Protocol_Request.Context; Protocol_Transaction_Context : out Protocol_Transaction.Context) is
      First : constant Types.Bit_Index := Protocol_Request.Field_First (Protocol_Request_Context, Protocol_Request.F_Data);
      Last : constant Types.Bit_Index := Protocol_Request.Field_Last (Protocol_Request_Context, Protocol_Request.F_Data);
      Buffer : Types.Bytes_Ptr;
   begin
      Protocol_Request.Take_Buffer (Protocol_Request_Context, Buffer);
      pragma Warnings (Off, "unused assignment to ""Buffer""");
      Protocol_Transaction.Initialize (Protocol_Transaction_Context, Buffer, First, Last);
      pragma Warnings (On, "unused assignment to ""Buffer""");
   end Switch_To_Data;

end Parpen.Protocol.Generic_Contains;
