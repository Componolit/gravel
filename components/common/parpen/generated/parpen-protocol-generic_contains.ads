with Parpen.Generic_Types;
with Parpen.Protocol;
use Parpen.Protocol;
with Parpen.Protocol.Generic_Request;
with Parpen.Protocol.Generic_Transaction;

generic
   with package Types is new Parpen.Generic_Types (<>);
   with package Protocol_Request is new Parpen.Protocol.Generic_Request (Types, others => <>);
   with package Protocol_Transaction is new Parpen.Protocol.Generic_Transaction (Types, others => <>);
package Parpen.Protocol.Generic_Contains with
  SPARK_Mode
is

   pragma Annotate (GNATprove, Terminating, Generic_Contains);

   use type Types.Bytes, Types.Bytes_Ptr, Types.Index, Types.Length, Types.Bit_Index, Types.Bit_Length, Protocol_Request.Field_Cursors;

   function Transaction_In_Request_Data (Ctx : Protocol_Request.Context) return Boolean is
     (Protocol_Request.Has_Buffer (Ctx)
      and then Protocol_Request.Present (Ctx, Protocol_Request.F_Data)
      and then Protocol_Request.Valid (Ctx, Protocol_Request.F_Tag)
      and then Protocol_Request.Get_Tag (Ctx) = REQUEST_TRANSACTION);

   procedure Switch_To_Data (Protocol_Request_Context : in out Protocol_Request.Context; Protocol_Transaction_Context : out Protocol_Transaction.Context) with
     Pre =>
       not Protocol_Request_Context'Constrained
          and not Protocol_Transaction_Context'Constrained
          and Protocol_Request.Has_Buffer (Protocol_Request_Context)
          and Protocol_Request.Present (Protocol_Request_Context, Protocol_Request.F_Data)
          and Protocol_Request.Valid (Protocol_Request_Context, Protocol_Request.F_Tag)
          and Transaction_In_Request_Data (Protocol_Request_Context),
     Post =>
       not Protocol_Request.Has_Buffer (Protocol_Request_Context)
          and Protocol_Transaction.Has_Buffer (Protocol_Transaction_Context)
          and Protocol_Request_Context.Buffer_First = Protocol_Transaction_Context.Buffer_First
          and Protocol_Request_Context.Buffer_Last = Protocol_Transaction_Context.Buffer_Last
          and Protocol_Transaction_Context.First = Protocol_Request.Field_First (Protocol_Request_Context, Protocol_Request.F_Data)
          and Protocol_Transaction_Context.Last = Protocol_Request.Field_Last (Protocol_Request_Context, Protocol_Request.F_Data)
          and Protocol_Transaction.Initialized (Protocol_Transaction_Context)
          and Protocol_Request_Context.Buffer_First = Protocol_Request_Context.Buffer_First'Old
          and Protocol_Request_Context.Buffer_Last = Protocol_Request_Context.Buffer_Last'Old
          and Protocol_Request_Context.First = Protocol_Request_Context.First'Old
          and Protocol_Request.Cursors (Protocol_Request_Context) = Protocol_Request.Cursors (Protocol_Request_Context)'Old;

end Parpen.Protocol.Generic_Contains;
