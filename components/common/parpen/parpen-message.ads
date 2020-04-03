with Parpen.Generic_Types;
with Parpen.Resolve;
with Parpen.Protocol;

generic
   with package Types is new Parpen.Generic_Types (<>);
   with procedure Trace (Message : String);
   type Client_ID is (<>);
   Num_Nodes           : Natural;
   Num_Handles         : Natural;
   Num_Name_DB_Entries : Natural;
package Parpen.Message
is
   type Status is
      (Status_Valid,
       Status_Invalid,
       Status_Invalid_Handle,
       Status_Invalid_Binder,
       Status_Invalid_Method,
       Status_Invalid_Client,
       Status_Not_Found,
       Status_Offset_Out_Of_Range,
       Status_Receiver_Not_Ready,
       Status_Receive_Buffer_Too_Small,
       Status_Overflow);

   type Database is private;

   type BH_Kind is (BH_Binder, BH_Handle);
   type BH_Option (Kind : BH_Kind) is
      record
         case Kind is
            when BH_Binder =>
               Binder : Parpen.Protocol.Binder;
            when BH_Handle =>
               Handle : Parpen.Protocol.Handle;
         end case;
      end record;

   type Client_State (Status    : Parpen.Message.Status := Status_Invalid;
                      Receiving : Boolean := False) is
      record
         case Status is
            when Status_Valid =>
               case Receiving is
                  when True =>
                     First  : Types.Index;
                     Length : Natural;
                  when False =>
                     null;
               end case;
            when others =>
               null;
         end case;
      end record;

   type Transaction is
      record
         Handle         : Parpen.Protocol.Handle;
         Method         : Parpen.Protocol.Method;
         Cookie         : Parpen.Protocol.Cookie;
         Send_Offset    : Types.Bit_Length;
         Send_Length    : Types.Bit_Length;
         Recv_Offset    : Types.Bit_Length;
         Recv_Length    : Types.Bit_Length;
         Offsets_Offset : Types.Bit_Length;
         Offsets_Length : Types.Bit_Length;
      end record;

   --  Add a client to client database
   procedure Add_Client (ID     :        Client_ID;
                         Status :    out Parpen.Message.Status);

   --  Return client state
   function Get_Client_State (ID : Client_ID) return Client_State;

   --  Set client state
   procedure Set_Client_State (ID     :        Client_ID;
                               State  :        Client_State;
                               Status :    out Parpen.Message.Status);

   --  Dispatch a transaction, call generic Send procedure to send data to receiver
   generic
      with procedure Send (ID         : Client_ID;
                           BH         : BH_Option;
                           Method     : Parpen.Protocol.Method;
                           Cookie     : Parpen.Protocol.Cookie;
                           Data       : Types.Bytes_Ptr;
                           Data_First : Types.Index;
                           Recv_First : Types.Index;
                           Length     : Natural);
   procedure Dispatch (Sender      :        Client_ID;
                       Transaction :        Parpen.Message.Transaction;
                       Data        : in out Types.Bytes_Ptr;
                       Status      :    out Parpen.Message.Status);

   procedure Ignore (ID         : Client_ID;
                     BH         : BH_Option;
                     Method     : Parpen.Protocol.Method;
                     Cookie     : Parpen.Protocol.Cookie;
                     Data       : Types.Bytes_Ptr;
                     Data_First : Types.Index;
                     Recv_First : Types.Index;
                     Length     : Natural);

   --  Apply Operation to every offset encoded in Data
   generic
      with procedure Operation (Offset   :        Parpen.Protocol.Offset;
                                Status   :    out Parpen.Message.Status);
   procedure Offsets (Data           : in out Types.Bytes_Ptr;
                      Offsets_Offset :        Types.Bit_Length;
                      Offsets_Length :        Types.Bit_Length;
                      Status         :    out Parpen.Message.Status);

   --  Initialized package and set client ID of name service
   --  This ID must not be used for other purposes.
   procedure Initialize (Name_Service_ID :         Client_ID;
                         Status          :     out Parpen.Message.Status);

private

   type Node_ID is new Natural range 1 .. Num_Nodes;
   type Handle_ID is new Natural range 0 .. Num_Handles;
   subtype Regular_Handle_ID is Handle_ID range 1 .. Handle_ID'Last;

   package Resolve is new Parpen.Resolve (Client_ID    => Client_ID,
                                          Client_State => Client_State,
                                          Node_ID      => Node_ID,
                                          Handle_ID    => Regular_Handle_ID,
                                          Types        => Types);

   type Database is tagged
      record
         Inner : Resolve.Database;
      end record;

   Clients : Database;

   procedure Translate (Data           : in out Types.Bytes_Ptr;
                        Data_Offset    :        Types.Bit_Length;
                        Data_Length    :        Types.Bit_Length;
                        Offsets_Offset :        Types.Bit_Length;
                        Offsets_Length :        Types.Bit_Length;
                        Source_ID      :        Client_ID;
                        Dest_ID        :        Client_ID;
                        Status         :    out Parpen.Message.Status) with SPARK_Mode;

end Parpen.Message;
