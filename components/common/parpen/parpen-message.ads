with Parpen.Generic_Types;
with Parpen.Resolve;
with Parpen.Protocol;

generic
   with package Types is new Parpen.Generic_Types (<>);
   type Client_ID is (<>);
   Num_Nodes           : Natural;
   Num_Handles         : Natural;
   Num_Name_DB_Entries : Natural;
package Parpen.Message
is
   type Result_Type is
      (Result_Valid,
       Result_Invalid,
       Result_Invalid_Handle,
       Result_Invalid_Method,
       Result_Offset_Out_Of_Range,
       Result_Receiver_Not_Ready,
       Result_Receive_Buffer_Too_Small,
       Result_Overflow);

   type Database is private;

   type Client_State (Receiving : Boolean := False) is
      record
         case Receiving is
            when True =>
               First : Types.Index;
               Last  : Types.Index;
            when False =>
               null;
         end case;
      end record;

   --  Add a client to client database
   procedure Add_Client (ID : Client_ID);

   --  Return client state
   function Get_Client_State (ID : Client_ID) return Client_State;

   --  Set client state
   procedure Set_Client_State (ID    : Client_ID;
                               State : Client_State);

   --  Dispatch a transaction, call generic Send procedure to send data to receiver
   generic
      with procedure Send (ID         : Client_ID;
                           Handle     : Parpen.Protocol.Handle;
                           Method     : Parpen.Protocol.Method;
                           Cookie     : Parpen.Protocol.Cookie;
                           Oneway     : Boolean;
                           Accept_FDs : Boolean;
                           Data       : Types.Bytes_Ptr;
                           Data_First : Types.Index;
                           Data_Last  : Types.Index;
                           Recv_First : Types.Index;
                           Recv_Last  : Types.Index);
   procedure Dispatch (Sender         :        Client_ID;
                       Handle         :        Parpen.Protocol.Handle;
                       Method         :        Parpen.Protocol.Method;
                       Cookie         :        Parpen.Protocol.Cookie;
                       Oneway         :        Boolean;
                       Accept_FDs     :        Boolean;
                       Data           : in out Types.Bytes_Ptr;
                       Send_Offset    :        Types.Bit_Length;
                       Send_Length    :        Types.Bit_Length;
                       Recv_Offset    :        Types.Bit_Length;
                       Recv_Length    :        Types.Bit_Length;
                       Offsets_Offset :        Types.Bit_Length;
                       Offsets_Length :        Types.Bit_Length;
                       Result         :    out Result_Type);

   procedure Ignore (ID         : Client_ID;
                     Handle     : Parpen.Protocol.Handle;
                     Method     : Parpen.Protocol.Method;
                     Cookie     : Parpen.Protocol.Cookie;
                     Oneway     : Boolean;
                     Accept_FDs : Boolean;
                     Data       : Types.Bytes_Ptr;
                     Data_First : Types.Index;
                     Data_Last  : Types.Index;
                     Recv_First : Types.Index;
                     Recv_Last  : Types.Index);

   --  Apply Operation to every offset encoded in Data
   generic
      with procedure Operation (Offset   :        Parpen.Protocol.Offset;
                                Result   :    out Result_Type);
   procedure Offsets (Data           : in out Types.Bytes_Ptr;
                      Offsets_Offset :        Types.Bit_Length;
                      Offsets_Length :        Types.Bit_Length;
                      Result         :    out Result_Type);

   --  Initialized package and set client ID of name service
   --  This ID must not be used for other purposes.
   procedure Initialize (Name_Service_ID : Client_ID);

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
                        Result         :    out Result_Type);

end Parpen.Message;
