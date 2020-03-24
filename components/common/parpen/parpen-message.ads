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
       Result_Overflow);

   type Database is private;

   procedure Add_Client (ID : Client_ID);

   generic
      with procedure Send (ID         : Client_ID;
                           Handle     : Parpen.Protocol.Handle;
                           Method     : Parpen.Protocol.Method;
                           Cookie     : Parpen.Protocol.Cookie;
                           Oneway     : Boolean;
                           Accept_FDs : Boolean;
                           Data       : Types.Bytes;
                           Last       : Types.Index);
      pragma Unreferenced (Send);
   procedure Dispatch (Sender         :        Client_ID;
                       Handle         :        Parpen.Protocol.Handle;
                       Method         :        Parpen.Protocol.Method;
                       Cookie         :        Parpen.Protocol.Cookie;
                       Oneway         :        Boolean;
                       Accept_FDs     :        Boolean;
                       Data           : in out Types.Bytes_Ptr;
                       Data_Offset    :        Types.Bit_Length;
                       Data_Length    :        Types.Bit_Length;
                       Offsets_Offset :        Types.Bit_Length;
                       Offsets_Length :        Types.Bit_Length;
                       Result         :    out Result_Type);

   --  FIXME: Make private
   procedure Translate (Data           : in out Types.Bytes_Ptr;
                        Data_Offset    :        Types.Bit_Length;
                        Data_Length    :        Types.Bit_Length;
                        Offsets_Offset :        Types.Bit_Length;
                        Offsets_Length :        Types.Bit_Length;
                        Source_ID      :        Client_ID;
                        Dest_ID        :        Client_ID;
                        Result         :    out Result_Type);


   --  Apply Operation to every offset encoded in Data
   generic
      with procedure Operation (Offset   :        Parpen.Protocol.Offset;
                                Result   :    out Result_Type);
   procedure Offsets (Data           : in out Types.Bytes_Ptr;
                      Offsets_Offset :        Types.Bit_Length;
                      Offsets_Length :        Types.Bit_Length;
                      Result         :    out Result_Type);

   procedure Initialize;

private

   type Node_ID is new Natural range 1 .. Num_Nodes;
   type Handle_ID is new Natural range 0 .. Num_Handles;
   subtype Regular_Handle_ID is Handle_ID range 1 .. Handle_ID'Last;

   package Resolve is new Parpen.Resolve (Client_ID => Client_ID,
                                          Node_ID   => Node_ID,
                                          Handle_ID => Regular_Handle_ID,
                                          Types     => Types);

   type Database is tagged record
      Inner : Resolve.Database;
   end record;

   Clients : Database;

end Parpen.Message;
