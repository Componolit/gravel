with Parpen.Generic_Types;
with Parpen.Resolve;
with Parpen.Protocol;

generic
   type Client_ID is (<>);
   Num_Nodes   : Natural;
   Num_Handles : Natural;
   with package Types is new Parpen.Generic_Types (<>);
package Parpen.Message
is
   type Result_Type is
      (Result_Valid,
       Result_Invalid);

   type Database is private;

   procedure Add_Client (ID : Client_ID);

   procedure Translate (Data           : in out Types.Bytes_Ptr;
                        Data_Offset    :        Types.Bit_Length;
                        Data_Length    :        Types.Bit_Length;
                        Offsets_Offset :        Types.Bit_Length;
                        Offsets_Length :        Types.Bit_Length;
                        Source_ID      :        Client_ID;
                        Dest_ID        :        Client_ID;
                        Result         :    out Result_Type);

   generic
      with procedure Apply (Offset   :        Parpen.Protocol.Offset;
                            Continue :    out Boolean);
   procedure Offsets (Data           : in out Types.Bytes_Ptr;
                      Offsets_Offset :        Types.Bit_Length;
                      Offsets_Length :        Types.Bit_Length;
                      Result         :    out Result_Type);

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