with Parpen.Generic_Types;
with Parpen.Resolve;

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

   procedure Translate (Data      : in out Types.Bytes_Ptr;
                        Offsets   :        Types.Bytes_Ptr;
                        Source_ID :        Client_ID;
                        Dest_ID   :        Client_ID;
                        Result    :    out Result_Type);

private

   type Node_ID is new Natural range 1 .. Num_Nodes;
   type Handle_ID is new Natural range 0 .. Num_Handles;

   package Resolve is new Parpen.Resolve (Client_ID => Client_ID,
                                          Node_ID   => Node_ID,
                                          Handle_ID => Handle_ID,
                                          Types     => Types);

   type Database is tagged record
      Inner : Resolve.Database;
   end record;

   Clients : Database;

end Parpen.Message;
