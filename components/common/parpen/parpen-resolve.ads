with Parpen.Generic_Types;
with Parpen.Unique_Map;
with Parpen.Binder;

generic
   type Client_ID is (<>);
   type Node_ID is (<>);
   type Handle_ID is (<>);
   with package Types is new Parpen.Generic_Types (<>);
package Parpen.Resolve
is
   type Result_Type is
      (Result_OK,
       Result_Invalid,
       Result_Invalid_Owner,
       Result_Invalid_Source,
       Result_Invalid_Destination,
       Result_Invalid_Handle,
       Result_Handle_Not_Found,
       Result_Node_Not_Found,
       Result_Needless);

   type Node_Option is tagged private;
   type Client_Cursor_Option is tagged private;

   function Valid (N : Node_Option) return Boolean;
   function Found (N : Node_Option) return Boolean;

   type Database is tagged private;

   function Initialized (DB : Database) return Boolean with Ghost;

   procedure Initialize (DB : in out Database) with
      Post => Initialized (DB);

   function Get_Node (DB       : Database'Class;
                      Owner_ID : Client_ID;
                      Value    : Parpen.Binder.Value) return Node_Option with
      Pre => Initialized (DB);

   function Get_Node (DB       : Database'Class;
                      Owner_ID : Client_ID;
                      Handle   : Parpen.Binder.Handle) return Node_Option with
      Pre => Initialized (DB);

   function Get_Owner (Node : Node_Option) return Client_ID with
      Pre => Found (Node);

   function Get_Value (Node : Node_Option) return Parpen.Binder.Value with
      Pre => Found (Node);

   procedure Add_Node (DB     : in out Database'Class;
                       Cursor : in out Node_Option;
                       Owner  :        Client_ID;
                       Value  :        Parpen.Binder.Value) with
      Pre => Initialized (DB);

   procedure Add_Client (DB : in out Database'Class;
                         ID :        Client_ID) with
      Pre => Initialized (DB);

   procedure Add_Handle (DB    : in out Database'Class;
                         ID    :        Client_ID;
                         Node  :        Node_Option) with
      Pre => Initialized (DB);

   procedure Resolve (DB        : in out Database;
                      Buffer    : in out Types.Bytes_Ptr;
                      Offset    :        Types.Bit_Length;
                      Length    :        Types.Bit_Length;
                      Source_ID :        Client_ID;
                      Dest_ID   :        Client_ID;
                      Result    :    out Result_Type) with
      Pre => Initialized (DB);

private

   type Node_Type is
      record
         Owner : Client_ID;
         Value : Parpen.Binder.Value;
      end record;

   package Node_DB is new Parpen.Unique_Map (Key     => Node_ID,
                                             Element => Node_Type);
   use type Node_DB.Status;

   type Node_Option is tagged
      record
         Inner : Node_DB.Option;
      end record;

   function Valid (N : Node_Option) return Boolean is
      (N.Inner.State = Node_DB.Status_Valid
       or N.Inner.State = Node_DB.Status_Not_Found);

   function Found (N : Node_Option) return Boolean is
      (Valid (N)
       and N.Inner.State /= Node_DB.Status_Not_Found);

   package Handle_DB is new Parpen.Unique_Map (Key     => Handle_ID,
                                               Element => Node_ID);

   type Client_Type is
      record
         Handles : Handle_DB.Database;
      end record;

   package Client_DB is new Parpen.Unique_Map (Key     => Client_ID,
                                               Element => Client_Type);

   type Client_Cursor_Option is tagged
      record
         Inner : Client_DB.Option;
      end record;

   type Database is tagged
      record
         Nodes   : Node_DB.Database;
         Clients : Client_DB.Database;
      end record;

   function Initialized (DB : Database) return Boolean is
      (DB.Nodes.Initialized and DB.Clients.Initialized);

   function Get_Node (DB       : Database'Class;
                      Owner_ID : Client_ID;
                      Value    : Parpen.Binder.Value) return Node_Option is
      (Inner => DB.Nodes.Find ((Owner => Owner_ID, Value => Value)));

   function Get_Owner (Node : Node_Option) return Client_ID is
      (Node.Inner.Data.Owner);

   function Get_Value (Node : Node_Option) return Parpen.Binder.Value is
      (Node.Inner.Data.Value);

end Parpen.Resolve;
