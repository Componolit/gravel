with Parpen.Generic_Types;
with Parpen.Unique_Map;
with Parpen.Binder;

generic
   type Client_ID is (<>);
   type Client_State is private;
   type Node_ID is (<>);
   type Handle_ID is (<>);
   with package Types is new Parpen.Generic_Types (<>);
package Parpen.Resolve
is
   type Status is
      (Status_Valid,
       Status_Invalid,
       Status_Invalid_Client,
       Status_Invalid_Owner,
       Status_Invalid_Source,
       Status_Invalid_Destination,
       Status_Invalid_Handle,
       Status_Handle_Not_Found,
       Status_Node_Not_Found,
       Status_Needless);

   type Client_State_Option (Status : Parpen.Resolve.Status) is
      record
         case Status is
            when Status_Valid =>
               Data : Client_State;
            when others =>
               null;
         end case;
      end record;

   type Node_Option is private;
   type Client_Cursor_Option is tagged private;

   function Valid (Node : Node_Option) return Boolean;
   function Found (Node : Node_Option) return Boolean;

   type Database is tagged private;

   function Initialized (Database : Parpen.Resolve.Database) return Boolean with Ghost;

   procedure Initialize (Database : in out Parpen.Resolve.Database) with
      Post => Initialized (Database);

   function Get_Node (Database : Parpen.Resolve.Database;
                      Owner_ID : Client_ID;
                      Value    : Parpen.Binder.Value) return Node_Option with
      Pre => Initialized (Database);

   function Get_Node (Database : Parpen.Resolve.Database;
                      Owner_ID : Client_ID;
                      Handle   : Parpen.Binder.Handle) return Node_Option with
      Pre => Initialized (Database);

   function Get_Owner (Node : Node_Option) return Client_ID with
      Pre => Found (Node);

   function Get_Value (Node : Node_Option) return Parpen.Binder.Value with
      Pre => Found (Node);

   procedure Add_Node (Database : in out Parpen.Resolve.Database;
                       Cursor   : in out Node_Option;
                       Owner    :        Client_ID;
                       Value    :        Parpen.Binder.Value;
                       Status   :    out Parpen.Resolve.Status) with
      Pre => Initialized (Database);

   procedure Add_Client (Database : in out Parpen.Resolve.Database;
                         ID       :        Client_ID;
                         State    :        Client_State;
                         Status   :    out Parpen.Resolve.Status) with
      Pre => Initialized (Database);

   procedure Set_Client_State (Database : in out Parpen.Resolve.Database;
                               ID       :        Client_ID;
                               State    :        Client_State;
                               Status   :    out Parpen.Resolve.Status) with
      Pre  => Initialized (Database),
      Post => Initialized (Database);

   function Get_Client_State (Database : Parpen.Resolve.Database;
                              ID       : Client_ID) return Client_State_Option with
      Pre => Initialized (Database);

   procedure Add_Handle (Database : in out Parpen.Resolve.Database;
                         ID       :        Client_ID;
                         Node     :        Node_Option;
                         Status   :    out Parpen.Resolve.Status) with
      Pre => Initialized (Database);

   procedure Resolve (Database  : in out Parpen.Resolve.Database;
                      Buffer    : in out Types.Bytes_Ptr;
                      Offset    :        Types.Bit_Length;
                      Length    :        Types.Bit_Length;
                      Source_ID :        Client_ID;
                      Dest_ID   :        Client_ID;
                      Status    :    out Parpen.Resolve.Status) with
      Pre => Initialized (Database);

private

   type Node_Type is
      record
         Owner : Client_ID;
         Value : Parpen.Binder.Value;
      end record;

   package Node_DB is new Parpen.Unique_Map (Key     => Node_ID,
                                             Element => Node_Type);
   use type Node_DB.Status;

   type Node_Option is
      record
         Inner : Node_DB.Option;
      end record;

   function Valid (Node : Node_Option) return Boolean is
      (Node.Inner.Status = Node_DB.Status_Valid
       or Node.Inner.Status = Node_DB.Status_Not_Found);

   function Found (Node : Node_Option) return Boolean is
      (Valid (Node)
       and Node.Inner.Status /= Node_DB.Status_Not_Found);

   package Handle_DB is new Parpen.Unique_Map (Key     => Handle_ID,
                                               Element => Node_ID);

   type Client_Type is
      record
         Handles : Handle_DB.Database;
         State   : Client_State;
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

   function Initialized (Database : Parpen.Resolve.Database) return Boolean is
      (Database.Nodes.Initialized and Database.Clients.Initialized);

   function Get_Node (Database : Parpen.Resolve.Database;
                      Owner_ID : Client_ID;
                      Value    : Parpen.Binder.Value) return Node_Option is
      (Inner => Database.Nodes.Find ((Owner => Owner_ID,
                                      Value => Value)));

   function Get_Owner (Node : Node_Option) return Client_ID is
      (Node.Inner.Data.Owner);

   function Get_Value (Node : Node_Option) return Parpen.Binder.Value is
      (Node.Inner.Data.Value);

end Parpen.Resolve;
