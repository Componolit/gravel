with Parpen.Generic_Types;
with Parpen.Unique_Map;
with Parpen.Protocol;

generic
   type Client_ID is (<>);
   Null_Client_ID : Client_ID;
   type Node_ID is (<>);
   Null_Node_ID : Node_ID;
   with package Types is new Parpen.Generic_Types (<>);
package Parpen.Resolve
is
   type Result_Type is (Result_OK, Result_Invalid, Result_Needless);

   type Node_Cursor_Option is tagged private;
   type Client_Cursor_Option is tagged private;

   function Valid (N : Node_Cursor_Option) return Boolean;

   type Database is tagged private;

   function Initialized (DB : Database) return Boolean with Ghost;

   procedure Initialize (DB : in out Database) with
      Post => Initialized (DB);

   function Find_Node (DB    : Database'Class;
                       Owner : Client_ID;
                       Value : Parpen.Protocol.Binder) return Node_Cursor_Option with
      Pre => Initialized (DB);

   procedure Insert_Node (DB     : in out Database'Class;
                          Cursor :        Node_Cursor_Option;
                          Owner  :        Client_ID;
                          Value  :        Parpen.Protocol.Binder) with
      Pre => Initialized (DB);

   procedure Resolve_Handle (DB     :        Database;
                             Buffer : in out Types.Bytes_Ptr;
                             Offset :        Types.Bit_Length;
                             Source :        Client_ID;
                             Dest   :        Client_ID;
                             Result :    out Result_Type) with
      Pre => Initialized (DB);

private

   type Node_Type is
      record
         Owner : Client_ID;
         Value : Parpen.Protocol.Binder;
      end record;
   Null_Node : constant Node_Type := (Owner => Null_Client_ID,
                                      Value => 0);

   package Node_DB is new Parpen.Unique_Map (Key          => Node_ID,
                                             Null_Key     => Null_Node_ID,
                                             Element      => Node_Type,
                                             Null_Element => Null_Node);
   use type Node_DB.Status;

   type Node_Cursor_Option is tagged
      record
         Inner : Node_DB.Option;
      end record;

   function Valid (N : Node_Cursor_Option) return Boolean is
      (N.Inner.Result = Node_DB.Status_OK
       or N.Inner.Result = Node_DB.Status_Not_Found);

   type Client_Type is null record;
   Null_Client : constant Client_Type := (null record);

   package Client_DB is new Parpen.Unique_Map (Key          => Client_ID,
                                               Null_Key     => Null_Client_ID,
                                               Element      => Client_Type,
                                               Null_Element => Null_Client);

   type Client_Cursor_Option is tagged
      record
         Inner : Client_DB.Option;
      end record;

   type Database is tagged
      record
         Nodes   : Node_DB.Database;
         Clients : Client_DB.Database;
      end record;

   function Initialized (DB : Database) return Boolean is (DB.Nodes.Initialized);

   function Find_Node (DB    : Database'Class;
                       Owner : Client_ID;
                       Value : Parpen.Protocol.Binder) return Node_Cursor_Option is
      (Inner => DB.Nodes.Search_Value ((Owner => Owner, Value => Value)));

end Parpen.Resolve;
