with Parpen.Binder.Generic_IBinder;

package body Parpen.Resolve is

   package IBinder_Package is new Parpen.Binder.Generic_IBinder (Types);

   procedure Resolve_Handle (Database  : in out Parpen.Resolve.Database;
                             Context   : in out IBinder_Package.Context;
                             Source_ID :        Client_ID;
                             Dest_ID   :        Client_ID;
                             Status    :    out Status_Type);

   procedure Resolve_Binder (Database  : in out Parpen.Resolve.Database;
                             Context   : in out IBinder_Package.Context;
                             Source_ID :        Client_ID;
                             Dest_ID   :        Client_ID;
                             Status    :    out Status_Type);

   procedure Install_Handle (Database  : in out Parpen.Resolve.Database;
                             Context   : in out IBinder_Package.Context;
                             Dest_ID   :        Client_ID;
                             Node      :        Node_ID;
                             Weak      :        Boolean);

   procedure Install_Binder (Context : in out IBinder_Package.Context;
                             Binder  :        Parpen.Binder.Value;
                             Weak    :        Boolean);

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Database : in out Parpen.Resolve.Database)
   is
   begin
      Database.Nodes.Initialize;
      Database.Clients.Initialize;
   end Initialize;

   --------------
   -- Add_Node --
   --------------

   procedure Add_Node (Database : in out Parpen.Resolve.Database;
                       Cursor   : in out Node_Option;
                       Owner    :        Client_ID;
                       Value    :        Parpen.Binder.Value)
   is
      Node : Node_DB.Option := (Status   => Node_DB.Status_Valid,
                                Data     => (Owner, Value),
                                Position => Cursor.Inner.Free);
   begin
      Database.Nodes.Insert (Node);
   end Add_Node;

   ----------------
   -- Add_Client --
   ----------------

   procedure Add_Client (Database : in out Parpen.Resolve.Database;
                         ID       :        Client_ID;
                         State    :        Client_State)
   is
      Client : Client_DB.Option := (Status   => Client_DB.Status_Valid,
                                    Position => ID,
                                    Data     => (Handles => Handle_DB.Null_DB,
                                                 Status  => State));
   begin
      Database.Clients.Insert (Client);
   end Add_Client;

   ----------------------
   -- Get_Client_State --
   ----------------------

   function Get_Client_State (Database : Parpen.Resolve.Database;
                              ID       : Client_ID) return Client_State is
      (Database.Clients.Get (ID).Data.Status);

   ----------------------
   -- Set_Client_State --
   ----------------------

   procedure Set_Client_State (Database : in out Parpen.Resolve.Database;
                               ID       :        Client_ID;
                               State    :        Client_State)
   is
      procedure Set_State (Client : in out Client_Type);
      procedure Set_State (Client : in out Client_Type)
      is
      begin
         Client.Status := State;
      end Set_State;
      procedure Set_State is new Client_DB.Generic_Apply (Operation => Set_State);
   begin
      Set_State (Database => Database.Clients,
                 Key      => ID);
   end Set_Client_State;

   ----------------
   -- Add_Handle --
   ----------------

   procedure Add_Handle (Database : in out Parpen.Resolve.Database;
                         ID       :        Client_ID;
                         Node     :        Node_Option)
   is
      procedure Add_Node (Client : in out Client_Type);
      procedure Add_Node (Client : in out Client_Type)
      is
         Result : Handle_DB.Option;
         use type Handle_DB.Status;
      begin
         Result := Client.Handles.Find (Node.Inner.Free);
         if Result.Status = Handle_DB.Status_Not_Found then
            Result := (Status   => Handle_DB.Status_Valid,
                       Position => Result.Free,
                       Data     => Node.Inner.Free);
            Client.Handles.Insert (Result);
         end if;
      end Add_Node;

      procedure Add_Node is new Client_DB.Generic_Apply (Operation => Add_Node);
   begin
      Add_Node (Database.Clients, ID);
   end Add_Handle;

   --------------------
   -- Install_Handle --
   --------------------

   procedure Install_Handle (Database  : in out Parpen.Resolve.Database;
                             Context   : in out IBinder_Package.Context;
                             Dest_ID   :        Client_ID;
                             Node      :        Node_ID;
                             Weak      :        Boolean)
   is
      Handle : Handle_DB.Option;
      Cookie : Parpen.Binder.Cookie;
      Flags  : Parpen.Binder.Flat_Binder_Flags;

      procedure Insert_Handle (Client : in out Client_Type);
      procedure Insert_Handle (Client : in out Client_Type)
      is
         use type Handle_DB.Status;
      begin
         Handle := Client.Handles.Find (Node);
         if Handle.Status = Handle_DB.Status_Not_Found then
            Handle := (Status   => Handle_DB.Status_Valid,
                       Position => Handle.Free,
                       Data     => Node);
            Client.Handles.Insert (Handle);
         end if;
      end Insert_Handle;

      procedure Update_Client_DB is new Client_DB.Generic_Apply (Insert_Handle);
   begin
      Update_Client_DB (Database.Clients, Dest_ID);

      --  Replace binder by handle
      Cookie := IBinder_Package.Get_Cookie (Context);
      Flags  := IBinder_Package.Get_Flags (Context);

      if Weak then
         IBinder_Package.Set_Kind (Context, Parpen.Binder.BK_WEAK_HANDLE);
      else
         IBinder_Package.Set_Kind (Context, Parpen.Binder.BK_STRONG_HANDLE);
      end if;
      IBinder_Package.Set_Arity (Context, Parpen.Binder.BA_SINGLE);
      IBinder_Package.Set_Tag (Context, 16#85#);
      IBinder_Package.Set_Flags (Context, Flags);
      IBinder_Package.Set_Handle (Context, Parpen.Binder.Handle'Val (Handle_ID'Pos (Handle.Position)));
      IBinder_Package.Set_Unused_Padding (Context, 0);
      IBinder_Package.Set_Cookie (Context, Cookie);
   end Install_Handle;

   --------------------
   -- Install_Binder --
   --------------------

   procedure Install_Binder (Context : in out IBinder_Package.Context;
                             Binder  :        Parpen.Binder.Value;
                             Weak    :        Boolean)
   is
      Cookie : Parpen.Binder.Cookie;
      Flags  : Parpen.Binder.Flat_Binder_Flags;
   begin
      Cookie := IBinder_Package.Get_Cookie (Context);
      Flags  := IBinder_Package.Get_Flags (Context);

      if Weak then
         IBinder_Package.Set_Kind (Context, Parpen.Binder.BK_WEAK_BINDER);
      else
         IBinder_Package.Set_Kind (Context, Parpen.Binder.BK_STRONG_BINDER);
      end if;
      IBinder_Package.Set_Arity (Context, Parpen.Binder.BA_SINGLE);
      IBinder_Package.Set_Tag (Context, 16#85#);
      IBinder_Package.Set_Flags (Context, Flags);
      IBinder_Package.Set_Binder (Context, Binder);
      IBinder_Package.Set_Cookie (Context, Cookie);
   end Install_Binder;

   --------------------
   -- Resolve_Handle --
   --------------------

   procedure Resolve_Binder (Database  : in out Parpen.Resolve.Database;
                             Context   : in out IBinder_Package.Context;
                             Source_ID :        Client_ID;
                             Dest_ID   :        Client_ID;
                             Status    :    out Status_Type)
   is
      Node   : Node_DB.Option;
      Binder : Parpen.Binder.Value;
      Weak   : Boolean;
      use type Parpen.Binder.Binder_Kind;
   begin
      Binder := IBinder_Package.Get_Binder (Context);
      Node := Database.Nodes.Find ((Source_ID, Binder));
      if Node.Status = Node_DB.Status_Not_Found then
         Node := (Status   => Node_DB.Status_Valid,
                  Position => Node.Free,
                  Data     => (Source_ID, Binder));
         Database.Nodes.Insert (Node);
      end if;

      if IBinder_Package.Get_Kind (Context) = Parpen.Binder.BK_WEAK_BINDER then
         Weak := True;
      elsif IBinder_Package.Get_Kind (Context) /= Parpen.Binder.BK_STRONG_BINDER then
         Weak := False;
      end if;

      if Node.Data.Owner = Dest_ID then
         Install_Binder (Context => Context,
                         Binder  => Node.Data.Value,
                         Weak    => Weak);
      else
         Install_Handle (Database => Database,
                         Context  => Context,
                         Dest_ID  => Dest_ID,
                         Node     => Node.Position,
                         Weak     => Weak);
      end if;

      Status := Status_OK;
   end Resolve_Binder;

   --------------------
   -- Resolve_Handle --
   --------------------

   procedure Resolve_Handle (Database  : in out Parpen.Resolve.Database;
                             Context   : in out IBinder_Package.Context;
                             Source_ID :        Client_ID;
                             Dest_ID   :        Client_ID;
                             Status    :    out Status_Type)
   is
      Handle           : Parpen.Binder.Handle;
      Node             : Node_DB.Option;
      Weak             : Boolean;
      use type Parpen.Binder.Binder_Kind;
   begin
      Handle := IBinder_Package.Get_Handle (Context);
      if
         Parpen.Binder.Handle'Pos (Handle) > Handle_ID'Pos (Handle_ID'Last)
         or Parpen.Binder.Handle'Pos (Handle) < Handle_ID'Pos (Handle_ID'First)
      then
         Status := Status_Invalid_Handle;
         return;
      end if;

      Node := Get_Node (Database => Database,
                        Owner_ID => Source_ID,
                        Handle   => Handle).Inner;
      if Node.Status /= Node_DB.Status_Valid then
         Status := Status_Node_Not_Found;
         return;
      end if;

      Weak := IBinder_Package.Get_Kind (Context) = Parpen.Binder.BK_WEAK_HANDLE;

      if Node.Data.Owner = Dest_ID then
         Install_Binder (Context => Context,
                         Binder  => Node.Data.Value,
                         Weak    => Weak);
      else
         Install_Handle (Database => Database,
                         Context  => Context,
                         Dest_ID  => Dest_ID,
                         Node     => Node.Position,
                         Weak     => Weak);
      end if;
      Status := Status_OK;
   end Resolve_Handle;

   -------------
   -- Resolve --
   -------------

   procedure Resolve (Database  : in out Parpen.Resolve.Database;
                      Buffer    : in out Types.Bytes_Ptr;
                      Offset    :        Types.Bit_Length;
                      Length    :        Types.Bit_Length;
                      Source_ID :        Client_ID;
                      Dest_ID   :        Client_ID;
                      Status    :    out Status_Type)
   is
      Context : IBinder_Package.Context := IBinder_Package.Create;
      Source  : Client_DB.Option;
      Dest    : Client_DB.Option;

      use type Types.Bit_Length;
      use type Client_DB.Status;
      use type Parpen.Binder.Binder_Kind;
   begin
      Status := Status_Invalid;
      Source := Database.Clients.Get (Source_ID);
      if Source.Status /= Client_DB.Status_Valid then
         Status := Status_Invalid_Source;
         return;
      end if;

      Dest := Database.Clients.Get (Dest_ID);
      if Dest.Status /= Client_DB.Status_Valid then
         Status := Status_Invalid_Destination;
         return;
      end if;

      IBinder_Package.Initialize (Context,
                                  Buffer,
                                  Types.First_Bit_Index (Buffer'First) + Offset,
                                  Types.First_Bit_Index (Buffer'First) + Offset + Length - 1);
      IBinder_Package.Verify_Message (Context);
      if not IBinder_Package.Valid_Message (Context) then
         return;
      end if;

      if
         IBinder_Package.Get_Kind (Context) = Parpen.Binder.BK_WEAK_HANDLE
         or IBinder_Package.Get_Kind (Context) = Parpen.Binder.BK_STRONG_HANDLE
      then
         Resolve_Handle (Database, Context, Source_ID, Dest_ID, Status);
      elsif
         IBinder_Package.Get_Kind (Context) = Parpen.Binder.BK_WEAK_BINDER
         or IBinder_Package.Get_Kind (Context) = Parpen.Binder.BK_STRONG_BINDER
      then
         Resolve_Binder (Database, Context, Source_ID, Dest_ID, Status);
      else
         Status := Status_Needless;
      end if;
      IBinder_Package.Take_Buffer (Context, Buffer);
   end Resolve;

   --------------
   -- Get_Node --
   --------------

   function Get_Node (Database : Parpen.Resolve.Database;
                      Owner_ID : Client_ID;
                      Handle   : Parpen.Binder.Handle) return Node_Option
   is
      Owner           : Client_DB.Option;
      Owner_Handle    : Handle_DB.Option;
      Owner_Handle_ID : constant Handle_ID := Handle_ID'Val (Parpen.Binder.Handle'Pos (Handle));
      use type Handle_DB.Status;
   begin
      Owner := Database.Clients.Get (Owner_ID);
      Owner_Handle := Owner.Data.Handles.Get (Owner_Handle_ID);
      if Owner_Handle.Status /= Handle_DB.Status_Valid then
         return (Inner => (Status => Node_DB.Status_Invalid));
      end if;
      return (Inner => Database.Nodes.Get (Owner_Handle.Data));
   end Get_Node;

end Parpen.Resolve;
