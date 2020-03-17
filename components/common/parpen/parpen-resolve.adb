with Parpen.Protocol.Generic_IBinder;

package body Parpen.Resolve is

   package IBinder_Package is new Parpen.Protocol.Generic_IBinder (Types);

   procedure Resolve_Handle (DB        : in     Database;
                             Context   : in out IBinder_Package.Context;
                             Source_ID :        Client_ID;
                             Dest_ID   :        Client_ID;
                             Result    :    out Result_Type);

   procedure Resolve_Binder (DB        : in out Database;
                             Context   : in out IBinder_Package.Context;
                             Source_ID :        Client_ID;
                             Dest_ID   :        Client_ID;
                             Result    :    out Result_Type);

   procedure Install_Handle (DB        :        Database;
                             Context   : in out IBinder_Package.Context;
                             Dest_ID   :        Client_ID;
                             Node      :        Node_ID;
                             Weak      :        Boolean);

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (DB : in out Database)
   is
   begin
      DB.Nodes.Initialize;
      DB.Clients.Initialize;
   end Initialize;

   --------------
   -- Add_Node --
   --------------

   procedure Add_Node (DB     : in out Database'Class;
                       Cursor : in out Node_Option;
                       Owner  :        Client_ID;
                       Value  :        Parpen.Protocol.Binder)
   is
      E : Node_DB.Option := (Result   => Node_DB.Status_Not_Found,
                             Data     => (Owner, Value),
                             Position => Cursor.Inner.Position);
   begin
      DB.Nodes.Insert (E);
   end Add_Node;

   ----------------
   -- Add_Client --
   ----------------

   procedure Add_Client (DB  : in out Database'Class;
                         ID  :        Client_ID)
   is
      E : Client_DB.Option := (Result   => Client_DB.Status_Not_Found,
                               Position => ID,
                               Data     => (Handles => Handle_DB.Null_DB));
   begin
      DB.Clients.Insert (E);
   end Add_Client;

   ----------------
   -- Add_Handle --
   ----------------

   procedure Add_Handle (DB   : in out Database'Class;
                         ID   :        Client_ID;
                         Node :        Node_Option)
   is
      procedure Add_Node (Client : in out Client_Type);

      procedure Add_Node (Client : in out Client_Type)
      is
         Result : Handle_DB.Option;
         use type Handle_DB.Status;
      begin
         Result := Client.Handles.Find (Node.Inner.Position);
         if Result.Result = Handle_DB.Status_Not_Found then
            Result.Data := Node.Inner.Position;
            Client.Handles.Insert (Result);
         end if;
      end Add_Node;

      procedure Add_Node is new Client_DB.Generic_Apply (Operation => Add_Node);
   begin
      Add_Node (DB.Clients, ID);
   end Add_Handle;

   --------------------
   -- Install_Handle --
   --------------------

   procedure Install_Handle (DB        :        Database;
                             Context   : in out IBinder_Package.Context;
                             Dest_ID   :        Client_ID;
                             Node      :        Node_ID;
                             Weak      :        Boolean)
   is
      Dest   : Client_DB.Option;
      Handle : Handle_DB.Option;
      Cookie : Parpen.Protocol.Cookie;
      Flags  : Parpen.Protocol.Flat_Binder_Flags;
      use type Handle_DB.Status;
   begin
      Dest := DB.Clients.Get (Dest_ID);

      Handle := Dest.Data.Handles.Find (Node);
      if Handle.Result = Handle_DB.Status_Not_Found then
         Handle.Data := Node;
         Dest.Data.Handles.Insert (Handle);
      end if;

      --  Replace binder by handle
      Cookie := IBinder_Package.Get_Cookie (Context);
      Flags  := IBinder_Package.Get_Flags (Context);

      if Weak then
         IBinder_Package.Set_Kind (Context, Parpen.Protocol.BK_WEAK_HANDLE);
      else
         IBinder_Package.Set_Kind (Context, Parpen.Protocol.BK_STRONG_HANDLE);
      end if;
      IBinder_Package.Set_Arity (Context, Parpen.Protocol.BA_SINGLE);
      IBinder_Package.Set_Tag (Context, 16#85#);
      IBinder_Package.Set_Flags (Context, Flags);
      IBinder_Package.Set_Handle (Context, Parpen.Protocol.Handle'Val (Handle_ID'Pos (Handle.Position)));
      IBinder_Package.Set_Unused_Padding (Context, 0);
      IBinder_Package.Set_Cookie (Context, Cookie);
   end Install_Handle;

   --------------------
   -- Resolve_Handle --
   --------------------

   procedure Resolve_Binder (DB        : in out Database;
                             Context   : in out IBinder_Package.Context;
                             Source_ID :        Client_ID;
                             Dest_ID   :        Client_ID;
                             Result    :    out Result_Type)
   is
      Node   : Node_DB.Option;
      Binder : Parpen.Protocol.Binder;
      Weak   : Boolean;
      use type Parpen.Protocol.Binder_Kind;
   begin
      Binder := IBinder_Package.Get_Binder (Context);
      Node := DB.Nodes.Find ((Source_ID, Binder));
      if Node.Result = Node_DB.Status_Not_Found then
         Node.Data := (Source_ID, Binder);
         DB.Nodes.Insert (Node);
      end if;

      if IBinder_Package.Get_Kind (Context) = Parpen.Protocol.BK_WEAK_BINDER then
         Weak := True;
      elsif IBinder_Package.Get_Kind (Context) /= Parpen.Protocol.BK_STRONG_BINDER then
         Weak := False;
      end if;

      Install_Handle (DB, Context, Dest_ID, Node.Position, Weak);
      Result := Result_OK;
   end Resolve_Binder;

   --------------------
   -- Resolve_Handle --
   --------------------

   procedure Resolve_Handle (DB        : in     Database;
                             Context   : in out IBinder_Package.Context;
                             Source_ID :        Client_ID;
                             Dest_ID   :        Client_ID;
                             Result    :    out Result_Type)
   is
      Source           : Client_DB.Option;
      Source_Handle    : Handle_DB.Option;
      Source_Handle_ID : Handle_ID;
      Handle           : Parpen.Protocol.Handle;
      Node             : Node_DB.Option;
      Cookie           : Parpen.Protocol.Cookie;
      Flags            : Parpen.Protocol.Flat_Binder_Flags;
      Weak             : Boolean;
      use type Handle_DB.Status;
      use type Parpen.Protocol.Binder_Kind;
   begin
      Handle := IBinder_Package.Get_Handle (Context);
      if
         Parpen.Protocol.Handle'Pos (Handle) > Handle_ID'Pos (Handle_ID'Last)
         or Parpen.Protocol.Handle'Pos (Handle) < Handle_ID'Pos (Handle_ID'First)
      then
         Result := Result_Invalid_Handle;
         return;
      end if;

      Source_Handle_ID := Handle_ID'Val (Parpen.Protocol.Handle'Pos (Handle));

      Source := DB.Clients.Get (Source_ID);
      Source_Handle := Source.Data.Handles.Get (Source_Handle_ID);
      if Source_Handle.Result /= Handle_DB.Status_OK then
         Result := Result_Handle_Not_Found;
         return;
      end if;

      Node := DB.Nodes.Get (Source_Handle.Data);
      if Node.Result /= Node_DB.Status_OK then
         Result := Result_Node_Not_Found;
         return;
      end if;

      Weak := IBinder_Package.Get_Kind (Context) = Parpen.Protocol.BK_WEAK_HANDLE;

      if Node.Data.Owner = Dest_ID then
         Cookie := IBinder_Package.Get_Cookie (Context);
         Flags  := IBinder_Package.Get_Flags (Context);

         if Weak then
            IBinder_Package.Set_Kind (Context, Parpen.Protocol.BK_WEAK_BINDER);
         else
            IBinder_Package.Set_Kind (Context, Parpen.Protocol.BK_STRONG_BINDER);
         end if;
         IBinder_Package.Set_Arity (Context, Parpen.Protocol.BA_SINGLE);
         IBinder_Package.Set_Tag (Context, 16#85#);
         IBinder_Package.Set_Flags (Context, Flags);
         IBinder_Package.Set_Binder (Context, Node.Data.Value);
         IBinder_Package.Set_Cookie (Context, Cookie);
         Result := Result_OK;
         return;
      end if;

      Install_Handle (DB      => DB,
                      Context => Context,
                      Dest_ID => Dest_ID,
                      Node    => Node.Position,
                      Weak    => Weak);
      Result := Result_OK;

   end Resolve_Handle;

   -------------
   -- Resolve --
   -------------

   procedure Resolve (DB        : in out Database;
                      Buffer    : in out Types.Bytes_Ptr;
                      Offset    :        Types.Bit_Length;
                      Source_ID :        Client_ID;
                      Dest_ID   :        Client_ID;
                      Result    :    out Result_Type)
   is
      Context : IBinder_Package.Context := IBinder_Package.Create;
      Source : Client_DB.Option;
      Dest   : Client_DB.Option;

      use type Types.Bit_Length;
      use type Client_DB.Status;
      use type Parpen.Protocol.Binder_Kind;
   begin
      Result := Result_Invalid;
      Source := DB.Clients.Get (Source_ID);
      if Source.Result /= Client_DB.Status_OK then
         Result := Result_Invalid_Source;
         return;
      end if;

      Dest := DB.Clients.Get (Dest_ID);
      if Dest.Result /= Client_DB.Status_OK then
         Result := Result_Invalid_Destination;
         return;
      end if;

      IBinder_Package.Initialize (Context,
                                  Buffer,
                                  Types.First_Bit_Index (Buffer'First) + Offset,
                                  Types.Last_Bit_Index (Buffer'Last));
      IBinder_Package.Verify_Message (Context);
      if not IBinder_Package.Valid_Message (Context) then
         return;
      end if;

      if
         IBinder_Package.Get_Kind (Context) = Parpen.Protocol.BK_WEAK_HANDLE
         or IBinder_Package.Get_Kind (Context) = Parpen.Protocol.BK_STRONG_HANDLE
      then
         Resolve_Handle (DB, Context, Source_ID, Dest_ID, Result);
      elsif
         IBinder_Package.Get_Kind (Context) = Parpen.Protocol.BK_WEAK_BINDER
         or IBinder_Package.Get_Kind (Context) = Parpen.Protocol.BK_STRONG_BINDER
      then
         Resolve_Binder (DB, Context, Source_ID, Dest_ID, Result);
      else
         Result := Result_Needless;
      end if;
      IBinder_Package.Take_Buffer (Context, Buffer);
   end Resolve;

end Parpen.Resolve;
