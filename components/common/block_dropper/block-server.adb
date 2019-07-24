
with LSC.Types;
with LSC.SHA1;
with LSC.SHA1_Generic;

package body Block.Server with
   SPARK_Mode
is

   type Cache_Entry is record
      S : Instance.Request;
      C : Instance_Client.Request;
   end record;

   type Request_Cache is array (Request_Id) of Cache_Entry;

   type Hash is array (LSC.SHA1.Hash_Index) of Interfaces.Unsigned_8;
   type Message is array (LSC.Types.Natural_Index range <>) of Interfaces.Unsigned_8;

   function Iterate is new LSC.SHA1_Generic.Hash (LSC.Types.Natural_Index,
                                                  Interfaces.Unsigned_8,
                                                  Message,
                                                  LSC.SHA1.Hash_Index,
                                                  Interfaces.Unsigned_8,
                                                  Hash);

   function To_Message (H : Hash) return Message;

   procedure Allocate (I : out Request_Id;
                       S : out Boolean);

   Cache : Request_Cache := (others => Cache_Entry'(S => Instance.Null_Request,
                                                    C => Instance_Client.Null_Request));

   Hash_Mod   : Interfaces.Unsigned_8;
   Hash_Part  : Interfaces.Unsigned_8;
   Drop_Count : Interfaces.Unsigned_64;
   Dropped    : Interfaces.Unsigned_64;
   Modified   : Interfaces.Unsigned_64;
   Hash_Value : Hash;
   Do_Drop    : Boolean;
   Client     : Types.Client_Session := Instance_Client.Create;


   procedure Allocate (I : out Request_Id;
                       S : out Boolean)
   is
      use type Types.Request_Status;
   begin
      I := Request_Id'First;
      S := False;
      for J in Cache'Range loop
         if Instance.Status (Cache (J).S) = Types.Raw then
            I := J;
            S := True;
            exit;
         end if;
      end loop;
   end Allocate;

   function To_Message (H : Hash) return Message
   is
      Msg : Message (H'First .. H'Last) := (others => 0);
   begin
      for I in Msg'Range loop
         Msg (I) := H (I);
      end loop;
      return Msg;
   end To_Message;

   procedure Eager_Initialize (Capability :     Cai.Types.Capability;
                               Device     :     String;
                               Modulo     :     Interfaces.Unsigned_8;
                               Part       :     Interfaces.Unsigned_8;
                               Count      :     Interfaces.Unsigned_64;
                               Drop       :     Boolean;
                               Success    : out Boolean)
   is
      use type Interfaces.Unsigned_64;
      Null_Hash : Hash := (others => 0);
   begin
      Success := False;
      if not Instance_Client.Initialized (Client) then
         Instance_Client.Initialize (Client, Capability, Device);
      end if;
      if Instance_Client.Initialized (Client) then
         Success := True;
      else
         return;
      end if;
      Hash_Mod   := Modulo;
      Hash_Part  := Part;
      Drop_Count := Count;
      Dropped    := 0;
      Modified   := 0;
      Do_Drop    := Drop;
      Null_Hash (Null_Hash'First .. Null_Hash'First + 2) :=
         (Hash_Mod, Hash_Part, Interfaces.Unsigned_8 (Count mod 256));
      Hash_Value := Iterate (To_Message (Null_Hash));
   end Eager_Initialize;

   procedure Initialize (S : Types.Server_Instance;
                         L : String;
                         B : Types.Byte_Length)
   is
      pragma Unreferenced (S);
      pragma Unreferenced (L);
      pragma Unreferenced (B);
   begin
      null;
   end Initialize;

   procedure Event
   is
      use type Types.Request_Status;
      use type Types.Request_Kind;
      use type Interfaces.Unsigned_8;
      use type Interfaces.Unsigned_64;
      Handle   : Instance_Client.Request_Handle;
      Index    : Request_Id;
      Success  : Boolean;
      Null_Req : Instance.Request := Instance.Null_Request;
   begin
      loop
         Instance_Client.Update_Response_Queue (Client, Handle);
         exit when not Instance_Client.Valid (Handle);
         Index := Instance_Client.Identifier (Handle);
         Instance_Client.Update_Request (Client, Cache (Index).C, Handle);
         if
            Instance_Client.Status (Cache (Index).C) = Types.Ok
            and then Instance_Client.Kind (Cache (Index).C) = Types.Read
         then
            Instance_Client.Read (Client, Cache (Index).C);
         end if;
         Instance.Acknowledge (Server, Cache (Index).S, Instance_Client.Status (Cache (Index).C));
         Instance_Client.Release (Client, Cache (Index).C);
      end loop;
      loop
         if
            Do_Drop
            and ((Dropped > 0 and Dropped <= Drop_Count)
                 or (Dropped = 0 and (Hash_Value (Hash_Value'First) mod Hash_Mod) < Hash_Part))
         then
            Instance.Process (Server, Null_Req);
            exit when Instance.Status (Null_Req) /= Types.Pending;
            Dropped := Dropped + 1;
         else
            Dropped := 0;
            Allocate (Index, Success);
            exit when not Success;
            Instance.Process (Server, Cache (Index).S);
            exit when Instance.Status (Cache (Index).S) /= Types.Pending;
         end if;
         Hash_Value := Iterate (To_Message (Hash_Value));
      end loop;
      for I in Cache'Range loop
         if
            Instance.Status (Cache (I).S) = Types.Pending
            and then Instance_Client.Status (Cache (I).C) = Types.Raw
         then
            Instance_Client.Allocate_Request (Client,
                                              Cache (I).C,
                                              Instance.Kind (Cache (I).S),
                                              Instance.Start (Cache (I).S),
                                              Instance.Length (Cache (I).S),
                                              I);
            exit when Instance_Client.Status (Cache (I).C) /= Types.Allocated;
            Instance_Client.Enqueue (Client, Cache (I).C);
         end if;
      end loop;
      Instance_Client.Submit (Client);
      Instance.Unblock_Client (Server);
   end Event;

   function Block_Count (S : Types.Server_Instance) return Types.Count
   is
      pragma Unreferenced (S);
   begin
      return Instance_Client.Block_Count (Client);
   end Block_Count;

   function Block_Size (S : Types.Server_Instance) return Types.Size
   is
      pragma Unreferenced (S);
   begin
      return Instance_Client.Block_Size (Client);
   end Block_Size;

   function Writable (S : Types.Server_Instance) return Boolean
   is
      pragma Unreferenced (S);
   begin
      return Instance_Client.Writable (Client);
   end Writable;

   function Maximum_Transfer_Size (S : Types.Server_Instance) return Types.Byte_Length
   is
      pragma Unreferenced (S);
   begin
      return Instance_Client.Maximum_Transfer_Size (Client);
   end Maximum_Transfer_Size;

   procedure Finalize (S : Types.Server_Instance)
   is
      pragma Unreferenced (S);
   begin
      if Instance_Client.Initialized (Client) then
         Instance_Client.Finalize (Client);
      end if;
   end Finalize;

   procedure Write (C :     Types.Client_Instance;
                    I :     Request_Id;
                    D : out Buffer)
   is
      pragma Unreferenced (C);
      use type Interfaces.Unsigned_8;
      use type Interfaces.Unsigned_64;
   begin
      Instance.Write (Server, Cache (I).S, D);
      if
         not Do_Drop
         and ((Modified > 0 and Modified <= Drop_Count)
              or (Modified = 0 and (Hash_Value (Hash_Value'First) mod Hash_Mod) < Hash_Part))
      then
         D (D'First) := (if D (D'First) = Byte'First then Byte'Last else Byte'First);
         Modified    := Modified + 1;
      else
         Modified := 0;
      end if;
   end Write;

   procedure Read (C : Types.Client_Instance;
                   I : Request_Id;
                   D : Buffer)
   is
      pragma Unreferenced (C);
      use type Interfaces.Unsigned_8;
      use type Interfaces.Unsigned_64;
      First : constant Byte := (if D (D'First) = Byte'First then Byte'Last else Byte'First);
   begin
      if
         not Do_Drop
         and ((Modified > 0 and Modified <= Drop_Count)
              or (Modified = 0 and (Hash_Value (Hash_Value'First) mod Hash_Mod) < Hash_Part))
      then
         Modified    := Modified + 1;
         Instance.Read (Server, Cache (I).S, First & (D (D'First + 1 .. D'Last)));
      else
         Modified := 0;
         Instance.Read (Server, Cache (I).S, D);
      end if;
   end Read;

end Block.Server;
