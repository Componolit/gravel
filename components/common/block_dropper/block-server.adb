
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

   Cache : Request_Cache;

   Hash_Mod   : Interfaces.Unsigned_8;
   Hash_Part  : Interfaces.Unsigned_8;
   Drop_Count : Interfaces.Unsigned_64;
   Dropped    : Interfaces.Unsigned_64;
   Rejected   : Interfaces.Unsigned_64;
   Modified   : Interfaces.Unsigned_64;
   Hash_Value : Hash;
   Do_Op      : Operation;
   Inited     : Boolean              := False;
   Client     : Types.Client_Session;

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
                               Op         :     Operation;
                               Success    : out Boolean)
   is
      use type Interfaces.Unsigned_64;
      Null_Hash : Hash := (others => 0);
   begin
      Success := False;
      if not Types.Initialized (Client) then
         Instance_Client.Initialize (Client, Capability, Device, True);
      end if;
      if Types.Initialized (Client) then
         Success := True;
      else
         return;
      end if;
      Hash_Mod   := Modulo;
      Hash_Part  := Part;
      Drop_Count := Count;
      Dropped    := 0;
      Rejected   := 0;
      Modified   := 0;
      Do_Op      := Op;
      Null_Hash (Null_Hash'First .. Null_Hash'First + 2) :=
         (Hash_Mod, Hash_Part, Interfaces.Unsigned_8 (Count mod 256));
      Hash_Value := Iterate (To_Message (Null_Hash));
   end Eager_Initialize;

   procedure Initialize (S : in out Types.Server_Session;
                         L :        String;
                         B :        Types.Byte_Length)
   is
      pragma Unreferenced (S);
      pragma Unreferenced (L);
      pragma Unreferenced (B);
   begin
      Inited := True;
   end Initialize;

   procedure Event
   is
      use type Types.Request_Status;
      use type Types.Request_Kind;
      use type Types.Result;
      use type Interfaces.Unsigned_8;
      use type Interfaces.Unsigned_64;
      Result   : Types.Result;
      Null_Req : Instance.Request;
   begin
      for I in Cache'Range loop
         if Instance.Status (Cache (I).S) = Types.Pending then
            if Instance_Client.Status (Cache (I).C) = Types.Pending then
               Instance_Client.Update_Request (Client, Cache (I).C);
            end if;
            if Instance_Client.Status (Cache (I).C) = Types.Ok then
               if Instance_Client.Kind (Cache (I).C) = Types.Read then
                  Instance_Client.Read (Client, Cache (I).C);
               end if;
               Instance.Acknowledge (Server, Cache (I).S, Instance_Client.Status (Cache (I).C));
            elsif Instance_Client.Status (Cache (I).C) = Types.Error then
               Instance.Acknowledge (Server, Cache (I).S, Instance_Client.Status (Cache (I).C));
            end if;
         end if;

         if Instance.Status (Cache (I).S) = Types.Raw then
            if Instance_Client.Status (Cache (I).C) /= Types.Raw then
               Instance_Client.Release (Client, Cache (I).C);
            end if;
            if
               Do_Op = Drop
               and ((Dropped > 0 and Dropped <= Drop_Count)
                    or (Dropped = 0 and (Hash_Value (Hash_Value'First) mod Hash_Mod) < Hash_Part))
            then
               Instance.Process (Server, Null_Req);
               Dropped := Dropped + 1;
            else
               Dropped := 0;
               Instance.Process (Server, Cache (I).S);
            end if;
         end if;

         if Instance.Status (Cache (I).S) = Types.Pending then
            if
               Do_Op = Reject
               and ((Rejected > 0 and Rejected <= Drop_Count)
                    or (Rejected = 0 and (Hash_Value (Hash_Value'First) mod Hash_Mod) < Hash_Part))
            then
               Instance.Acknowledge (Server, Cache (I).S, Types.Error);
               Rejected := Rejected + 1;
            else
               Rejected := 0;
               if Instance_Client.Status (Cache (I).C) = Types.Raw then
                  Instance_Client.Allocate_Request (Client,
                                                    Cache (I).C,
                                                    Instance.Kind (Cache (I).S),
                                                    Instance.Start (Cache (I).S),
                                                    Instance.Length (Cache (I).S),
                                                    I, Result);
                  if
                     Result = Types.Success
                     and then Instance_Client.Kind (Cache (I).C) = Types.Write
                  then
                     Instance_Client.Write (Client, Cache (I).C);
                  end if;
               end if;
               if Instance_Client.Status (Cache (I).C) = Types.Allocated then
                  Instance_Client.Enqueue (Client, Cache (I).C);
               end if;
            end if;
         end if;
         Hash_Value := Iterate (To_Message (Hash_Value));
      end loop;
      Instance_Client.Submit (Client);
      Instance.Unblock_Client (Server);
   end Event;

   function Block_Count (S : Types.Server_Session) return Types.Count
   is
      pragma Unreferenced (S);
   begin
      return Types.Block_Count (Client);
   end Block_Count;

   function Block_Size (S : Types.Server_Session) return Types.Size
   is
      pragma Unreferenced (S);
   begin
      return Types.Block_Size (Client);
   end Block_Size;

   function Writable (S : Types.Server_Session) return Boolean
   is
      pragma Unreferenced (S);
   begin
      return Types.Writable (Client);
   end Writable;

   procedure Finalize (S : in out Types.Server_Session)
   is
      pragma Unreferenced (S);
   begin
      Inited := False;
   end Finalize;

   procedure Write (C : in out Types.Client_Session;
                    I :        Request_Id;
                    D :    out Buffer)
   is
      pragma Unreferenced (C);
      use type Interfaces.Unsigned_8;
      use type Interfaces.Unsigned_64;
   begin
      Instance.Write (Server, Cache (I).S, D, 0);
      if
         Do_Op = Modify
         and ((Modified > 0 and Modified <= Drop_Count)
              or (Modified = 0 and (Hash_Value (Hash_Value'First) mod Hash_Mod) < Hash_Part))
      then
         D (D'First) := (if D (D'First) = Byte'First then Byte'Last else Byte'First);
         Modified    := Modified + 1;
      else
         Modified := 0;
      end if;
   end Write;

   procedure Read (C : in out Types.Client_Session;
                   I :        Request_Id;
                   D :        Buffer)
   is
      pragma Unreferenced (C);
      use type Interfaces.Unsigned_8;
      use type Interfaces.Unsigned_64;
      First : constant Byte := (if D (D'First) = Byte'First then Byte'Last else Byte'First);
   begin
      if
         Do_Op = Modify
         and ((Modified > 0 and Modified <= Drop_Count)
              or (Modified = 0 and (Hash_Value (Hash_Value'First) mod Hash_Mod) < Hash_Part))
      then
         Modified    := Modified + 1;
         Instance.Read (Server, Cache (I).S, First & (D (D'First + 1 .. D'Last)), 0);
      else
         Modified := 0;
         Instance.Read (Server, Cache (I).S, D, 0);
      end if;
   end Read;

   function Initialized (S : Types.Server_Session) return Boolean
   is
      pragma Unreferenced (S);
   begin
      return Inited;
   end Initialized;

   procedure Read (S : in out Types.Server_Session;
                   I :        Request_Id;
                   B :    out Buffer)
   is
      pragma Unreferenced (S);
      pragma Unreferenced (I);
   begin
      B := (others => 0);
   end Read;

   procedure Write (S : in out Types.Server_Session;
                    I :        Request_Id;
                    B :        Buffer)
   is
      pragma Unreferenced (S);
      pragma Unreferenced (I);
      pragma Unreferenced (B);
   begin
      null;
   end Write;

end Block.Server;
