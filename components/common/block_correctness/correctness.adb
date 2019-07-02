
with Ada.Unchecked_Conversion;
with LSC.Internal.Types;
with LSC.Internal.SHA256;
with LSC.AES_Generic;
with LSC.AES_Generic.CBC;
with Permutation;
with Output;

use all type LSC.Internal.Types.Word32_Array_Type;
use all type LSC.Internal.SHA256.Message_Index;

package body Correctness
is

   package Write_Permutation is new Permutation (Block.Id);
   package Read_Permutation is new Permutation (Block.Id);

   use all type Block.Count;
   use all type Block.Size;
   use all type Block.Request_Kind;
   use all type Block.Request_Status;

   procedure Write_Recv (C       : in out Block.Client_Session;
                         T       : in out Test_State;
                         Success : in out Boolean;
                         L       : in out Cai.Log.Client_Session) with
      Pre  => Client.Initialized (C) and Cai.Log.Client.Initialized (L),
      Post => Client.Initialized (C) and Cai.Log.Client.Initialized (L)
              and Client.Block_Size (C)'Old = Client.Block_Size (C);

   procedure Write_Send (C : in out Block.Client_Session;
                         T : in out Test_State;
                         L : in out Cai.Log.Client_Session) with
      Pre  => (Client.Initialized (C)
               and then Client.Block_Size (C) mod (LSC.Internal.SHA256.Block_Size / 8) = 0
               and then Client.Block_Size (C) > 0
               and then Client.Block_Size (C) <= Block_Buffer'Length)
              and Cai.Log.Client.Initialized (L)
              and Write_Permutation.Initialized,
      Post => Client.Initialized (C) and Cai.Log.Client.Initialized (L)
              and Client.Block_Size (C)'Old = Client.Block_Size (C);

   procedure Read_Recv (C       : in out Block.Client_Session;
                        T       : in out Test_State;
                        Success : in out Boolean;
                        L       : in out Cai.Log.Client_Session) with
      Pre  => Client.Initialized (C) and Cai.Log.Client.Initialized (L),
      Post => Client.Initialized (C) and Cai.Log.Client.Initialized (L)
              and Client.Block_Size (C)'Old = Client.Block_Size (C);

   procedure Read_Send (C       : in out Block.Client_Session;
                        T       : in out Test_State;
                        Success : in out Boolean;
                        L       : in out Cai.Log.Client_Session) with
      Pre  => Client.Initialized (C) and Cai.Log.Client.Initialized (L)
              and Read_Permutation.Initialized,
      Post => Client.Initialized (C) and Cai.Log.Client.Initialized (L)
              and Client.Block_Size (C)'Old = Client.Block_Size (C);

   procedure Hash_Block (Context : in out LSC.Internal.SHA256.Context_Type;
                         Buffer  :        Block.Buffer) with
      Pre => Buffer'Length mod (LSC.Internal.SHA256.Block_Size / 8) = 0;

   procedure Generate_Block (S :     Block.Id;
                             B : out Block.Buffer) with
      Pre => B'Length <= Block_Buffer'Length
             and B'Length > 0
             and B'Length mod 16 = 0;

   procedure Update_Write_Cache (T : in out Test_State;
                                 B :        Block.Size) with
      Pre => Write_Permutation.Initialized
             and B <= Block.Size (Block_Buffer'Last)
             and B > 0
             and B mod (LSC.Internal.SHA256.Block_Size / 8) = 0;

   Start        : Cai.Timer.Time;
   Last         : Cai.Timer.Time;
   Write_Buffer : Block_Buffer;
   Last_Context : LSC.Internal.SHA256.Context_Type;

   function State_Initialized return Boolean is
      (Write_Permutation.Initialized and Read_Permutation.Initialized);

   procedure Update_Write_Cache (T : in out Test_State;
                                 B :        Block.Size)
   is
      W : Write_Cache;
      L : Block.Id;
   begin
      W.Context := Last_Context;
      W.Buffer  := (others => Block.Byte'First);
      loop
         exit when not Write_Permutation.Has_Element or not Write_Ring.Free (T.Write_Data);
         Write_Permutation.Next (L);
         exit when Write_Ring.Has_Block (T.Write_Data, L);
         Write_Ring.Add (T.Write_Data, L);
         Generate_Block (L, W.Buffer (1 .. Block.Count'(1) * B));
         Hash_Block (W.Context, W.Buffer (1 .. Block.Count'(1) * B));
         Write_Ring.Set_Data (T.Write_Data, L, W);
         Last_Context := W.Context;
      end loop;
   end Update_Write_Cache;

   procedure Initialize (C   :        Block.Client_Session;
                         T   :    out Test_State;
                         Max :        Block.Count)
   is
   begin
      T.Last           := Block.Id'Last;
      T.Last_Finished  := True;
      T.Sent           := 0;
      T.Written        := 0;
      T.Read           := 0;
      T.Count          := Max;
      T.Bounds_Checked := False;
      T.Compared       := False;
      T.Write_Context  := LSC.Internal.SHA256.SHA256_Context_Init;
      T.Read_Context   := LSC.Internal.SHA256.SHA256_Context_Init;
      Read_Ring.Initialize (T.Read_Data);
      Write_Ring.Initialize (T.Write_Data);
      Write_Permutation.Initialize (Block.Id (Max - 1));
      Read_Permutation.Initialize (Block.Id (Max - 1));
      Last_Context := T.Write_Context;
      Update_Write_Cache (T, Client.Block_Size (C));
   end Initialize;

   procedure Bounds_Check (C       : in out Block.Client_Session;
                           T       : in out Test_State;
                           Success :    out Boolean;
                           L       : in out Cai.Log.Client_Session;
                           Timer   :        Cai.Timer.Client_Session)
   is
      Request : constant Client.Request := (Kind   => Block.Read,
                                            Priv   => Block.Null_Data,
                                            Start  => Block.Id (Client.Block_Count (C)),
                                            Length => 1,
                                            Status => Block.Raw);
      R : Client.Request := Client.Next (C);
   begin
      Success := True;
      if R.Kind = Block.Read then
         Success := R.Status = Block.Error;
         if not Success then
            Cai.Log.Client.Error (L, "Bounds check failed, block "
                                     & Cai.Log.Image (Cai.Log.Unsigned (R.Start))
                                     & " should not be: "
                                     & (case R.Status is
                                        when Block.Raw          => "Raw",
                                        when Block.Ok           => "Ok",
                                        when Block.Error        => "Error",
                                        when Block.Acknowledged => "Acknowledged"));
         end if;
         pragma Warnings (Off, "unused assignment to ""R""");
         Client.Release (C, R);
         pragma Warnings (On, "unused assignment to ""R""");
         T.Bounds_Checked := True;
         return;
      end if;
      if Client.Supported (C, Request.Kind) and then Client.Ready (C, Request) then
         Client.Enqueue (C, Request);
         Client.Submit (C);
         Start := Timer_Client.Clock (Timer);
         Last  := Timer_Client.Clock (Timer);
      end if;
   end Bounds_Check;

   function Bounds_Check_Finished (T : Test_State) return Boolean
   is
   begin
      return T.Bounds_Checked;
   end Bounds_Check_Finished;

   procedure Hash_Block (Context : in out LSC.Internal.SHA256.Context_Type;
                         Buffer  :        Block.Buffer)
   is
      subtype Block_Message is LSC.Internal.SHA256.Message_Type
         (1 .. Buffer'Length / (LSC.Internal.SHA256.Block_Size / 8));
      subtype Sub_Block is Block.Buffer (1 .. Buffer'Length);
      function Convert_Block is new Ada.Unchecked_Conversion (Sub_Block, Block_Message);
      Message : constant Block_Message := Convert_Block (Buffer);
   begin
      for B of Message loop
         LSC.Internal.SHA256.Context_Update (Context, B);
      end loop;
   end Hash_Block;

   procedure Write_Recv (C       : in out Block.Client_Session;
                         T       : in out Test_State;
                         Success : in out Boolean;
                         L       : in out Cai.Log.Client_Session)
   is
      Size : constant Block.Size := Client.Block_Size (C) with Ghost;
   begin
      while T.Written < T.Count loop
         pragma Loop_Invariant (Client.Initialized (C));
         pragma Loop_Invariant (Cai.Log.Client.Initialized (L));
         pragma Loop_Invariant (Client.Block_Size (C) = Size);
         declare
            R : Client.Request := Client.Next (C);
         begin
            if R.Kind = Block.Write then
               T.Written := T.Written + 1;
               Success   := R.Status = Block.Ok;
               if not Success then
                  Cai.Log.Client.Error (L, "Write received erroneous request "
                                           & Cai.Log.Image (Cai.Log.Unsigned (R.Start)) & "/"
                                           & Cai.Log.Image (Long_Integer (Client.Block_Count (C))));
               end if;
               pragma Warnings (Off, "unused assignment to ""R""");
               Client.Release (C, R);
               pragma Warnings (On, "unused assignment to ""R""");
            else
               exit;
            end if;
         end;
      end loop;
   end Write_Recv;

   procedure Write_Send (C : in out Block.Client_Session;
                         T : in out Test_State;
                         L : in out Cai.Log.Client_Session)
   is
      Request : Client.Request := (Kind   => Block.Write,
                                   Priv   => Block.Null_Data,
                                   Start  => 0,
                                   Length => 1,
                                   Status => Block.Raw);
      W : Write_Cache;
      Size : constant Block.Size := Client.Block_Size (C) with Ghost;
   begin
      if T.Sent < T.Count and Client.Supported (C, Request.Kind) then
         loop
            pragma Loop_Invariant (Client.Initialized (C));
            pragma Loop_Invariant (Cai.Log.Client.Initialized (L));
            pragma Loop_Invariant (Client.Supported (C, Request.Kind));
            pragma Loop_Invariant (Client.Block_Size (C) = Size);
            exit when T.Sent >= T.Count;
            if T.Last_Finished then
               if Write_Ring.Block_Ready (T.Write_Data) then
                  Request.Start := Write_Ring.Block_Peek (T.Write_Data);
               else
                  Cai.Log.Client.Warning (L, "Write cache depleted, generating block on demand...");
                  if Write_Permutation.Has_Element then
                     Write_Permutation.Next (Request.Start);
                  else
                     Request.Start := T.Last + 1;
                     Cai.Log.Client.Error (L, "Block permutation exceeded, increasing block number (Write).");
                  end if;
               end if;
               T.Last := Request.Start;
               T.Last_Finished := False;
            else
               Request.Start := T.Last;
            end if;
            exit when not Client.Ready (C, Request);
            if Write_Ring.Block_Ready (T.Write_Data) then
               Write_Ring.Get_Block (T.Write_Data, W);
               T.Write_Context := W.Context;
               Write_Buffer := W.Buffer;
            else
               Generate_Block (Request.Start, Write_Buffer);
               Hash_Block (T.Write_Context, Write_Buffer (1 .. Block.Count'(1) * Client.Block_Size (C)));
               Last_Context := T.Write_Context;
            end if;
            Client.Enqueue (C, Request);
            T.Sent := T.Sent + 1;
            T.Last_Finished := True;
         end loop;
         Client.Submit (C);
      end if;
   end Write_Send;

   procedure Write (C       : in out Block.Client_Session;
                    T       : in out Test_State;
                    Success :    out Boolean;
                    L       : in out Cai.Log.Client_Session;
                    Timer   :        Cai.Timer.Client_Session)
   is
      Current : Cai.Timer.Time;
   begin
      Success := True;
      Write_Recv (C, T, Success, L);
      Write_Send (C, T, L);
      Update_Write_Cache (T, Client.Block_Size (C));
      Current := Timer_Client.Clock (Timer);
      Output.Progress ("Writing",
                       Long_Integer (T.Written),
                       (if
                           Long_Integer'Last / 2 > Long_Integer (T.Count)
                        then
                           Long_Integer (T.Count) * 2
                        else
                           Long_Integer'Last),
                       Long_Integer (Client.Block_Size (C)),
                       Start,
                       Current,
                       Last,
                       L);
      if Write_Finished (T) then
         T.Sent := 0;
         T.Last := Block.Id'Last;
      end if;
   end Write;

   function Write_Finished (T : Test_State) return Boolean
   is
   begin
      return T.Written = T.Count;
   end Write_Finished;

   procedure Read_Recv (C       : in out Block.Client_Session;
                        T       : in out Test_State;
                        Success : in out Boolean;
                        L       : in out Cai.Log.Client_Session)
   is
      Size : constant Block.Size := Client.Block_Size (C) with Ghost;
   begin
      while T.Read < T.Count loop
         pragma Loop_Invariant (Client.Initialized (C));
         pragma Loop_Invariant (Cai.Log.Client.Initialized (L));
         pragma Loop_Invariant (Client.Block_Size (C) = Size);
         declare
            R : Client.Request := Client.Next (C);
         begin
            if R.Kind = Block.Read then
               if R.Status = Block.Ok and then Read_Ring.Has_Block (T.Read_Data, R.Start) then
                  Client.Read (C, R);
               else
                  Cai.Log.Client.Error (L, "Read received erroneous request "
                                           & Cai.Log.Image (Cai.Log.Unsigned (R.Start)) & "/"
                                           & Cai.Log.Image (Long_Integer (Client.Block_Count (C))));
                  Success := False;
               end if;
               T.Read := T.Read + 1;
               pragma Warnings (Off, "unused assignment to ""R""");
               Client.Release (C, R);
               pragma Warnings (On, "unused assignment to ""R""");
            else
               exit;
            end if;
         end;
      end loop;
   end Read_Recv;

   procedure Read_Send (C       : in out Block.Client_Session;
                        T       : in out Test_State;
                        Success : in out Boolean;
                        L       : in out Cai.Log.Client_Session)
   is
      Request : Client.Request := (Kind   => Block.Read,
                                   Priv   => Block.Null_Data,
                                   Start  => 0,
                                   Length => 1,
                                   Status => Block.Raw);
      Size : constant Block.Size := Client.Block_Size (C) with Ghost;
   begin
      if T.Sent < T.Count and Client.Supported (C, Request.Kind) then
         loop
            pragma Loop_Invariant (Client.Initialized (C));
            pragma Loop_Invariant (Cai.Log.Client.Initialized (L));
            pragma Loop_Invariant (Client.Supported (C, Request.Kind));
            pragma Loop_Invariant (Client.Block_Size (C) = Size);
            exit when not Read_Ring.Free (T.Read_Data) or T.Sent >= T.Count;
            if T.Last_Finished then
               if Read_Permutation.Has_Element then
                  Read_Permutation.Next (Request.Start);
               else
                  Request.Start := T.Last + 1;
                  Cai.Log.Client.Error (L, "Block permutation exceeded, increasing block number (Read).");
               end if;
               T.Last_Finished := False;
               T.Last := Request.Start;
            else
               Request.Start := T.Last;
            end if;
            exit when not Client.Ready (C, Request);
            Client.Enqueue (C, Request);
            if not Read_Ring.Has_Block (T.Read_Data, Request.Start) then
               Read_Ring.Add (T.Read_Data, Request.Start);
            else
               Cai.Log.Client.Error (L, "Tried to insert duplicated block: "
                                        & Cai.Log.Image (Cai.Log.Unsigned (Request.Start)));
               Success := False;
            end if;
            T.Sent := T.Sent + 1;
            T.Last_Finished := True;
         end loop;
         Client.Submit (C);
      end if;
   end Read_Send;

   procedure Read (C       : in out Block.Client_Session;
                   T       : in out Test_State;
                   Success :    out Boolean;
                   L       : in out Cai.Log.Client_Session;
                   Timer   :        Cai.Timer.Client_Session)
   is
      Current : Cai.Timer.Time;
      Done : Long_Integer;
      Todo : Long_Integer;
   begin
      Success := True;
      Read_Recv (C, T, Success, L);
      Read_Send (C, T, Success, L);
      while Read_Ring.Block_Ready (T.Read_Data) loop
         declare
            Buf : Block_Buffer;
         begin
            Read_Ring.Get_Block (T.Read_Data, Buf);
            Hash_Block (T.Read_Context, Buf (1 .. Block.Count'(1) * Client.Block_Size (C)));
         end;
      end loop;
      if Timer_Client.Initialized (Timer) then
         Current := Timer_Client.Clock (Timer);
      else
         Current := Cai.Timer.Time'First;
      end if;
      if Block.Count'Last - T.Read > T.Written then
         Done := Long_Integer (T.Written + T.Read);
      else
         Done := Long_Integer'Last;
      end if;
      if Block.Count'Last / 2 > T.Count then
         Todo := Long_Integer (T.Count) * 2;
      else
         Todo := Long_Integer'Last;
      end if;
      Output.Progress ("Reading",
                       Done,
                       Todo,
                       Long_Integer (Client.Block_Size (C)),
                       Start,
                       Current,
                       Last,
                       L);
   end Read;

   function Read_Finished (T : Test_State) return Boolean
   is
   begin
      return T.Read = T.Count;
   end Read_Finished;

   procedure Compare (T     : in out Test_State;
                      Equal :    out Boolean)
   is
      B : constant LSC.Internal.SHA256.Block_Type := (others => 0);
   begin
      LSC.Internal.SHA256.Context_Finalize (T.Write_Context, B, 0);
      LSC.Internal.SHA256.Context_Finalize (T.Read_Context, B, 0);
      Equal := LSC.Internal.SHA256.SHA256_Get_Hash (T.Write_Context) =
               LSC.Internal.SHA256.SHA256_Get_Hash (T.Read_Context);
      T.Compared := True;
   end Compare;

   function Compare_Finished (T : Test_State) return Boolean
   is
   begin
      return T.Compared;
   end Compare_Finished;

   procedure Block_Read (T : in out Test_State;
                         S :        Block.Id;
                         D :        Block.Buffer)
   is
      Padded : Block_Buffer := (others => Block.Byte'First);
   begin
      Padded (Padded'First .. Padded'First + D'Length - 1) := D;
      Read_Ring.Set_Data (T.Read_Data, S, Padded);
   end Block_Read;

   procedure Generate_Block (S :     Block.Id;
                             B : out Block.Buffer)
   is
      function CBC_Key is new LSC.AES_Generic.Enc_Key (Block.Buffer_Index,
                                                       Block.Byte,
                                                       Block.Buffer);
      procedure CBC is new LSC.AES_Generic.CBC.Encrypt (Block.Buffer_Index,
                                                        Block.Byte,
                                                        Block.Buffer,
                                                        Block.Buffer_Index,
                                                        Block.Byte,
                                                        Block.Buffer);
      subtype Id is Block.Buffer (1 .. 8);
      function Convert_Id is new Ada.Unchecked_Conversion (Block.Id, Id);
      Null_Block : constant Block.Buffer (B'First .. B'Last) := (others => Block.Byte'First);
      IV : Block.Buffer (1 .. 16) := (others => Block.Byte'First);
      Key : constant Block.Buffer (1 .. 16) := (others => Block.Byte'Val (16#42#));
      --  This is no cryptographically secure encryption and only used to generate pseudo random blocks
   begin
      IV (1 .. 8) := Convert_Id (S);
      CBC (Null_Block, IV, CBC_Key (Key, LSC.AES_Generic.L128), B);
   end Generate_Block;

   procedure Block_Write (T : in out Test_State;
                          S :        Block.Id;
                          D :    out Block.Buffer)
   is
      pragma Unreferenced (T);
      pragma Unreferenced (S);
   begin
      D := Write_Buffer (1 .. D'Length);
   end Block_Write;

end Correctness;
