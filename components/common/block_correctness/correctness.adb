
with Ada.Unchecked_Conversion;
with Cai.Log.Client;
with Cai.Timer;
with Cai.Timer.Client;
with LSC.Internal.Types;
with LSC.Internal.SHA256;
with Output;

use all type LSC.Internal.Types.Word32_Array_Type;
use all type LSC.Internal.SHA256.Message_Index;

package body Correctness is

   use all type Block.Count;
   use all type Block.Size;
   use all type Block.Request_Kind;
   use all type Block.Request_Status;

   Timer : Cai.Timer.Client_Session := Cai.Timer.Client.Create;

   procedure Initialize (C   : in out Block.Client_Session;
                         T   :    out Test_State;
                         L   : in out Cai.Log.Client_Session;
                         Cap :        Cai.Types.Capability;
                         Max :        Block.Count)
   is
   begin
      T.Last           := Block.Id'Last;
      T.Sent           := 0;
      T.Written        := 0;
      T.Read           := 0;
      T.Count          := Max;
      T.Bounds_Checked := False;
      T.Write_Context  := LSC.Internal.SHA256.SHA256_Context_Init;
      T.Read_Context   := LSC.Internal.SHA256.SHA256_Context_Init;
      if Client.Block_Size (C) > 4096 then
         Cai.Log.Client.Warning (L, "Block size "
                                    & Cai.Log.Image (Long_Integer (Client.Block_Size (C)))
                                    & " is too large, requests might fail");
      end if;
      Cai.Timer.Client.Initialize (Timer, Cap);
      Ring.Initialize (T.Data);
   end Initialize;

   Start    : Cai.Timer.Time;
   Last     : Cai.Timer.Time;

   procedure Bounds_Check (C       : in out Block.Client_Session;
                           T       : in out Test_State;
                           Success :    out Boolean;
                           L       : in out Cai.Log.Client_Session)
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
                                     & Cai.Log.Image (Long_Integer (R.Start))
                                     & " should not be: "
                                     & (case R.Status is
                                        when Block.Raw          => "Raw",
                                        when Block.Ok           => "Ok",
                                        when Block.Error        => "Error",
                                        when Block.Acknowledged => "Acknowledged"));
         end if;
         Client.Release (C, R);
         T.Bounds_Checked := True;
         return;
      end if;
      while not Client.Ready (C, Request) loop
         null;
      end loop;
      Client.Enqueue (C, Request);
      Client.Submit (C);
      Start := Cai.Timer.Client.Clock (Timer);
      Last := Cai.Timer.Client.Clock (Timer);
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
                         L       : in out Cai.Log.Client_Session);

   procedure Write_Recv (C       : in out Block.Client_Session;
                         T       : in out Test_State;
                         Success : in out Boolean;
                         L       : in out Cai.Log.Client_Session)
   is
   begin
      while T.Written < T.Count loop
         declare
            R : Client.Request := Client.Next (C);
         begin
            if R.Kind = Block.Write then
               T.Written := T.Written + 1;
               Success   := R.Status = Block.Ok;
               if not Success then
                  Cai.Log.Client.Error (L, "Write received erroneous request "
                                           & Cai.Log.Image (Long_Integer (R.Start)) & "/"
                                           & Cai.Log.Image (Long_Integer (Client.Block_Count (C))));
               end if;
               Client.Release (C, R);
            else
               exit;
            end if;
         end;
      end loop;
   end Write_Recv;

   procedure Write_Send (C : in out Block.Client_Session;
                         T : in out Test_State);

   procedure Write_Send (C : in out Block.Client_Session;
                         T : in out Test_State)
   is
      Request : Client.Request := (Kind   => Block.Write,
                                   Priv   => Block.Null_Data,
                                   Start  => 0,
                                   Length => 1,
                                   Status => Block.Raw);
   begin
      if T.Sent < T.Count then
         loop
            exit when not Client.Ready (C, Request) or T.Sent >= T.Count;
            Request.Start := Next (T.Last);
            Client.Enqueue (C, Request);
            T.Sent := T.Sent + 1;
            T.Last := Request.Start;
         end loop;
         Client.Submit (C);
      end if;
   end Write_Send;

   procedure Write (C       : in out Block.Client_Session;
                    T       : in out Test_State;
                    Success :    out Boolean;
                    L       : in out Cai.Log.Client_Session)
   is
   begin
      Success := True;
      Write_Recv (C, T, Success, L);
      Write_Send (C, T);
      Output.Progress ("Writing",
                       Long_Integer (T.Written),
                       Long_Integer (T.Count),
                       Long_Integer (Client.Block_Size (C)),
                       Start,
                       Cai.Timer.Client.Clock (Timer),
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
                        L       : in out Cai.Log.Client_Session);

   procedure Read_Recv (C       : in out Block.Client_Session;
                        T       : in out Test_State;
                        Success : in out Boolean;
                        L       : in out Cai.Log.Client_Session)
   is
   begin
      while T.Read < T.Count loop
         declare
            R : Client.Request := Client.Next (C);
         begin
            if R.Kind = Block.Read then
               if R.Status = Block.Ok and then Ring.Has_Block (T.Data, R.Start) then
                  Client.Read (C, R);
               else
                  Cai.Log.Client.Error (L, "Read received erroneous request "
                                           & Cai.Log.Image (Long_Integer (R.Start)) & "/"
                                           & Cai.Log.Image (Long_Integer (Client.Block_Count (C))));
                  Success := False;
               end if;
               T.Read := T.Read + 1;
               Client.Release (C, R);
            else
               exit;
            end if;
         end;
      end loop;
   end Read_Recv;

   procedure Cache_Data (T : in out Test_State;
                         S : Block.Id;
                         B : Block.Buffer)
   is
      use type Block.Buffer;
      Pad : constant Block.Buffer (1 .. Block_Buffer'Length - B'Length) := (others => Block.Byte'First);
   begin
      Ring.Set_Data (T.Data, S, B & Pad);
   end Cache_Data;

   procedure Read_Send (C       : in out Block.Client_Session;
                        T       : in out Test_State;
                        Success : in out Boolean;
                        L       : in out Cai.Log.Client_Session);

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
   begin
      if T.Sent < T.Count then
         loop
            exit when not Client.Ready (C, Request) or not Ring.Free (T.Data) or T.Sent >= T.Count;
            Request.Start := Next (T.Last);
            Client.Enqueue (C, Request);
            if not Ring.Has_Block (T.Data, Request.Start) then
               Ring.Add (T.Data, Request.Start);
            else
               Cai.Log.Client.Error (L, "Tried to insert duplicated block");
               Success := False;
            end if;
            T.Sent := T.Sent + 1;
            T.Last := Request.Start;
         end loop;
         Client.Submit (C);
      end if;
   end Read_Send;

   procedure Read (C       : in out Block.Client_Session;
                   T       : in out Test_State;
                   Success :    out Boolean;
                   L       : in out Cai.Log.Client_Session)
   is
   begin
      Success := True;
      Read_Recv (C, T, Success, L);
      Read_Send (C, T, Success, L);
      while Ring.Block_Ready (T.Data) loop
         declare
            Buf : Block_Buffer;
            Id  : Block.Id;
         begin
            Ring.Get_Block (T.Data, Id, Buf);
            Hash_Block (T.Read_Context, Buf (1 .. Block.Count (1) * Client.Block_Size (C)));
         end;
      end loop;
      Output.Progress ("Reading",
                       Long_Integer (T.Read),
                       Long_Integer (T.Count),
                       Long_Integer (Client.Block_Size (C)),
                       Start,
                       Cai.Timer.Client.Clock (Timer),
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

end Correctness;
