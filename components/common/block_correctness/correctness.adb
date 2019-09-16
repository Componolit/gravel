
with Ada.Unchecked_Conversion;
with LSC.Internal.Types;
with LSC.Internal.SHA256;
with LSC.AES_Generic;
with LSC.AES_Generic.CBC;
with Componolit.Gneiss.Log.Client;
with Componolit.Gneiss.Strings_Generic;
with Permutation;
with Output;

use all type LSC.Internal.Types.Word32_Array_Type;
use all type LSC.Internal.SHA256.Message_Index;

package body Correctness
is

   function Image is new Cai.Strings_Generic.Image_Modular (Block.Id);

   package Write_Permutation is new Permutation (Block.Id);
   package Read_Permutation is new Permutation (Block.Id);

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

   procedure Update_Read_Cache (T : in out Test_State);

   Start        : Cai.Timer.Time;
   Last         : Cai.Timer.Time;

   function State_Initialized return Boolean is
      (Write_Permutation.Initialized and Read_Permutation.Initialized);

   procedure Update_Write_Cache (T : in out Test_State;
                                 B :        Block.Size)
   is
   begin
      while
         not Cache (T.Write).P
         and Write_Permutation.Has_Element
         and T.Generated < T.Count
      loop
         Cache (T.Write).P := True;
         T.Generated := T.Generated + 1;
         Write_Permutation.Next (Cache (T.Write).S);
         Generate_Block (Cache (T.Write).S, Cache (T.Write).B (1 .. Block.Count'(1) * B));
         Hash_Block (T.Write_Context, Cache (T.Write).B (1 .. Block.Count'(1) * B));
         T.Write := Block.Request_Id'Succ (T.Write);
      end loop;
   end Update_Write_Cache;

   procedure Update_Read_Cache (T : in out Test_State)
   is
   begin
      while
         not Cache (T.Write).P
         and Read_Permutation.Has_Element
         and T.Generated < T.Count
      loop
         Cache (T.Write).P := True;
         T.Generated := T.Generated + 1;
         Read_Permutation.Next (Cache (T.Write).S);
         T.Write := Block.Request_Id'Succ (T.Write);
      end loop;
   end Update_Read_Cache;

   procedure Initialize (C   :        Block.Client_Session;
                         T   :    out Test_State;
                         Max :        Block.Count)
   is
   begin
      T.Write          := Block.Request_Id'First;
      T.Read           := Block.Request_Id'First;
      T.Generated      := 0;
      T.Count          := Max;
      T.Bounds_Checked := False;
      T.Compared       := False;
      T.Write_Context  := LSC.Internal.SHA256.SHA256_Context_Init;
      T.Read_Context   := LSC.Internal.SHA256.SHA256_Context_Init;
      Write_Permutation.Initialize (Block.Id (Max - 1));
      Read_Permutation.Initialize (Block.Id (Max - 1));
      Update_Write_Cache (T, Block.Block_Size (C));
      Update_Read_Cache (T);
   end Initialize;

   procedure Bounds_Check (C       : in out Block.Client_Session;
                           T       : in out Test_State;
                           Success :    out Boolean;
                           L       : in out Cai.Log.Client_Session;
                           Timer   :        Cai.Timer.Client_Session)
   is
      use type Block.Request_Status;
      Result : Block.Result;
   begin
      Success := True;
      if Client.Status (Cache (Cache'First).R) = Block.Pending then
         Client.Update_Request (C, Cache(Cache'First).R);
      end if;
      if Client.Status (Cache (Cache'First).R) in Block.Ok | Block.Error then
         T.Bounds_Checked := True;
         Success := Client.Status (Cache (Cache'First).R) = Block.Error;
         if not Success then
            Cai.Log.Client.Error (L, "Bounds check failed, block "
                                     & Image (Client.Start (Cache (Cache'First).R))
                                     & " should not be: "
                                     & (case Client.Status (Cache (Cache'First).R) is
                                        when Block.Raw          => "Raw",
                                        when Block.Allocated    => "Allocated",
                                        when Block.Pending      => "Pending",
                                        when Block.Ok           => "Ok",
                                        when Block.Error        => "Error"));
         end if;
         Client.Release (C, Cache (Cache'First).R);
      end if;
      if
         Client.Status (Cache (Cache'First).R) = Block.Raw
         and not T.Bounds_Checked
      then
         Client.Allocate_Request (C,
                                  Cache (Cache'First).R,
                                  Block.Read,
                                  Block.Id (Block.Block_Count (C)),
                                  1,
                                  Cache'First,
                                  Result);
      end if;
      if Client.Status (Cache (Cache'First).R) = Block.Allocated then
         Client.Enqueue (C, Cache (Cache'First).R);
         if Client.Status (Cache (Cache'First).R) = Block.Pending then
            Start := Timer_Client.Clock (Timer);
            Last  := Timer_Client.Clock (Timer);
         end if;
      end if;
      Client.Submit (C);
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

   procedure Write (C       : in out Block.Client_Session;
                    T       : in out Test_State;
                    Success :    out Boolean;
                    L       : in out Cai.Log.Client_Session;
                    Timer   :        Cai.Timer.Client_Session)
   is
      use type Block.Request_Status;
      Current : Cai.Timer.Time;
      Result  : Block.Result;
   begin
      Success := True;
      for I in Cache'Range loop
         if Client.Status (Cache (I).R) = Block.Pending then
            Client.Update_Request (C, Cache (I).R);
         end if;
         if Client.Status (Cache (I).R) = Block.Ok then
            Client.Release (C, Cache (I).R);
            T.Write_Recv   := T.Write_Recv + 1;
         end if;
         if Client.Status (Cache (I).R) = Block.Error then
            Cai.Log.Client.Error (L, "Write request at block "
                                     & Image (Client.Start (Cache (I).R))
                                     & " returned with error.");
            Client.Release (C, Cache (I).R);
            Success := False;
            return;
         end if;
         if Client.Status (Cache (I).R) = Block.Raw and Cache (I).P then
            Client.Allocate_Request (C,
                                     Cache (I).R,
                                     Block.Write,
                                     Cache (I).S,
                                     1,
                                     I,
                                     Result);
         end if;
         if Client.Status (Cache (I).R) = Block.Allocated then
            Client.Enqueue (C, Cache (I).R);
            if Client.Status (Cache (I).R) = Block.Pending then
               Cache (I).P := False;
            end if;
         end if;
      end loop;
      Client.Submit (C);
      Current := Timer_Client.Clock (Timer);
      Output.Progress ("Writing",
                       Long_Integer (T.Write_Recv),
                       (if
                           Long_Integer'Last / 2 > Long_Integer (T.Count)
                        then
                           Long_Integer (T.Count) * 2
                        else
                           Long_Integer'Last),
                       Long_Integer (Block.Block_Size (C)),
                       Start,
                       Current,
                       Last,
                       L);
      if Write_Finished (T) then
         T.Write     := Block.Request_Id'First;
         T.Read      := Block.Request_Id'First;
         T.Generated := 0;
         for I in Cache'Range loop
            Client.Release (C, Cache (I).R);
            Cache (I).P := False;
            Cache (I).S := 0;
         end loop;
         Update_Read_Cache (T);
      else
         Update_Write_Cache (T, Block.Block_Size (C));
      end if;
   end Write;

   function Write_Finished (T : Test_State) return Boolean
   is
   begin
      return T.Write_Recv = T.Count;
   end Write_Finished;

   procedure Read (C       : in out Block.Client_Session;
                   T       : in out Test_State;
                   Success :    out Boolean;
                   L       : in out Cai.Log.Client_Session;
                   Timer   :        Cai.Timer.Client_Session)
   is
      use type Block.Request_Status;
      Current : Cai.Timer.Time;
      Done    : Long_Integer;
      Todo    : Long_Integer;
      Result  : Block.Result;
   begin
      Success := True;
      for I in Cache'Range loop
         if Client.Status (Cache (I).R) = Block.Pending then
            Client.Update_Request (C, Cache (I).R);
         end if;
         if Client.Status (Cache (I).R) = Block.Error then
            Cai.Log.Client.Error (L, "Read request at block "
                                     & Image (Client.Start (Cache (I).R))
                                     & " returned with error.");
            Client.Release (C, Cache (I).R);
            Success := False;
            return;
         end if;
      end loop;
      loop
         exit when Client.Status (Cache (T.Read).R) /= Block.Ok;
         T.Read_Recv := T.Read_Recv + 1;
         Client.Read (C, Cache (T.Read).R);
         Client.Release (C, Cache (T.Read).R);
         T.Read := Block.Request_Id'Succ (T.Read);
      end loop;
      for I in Cache'Range loop
         if Client.Status (Cache (I).R) = Block.Raw and Cache (I).P then
            Client.Allocate_Request (C,
                                     Cache (I).R,
                                     Block.Read,
                                     Cache (I).S,
                                     1,
                                     I,
                                     Result);
         end if;
         if Client.Status (Cache (I).R) = Block.Allocated then
            Client.Enqueue (C, Cache (I).R);
            if Client.Status (Cache (I).R) = Block.Pending then
               Cache (I).P := False;
            end if;
         end if;
      end loop;
      if not Read_Finished (T) then
         Update_Read_Cache (T);
      end if;
      if Cai.Timer.Initialized (Timer) then
         Current := Timer_Client.Clock (Timer);
      else
         Current := Cai.Timer.Time'First;
      end if;
      if Block.Count'Last - T.Read_Recv > T.Write_Recv then
         Done := Long_Integer (T.Write_Recv + T.Read_Recv);
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
                       Long_Integer (Block.Block_Size (C)),
                       Start,
                       Current,
                       Last,
                       L);
   end Read;

   function Read_Finished (T : Test_State) return Boolean
   is
   begin
      return T.Read_Recv = T.Count;
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
                         I :        Block.Request_Id;
                         D :        Block.Buffer)
   is
      pragma Unreferenced (I);
   begin
      Hash_Block (T.Read_Context, D);
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
                          I :        Block.Request_Id;
                          D :    out Block.Buffer)
   is
      pragma Unreferenced (T);
   begin
      D := Cache (I).B (1 .. D'Length);
   end Block_Write;

end Correctness;
