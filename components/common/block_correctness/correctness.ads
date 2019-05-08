
with Cai.Types;
with Cai.Log;
with Cai.Log.Client;
with Cai.Block;
with Cai.Block.Client;
with Cai.Timer;
with Cai.Timer.Client;
with LSC.Internal.SHA256;
with Ringbuffer;

generic
   with package Block is new Cai.Block (<>);
   with package Client is new Block.Client (<>);
package Correctness
is

   use type Block.Count;
   use type Block.Size;
   use type Block.Buffer_Index;

   type Buffer_Index is mod 256;
   subtype Block_Buffer is Block.Buffer (1 .. 4096);

   type Write_Cache is record
      Buffer  : Block_Buffer;
      Context : LSC.Internal.SHA256.Context_Type;
   end record;

   package Read_Ring is new Ringbuffer (Block, Buffer_Index, Block_Buffer);
   package Write_Ring is new Ringbuffer (Block, Buffer_Index, Write_Cache);

   type Test_State is record
      Last           : Block.Id;
      Sent           : Block.Count;
      Written        : Block.Count;
      Read           : Block.Count;
      Count          : Block.Count;
      Bounds_Checked : Boolean;
      Compared       : Boolean;
      Write_Context  : LSC.Internal.SHA256.Context_Type;
      Read_Context   : LSC.Internal.SHA256.Context_Type;
      Read_Data      : Read_Ring.Cycle;
      Write_Data     : Write_Ring.Cycle;
   end record;

   function State_Initialized return Boolean with Ghost;

   procedure Initialize (C   :        Block.Client_Session;
                         T   :    out Test_State;
                         Max :        Block.Count) with
     Pre => Client.Initialized (C)
     and then Client.Block_Size (C) <= Block.Size (Block_Buffer'Last)
     and then Client.Block_Size (C) > 0
     and then Client.Block_Size (C) mod (LSC.Internal.SHA256.Block_Size / 8) = 0
     and then Max > 1,
     Post => Client.Initialized (C) and State_Initialized;

   procedure Bounds_Check (C       : in out Block.Client_Session;
                           T       : in out Test_State;
                           Success :    out Boolean;
                           L       : in out Cai.Log.Client_Session;
                           Timer   :        Cai.Timer.Client_Session) with
     Pre => Client.Initialized (C)
     and Cai.Log.Client.Initialized (L)
     and Cai.Timer.Client.Initialized (Timer),
     Post => Client.Initialized (C) and Cai.Log.Client.Initialized (L);

   function Bounds_Check_Finished (T : Test_State) return Boolean;

   procedure Write (C       : in out Block.Client_Session;
                    T       : in out Test_State;
                    Success :    out Boolean;
                    L       : in out Cai.Log.Client_Session;
                    Timer   :        Cai.Timer.Client_Session) with
     Pre => Client.Initialized (C)
     and then Client.Block_Size (C) <= Block.Size (Block_Buffer'Last)
     and then Cai.Log.Client.Initialized (L)
     and then Cai.Timer.Client.Initialized (Timer)
     and then State_Initialized,
     Post => Client.Initialized (C) and Cai.Log.Client.Initialized (L);

   function Write_Finished (T : Test_State) return Boolean;

   procedure Read (C       : in out Block.Client_Session;
                   T       : in out Test_State;
                   Success :    out Boolean;
                   L       : in out Cai.Log.Client_Session;
                   Timer   :        Cai.Timer.Client_Session) with
     Pre => Client.Initialized (C)
     and then Cai.Log.Client.Initialized (L)
     and then Cai.Timer.Client.Initialized (Timer)
     and then State_Initialized
     and then Client.Block_Size (C) <= Block_Buffer'Length
     and then Client.Block_Size (C) mod (LSC.Internal.SHA256.Block_Size / 8) = 0,
     Post => Client.Initialized (C) and Cai.Log.Client.Initialized (L);

   function Read_Finished (T : Test_State) return Boolean;

   procedure Compare (T     : in out Test_State;
                      Equal :    out Boolean);

   function Compare_Finished (T : Test_State) return Boolean;

   procedure Block_Read (T : in out Test_State;
                         S :        Block.Id;
                         D :        Block.Buffer) with
     Pre => Read_Ring.Has_Block (T.Read_Data, S)
     and D'Length <= Block_Buffer'Length;

   procedure Block_Write (T : in out Test_State;
                          S :        Block.Id;
                          D :    out Block.Buffer) with
     Pre => D'Length <= Block_Buffer'Length;

end Correctness;
