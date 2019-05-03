
with Cai.Types;
with Cai.Log;
with Cai.Block;
with Cai.Block.Client;
with LSC.Internal.SHA256;
with Ringbuffer;

generic
   with package Block is new Cai.Block (<>);
   with package Client is new Block.Client (<>);
   with function Next (Current : Block.Id) return Block.Id;
package Correctness is

   use type Block.Buffer_Index;

   type Buffer_Index is mod 256;
   subtype Block_Buffer is Block.Buffer (1 .. 4096);

   package Ring is new Ringbuffer (Block, Buffer_Index, Block_Buffer);

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
      Data           : Ring.Cycle;
   end record;

   function Byte_Image (Bytes : Long_Integer) return String;

   procedure Initialize (C   : in out Block.Client_Session;
                         T   :    out Test_State;
                         L   : in out Cai.Log.Client_Session;
                         Cap :        Cai.Types.Capability;
                         Max :        Block.Count);

   procedure Bounds_Check (C       : in out Block.Client_Session;
                           T       : in out Test_State;
                           Success :    out Boolean;
                           L       : in out Cai.Log.Client_Session);

   function Bounds_Check_Finished (T : Test_State) return Boolean;

   procedure Write (C       : in out Block.Client_Session;
                    T       : in out Test_State;
                    Success :    out Boolean;
                    L       : in out Cai.Log.Client_Session);

   function Write_Finished (T : Test_State) return Boolean;

   procedure Read (C       : in out Block.Client_Session;
                   T       : in out Test_State;
                   Success :    out Boolean;
                   L       : in out Cai.Log.Client_Session);

   function Read_Finished (T : Test_State) return Boolean;

   procedure Compare (T     : in out Test_State;
                      Equal :    out Boolean);

   function Compare_Finished (T : Test_State) return Boolean;

   procedure Hash_Block (Context : in out LSC.Internal.SHA256.Context_Type;
                         Buffer  :        Block.Buffer) with
      Pre => Buffer'Length mod (LSC.Internal.SHA256.Block_Size / 8) = 0;

   procedure Cache_Data (T : in out Test_State;
                         S : Block.Id;
                         B : Block.Buffer) with
     Pre => B'Length > 0 and B'Length <= 4096;

end Correctness;
