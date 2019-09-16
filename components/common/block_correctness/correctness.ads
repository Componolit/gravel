
with Componolit.Gneiss.Log;
with Componolit.Gneiss.Block;
with Componolit.Gneiss.Block.Client;
with Componolit.Gneiss.Timer;
with Componolit.Gneiss.Timer.Client;
with LSC.Internal.SHA256;

generic
   with package Block is new Componolit.Gneiss.Block (<>);
   with package Client is new Block.Client (<>);
   with package Timer_Client is new Componolit.Gneiss.Timer.Client (<>);
package Correctness
is
   pragma Unevaluated_Use_Of_Old (Allow);
   package Cai renames Componolit.Gneiss;

   use type Block.Count;
   use type Block.Size;
   use type Block.Buffer_Index;

   subtype Block_Buffer is Block.Buffer (1 .. 4096);
   Null_Buffer : constant Block_Buffer := (others => Block.Byte'First);

   type Request_Cache_Entry is limited record
      R : Client.Request;
      --  Request
      P : Boolean      := False;
      --  Preallocated
      S : Block.Id     := Block.Id'First;
      --  Preallocated request id
      B : Block_Buffer := (others => Block.Byte'First);
      --  Preallocated block (Write) / Received, unhandled block (Read)
   end record;

   type Request_Cache is array (Block.Request_Id'Range) of Request_Cache_Entry;

   Cache : Request_Cache;

   type Test_State is record
      Write          : Block.Request_Id;
      Read           : Block.Request_Id;
      Generated      : Block.Count;
      Write_Recv     : Block.Count;
      Read_Recv      : Block.Count;
      Count          : Block.Count;
      Bounds_Checked : Boolean;
      Compared       : Boolean;
      Write_Context  : LSC.Internal.SHA256.Context_Type;
      Read_Context   : LSC.Internal.SHA256.Context_Type;
   end record;

   function State_Initialized return Boolean with
      Ghost;

   procedure Initialize (C   :        Block.Client_Session;
                         T   :    out Test_State;
                         Max :        Block.Count) with
      Pre  => Block.Initialized (C)
              and then Block.Block_Size (C) <= Block.Size (Block_Buffer'Last)
              and then Block.Block_Size (C) > 0
              and then Block.Block_Size (C) mod (LSC.Internal.SHA256.Block_Size / 8) = 0
              and then Max > 1,
      Post => Block.Initialized (C) and State_Initialized;

   procedure Bounds_Check (C       : in out Block.Client_Session;
                           T       : in out Test_State;
                           Success :    out Boolean;
                           L       : in out Cai.Log.Client_Session;
                           Timer   :        Cai.Timer.Client_Session) with
      Pre  => Block.Initialized (C)
              and Cai.Log.Initialized (L)
              and Cai.Timer.Initialized (Timer),
      Post => Block.Initialized (C) and Cai.Log.Initialized (L);

   function Bounds_Check_Finished (T : Test_State) return Boolean;

   procedure Write (C       : in out Block.Client_Session;
                    T       : in out Test_State;
                    Success :    out Boolean;
                    L       : in out Cai.Log.Client_Session;
                    Timer   :        Cai.Timer.Client_Session) with
      Pre  => Block.Initialized (C)
              and then Block.Block_Size (C) > 0
              and then Block.Block_Size (C) <= Block_Buffer'Length
              and then Block.Block_Size (C) mod (LSC.Internal.SHA256.Block_Size / 8) = 0
              and then Cai.Log.Initialized (L)
              and then Cai.Timer.Initialized (Timer)
              and then State_Initialized,
      Post => Block.Initialized (C)
              and then Cai.Log.Initialized (L)
              and then State_Initialized
              and then Block.Block_Size (C)'Old = Block.Block_Size (C);

   function Write_Finished (T : Test_State) return Boolean;

   procedure Read (C       : in out Block.Client_Session;
                   T       : in out Test_State;
                   Success :    out Boolean;
                   L       : in out Cai.Log.Client_Session;
                   Timer   :        Cai.Timer.Client_Session) with
      Pre  => Block.Initialized (C)
              and then Cai.Log.Initialized (L)
              and then Cai.Timer.Initialized (Timer)
              and then State_Initialized
              and then Block.Block_Size (C) <= Block_Buffer'Length
              and then Block.Block_Size (C) mod (LSC.Internal.SHA256.Block_Size / 8) = 0,
      Post => Block.Initialized (C) and Cai.Log.Initialized (L) and State_Initialized;

   function Read_Finished (T : Test_State) return Boolean;

   procedure Compare (T     : in out Test_State;
                      Equal :    out Boolean);

   function Compare_Finished (T : Test_State) return Boolean;

   procedure Block_Read (T : in out Test_State;
                         I :        Block.Request_Id;
                         D :        Block.Buffer) with
      Pre => D'Length <= Block_Buffer'Length;

   procedure Block_Write (T : in out Test_State;
                          I :        Block.Request_Id;
                          D :    out Block.Buffer) with
      Pre => D'Length <= Block_Buffer'Length;

end Correctness;
