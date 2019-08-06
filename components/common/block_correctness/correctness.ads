
with Componolit.Interfaces.Log;
with Componolit.Interfaces.Log.Client;
with Componolit.Interfaces.Block;
with Componolit.Interfaces.Block.Client;
with Componolit.Interfaces.Timer;
with Componolit.Interfaces.Timer.Client;
with LSC.Internal.SHA256;

generic
   with package Block is new Componolit.Interfaces.Block (<>);
   with package Client is new Block.Client (<>);
   with package Timer_Client is new Componolit.Interfaces.Timer.Client (<>);
package Correctness
is
   pragma Unevaluated_Use_Of_Old (Allow);
   package Cai renames Componolit.Interfaces;

   use type Block.Count;
   use type Block.Size;
   use type Block.Buffer_Index;

   subtype Block_Buffer is Block.Buffer (1 .. 4096);
   Null_Buffer : constant Block_Buffer := (others => Block.Byte'First);

   type Request_Cache_Entry is limited record
      R : Client.Request; --  Request
      P : Boolean;        --  Preallocated
      S : Block.Id;       --  Preallocated request id
      B : Block_Buffer;   --  Preallocated block (Write) / Received, unhandled block (Read)
   end record;

   type Request_Cache is array (Client.Request_Id'Range) of Request_Cache_Entry;

   Cache : Request_Cache := (others => (R => Client.Null_Request,
                                        P => False,
                                        S => 0,
                                        B => (others => Block.Byte'First)));

   type Test_State is record
      Write          : Client.Request_Id;
      Read           : Client.Request_Id;
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
      Pre  => Client.Initialized (C)
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
      Pre  => Client.Initialized (C)
              and Cai.Log.Client.Initialized (L)
              and Timer_Client.Initialized (Timer),
      Post => Client.Initialized (C) and Cai.Log.Client.Initialized (L);

   function Bounds_Check_Finished (T : Test_State) return Boolean;

   procedure Write (C       : in out Block.Client_Session;
                    T       : in out Test_State;
                    Success :    out Boolean;
                    L       : in out Cai.Log.Client_Session;
                    Timer   :        Cai.Timer.Client_Session) with
      Pre  => Client.Initialized (C)
              and then Client.Block_Size (C) > 0
              and then Client.Block_Size (C) <= Block_Buffer'Length
              and then Client.Block_Size (C) mod (LSC.Internal.SHA256.Block_Size / 8) = 0
              and then Cai.Log.Client.Initialized (L)
              and then Timer_Client.Initialized (Timer)
              and then State_Initialized,
      Post => Client.Initialized (C)
              and then Cai.Log.Client.Initialized (L)
              and then State_Initialized
              and then Client.Block_Size (C)'Old = Client.Block_Size (C);

   function Write_Finished (T : Test_State) return Boolean;

   procedure Read (C       : in out Block.Client_Session;
                   T       : in out Test_State;
                   Success :    out Boolean;
                   L       : in out Cai.Log.Client_Session;
                   Timer   :        Cai.Timer.Client_Session) with
      Pre  => Client.Initialized (C)
              and then Cai.Log.Client.Initialized (L)
              and then Timer_Client.Initialized (Timer)
              and then State_Initialized
              and then Client.Block_Size (C) <= Block_Buffer'Length
              and then Client.Block_Size (C) mod (LSC.Internal.SHA256.Block_Size / 8) = 0,
      Post => Client.Initialized (C) and Cai.Log.Client.Initialized (L) and State_Initialized;

   function Read_Finished (T : Test_State) return Boolean;

   procedure Compare (T     : in out Test_State;
                      Equal :    out Boolean);

   function Compare_Finished (T : Test_State) return Boolean;

   procedure Block_Read (T : in out Test_State;
                         I :        Client.Request_Id;
                         D :        Block.Buffer) with
      Pre => D'Length <= Block_Buffer'Length;

   procedure Block_Write (T : in out Test_State;
                          I :        Client.Request_Id;
                          D :    out Block.Buffer) with
      Pre => D'Length <= Block_Buffer'Length;

end Correctness;
