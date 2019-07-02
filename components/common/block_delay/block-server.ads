
with Componolit.Interfaces.Types;
with Componolit.Interfaces.Block.Server;

package Block.Server with
   SPARK_Mode
is

   procedure Set_Capability (Cap : Componolit.Interfaces.Types.Capability);

   procedure Event;
   function Block_Count (S : Types.Server_Instance) return Types.Count;
   function Block_Size (S : Types.Server_Instance) return Types.Size;
   function Writable (S : Types.Server_Instance) return Boolean;
   function Maximum_Transfer_Size (S : Types.Server_Instance) return Types.Byte_Length;
   procedure Initialize (S : Types.Server_Instance;
                         L : String;
                         B : Types.Byte_Length);
   procedure Finalize (S : Types.Server_Instance);

   package Instance is new Types.Server (Event,
                                         Block_Count,
                                         Block_Size,
                                         Writable,
                                         Maximum_Transfer_Size,
                                         Initialize,
                                         Finalize);

end Block.Server;
