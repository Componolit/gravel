
with Gneiss.Types;
with Gneiss.Block.Dispatcher;
with Block.Server;

package Block.Service with
   SPARK_Mode
is
   procedure Start (Cap     :     Gneiss.Types.Capability;
                    Success : out Boolean);

   procedure Request (D : in out Block.Types.Dispatcher_Session;
                      C :        Block.Types.Dispatcher_Capability);

   package Instance is new Types.Dispatcher (Block.Server.Instance, Request);

private

   Dispatcher : Types.Dispatcher_Session;

end Block.Service;
