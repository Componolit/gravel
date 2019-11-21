
with Gneiss.Types;
with Gneiss.Block.Dispatcher;
with Interfaces;
with Block.Server;

package Block.Service with
   SPARK_Mode
is
   procedure Start (Cap     :     Gneiss.Types.Capability;
                    Success : out Boolean;
                    Device  :     String;
                    Modulo  :     Interfaces.Unsigned_8;
                    Part    :     Interfaces.Unsigned_8;
                    Count   :     Interfaces.Unsigned_64;
                    Op      :     Block.Server.Operation);

   procedure Request (D : in out Types.Dispatcher_Session;
                      C :        Block.Types.Dispatcher_Capability);

   package Instance is new Types.Dispatcher (Block.Server.Instance, Request);

private

   Dispatcher : Types.Dispatcher_Session;

end Block.Service;
