
with Componolit.Interfaces.Types;
with Componolit.Interfaces.Block.Dispatcher;
with Interfaces;
with Block.Server;

package Block.Service with
   SPARK_Mode
is
   procedure Start (Cap     :     Componolit.Interfaces.Types.Capability;
                    Success : out Boolean;
                    Device  :     String;
                    Modulo  :     Interfaces.Unsigned_8;
                    Part    :     Interfaces.Unsigned_8;
                    Count   :     Interfaces.Unsigned_64);

   procedure Request;

   package Instance is new Types.Dispatcher (Block.Server.Instance, Request);

private

   Dispatcher : Types.Dispatcher_Session := Instance.Create;

end Block.Service;
