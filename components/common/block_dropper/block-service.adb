
package body Block.Service with
   SPARK_Mode
is

   procedure Start (Cap     :     Componolit.Gneiss.Types.Capability;
                    Success : out Boolean;
                    Device  :     String;
                    Modulo  :     Interfaces.Unsigned_8;
                    Part    :     Interfaces.Unsigned_8;
                    Count   :     Interfaces.Unsigned_64;
                    Drop    :     Boolean)
   is
   begin
      if not Types.Initialized (Dispatcher) then
         Instance.Initialize (Dispatcher, Cap, True);
      end if;
      Success := Types.Initialized (Dispatcher);
      if Success then
         Block.Server.Eager_Initialize (Cap, Device, Modulo, Part, Count, Drop, Success);
         Instance.Register (Dispatcher);
      end if;
   end Start;

   procedure Request (D : in out Types.Dispatcher_Session;
                      C :        Types.Dispatcher_Capability)
   is
   begin
      if
         Instance.Valid_Session_Request (D, C)
         and not Types.Initialized (Block.Server.Server)
      then
         Instance.Session_Initialize (D, C, Block.Server.Server, True);
         if Types.Initialized (Block.Server.Server) then
            Instance.Session_Accept (D, C, Block.Server.Server);
         end if;
      end if;
      Instance.Session_Cleanup (D, C, Block.Server.Server);
   end Request;

end Block.Service;
