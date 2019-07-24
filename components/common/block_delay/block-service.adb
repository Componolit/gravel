
package body Block.Service with
   SPARK_Mode
is

   procedure Start (Cap     :     Componolit.Interfaces.Types.Capability;
                    Success : out Boolean)
   is
   begin
      if not Instance.Initialized (Dispatcher) then
         Instance.Initialize (Dispatcher, Cap);
      end if;
      Success := Instance.Initialized (Dispatcher);
      if Success then
         Block.Server.Set_Capability (Cap);
         Instance.Register (Dispatcher);
      end if;
   end Start;

   procedure Request (C : Block.Types.Dispatcher_Capability)
   is
   begin
      if
         Instance.Valid_Session_Request (Dispatcher, C)
         and not Block.Server.Instance.Initialized (Block.Server.Server)
      then
         Instance.Session_Initialize (Dispatcher, C, Block.Server.Server);
         if Block.Server.Instance.Initialized (Block.Server.Server) then
            Instance.Session_Accept (Dispatcher, C, Block.Server.Server);
         end if;
      end if;
      Instance.Session_Cleanup (Dispatcher, C, Block.Server.Server);
   end Request;

end Block.Service;
