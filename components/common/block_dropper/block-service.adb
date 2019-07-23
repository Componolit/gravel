
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

   procedure Request
   is
      Label : String (1 .. 160);
      Last  : Natural;
      Valid : Boolean;
   begin
      Instance.Session_Request (Dispatcher, Valid, Label, Last);
      if Valid and not Block.Server.Instance.Initialized (Block.Server.Server) then
         Instance.Session_Accept (Dispatcher, Block.Server.Server, Label (1 .. Last));
      end if;
      Instance.Session_Cleanup (Dispatcher, Block.Server.Server);
   end Request;

end Block.Service;
