
with Block;
with Block.Service;
with Componolit.Interfaces.Log;
with Componolit.Interfaces.Log.Client;

package body Component with
   SPARK_Mode
is

   Log : Cai.Log.Client_Session := Cai.Log.Client.Create;

   procedure Construct (Cap : Cai.Types.Capability)
   is
      Success : Boolean;
   begin
      if not Cai.Log.Client.Initialized (Log) then
         Cai.Log.Client.Initialize (Log, Cap, "");
      end if;
      if Cai.Log.Client.Initialized (Log) then
         Cai.Log.Client.Info (Log, "Delay server");
         Block.Service.Start (Cap, Success);
         if Success then
            Cai.Log.Client.Info (Log, "Delay server started...");
         else
            Cai.Log.Client.Error (Log, "Delay server start failed.");
            Main.Vacate (Cap, Main.Failure);
         end if;
      else
         Main.Vacate (Cap, Main.Failure);
      end if;
   end Construct;

   procedure Destruct
   is
   begin
      if Cai.Log.Client.Initialized (Log) then
         Cai.Log.Client.Finalize (Log);
      end if;
   end Destruct;

end Component;
