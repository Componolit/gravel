
with Componolit.Interfaces.Log;
with Componolit.Interfaces.Log.Client;
with Block;
with Block.Service;

package body Component with
   SPARK_Mode
is

   Log : Cai.Log.Client_Session := Cai.Log.Client.Create;

   procedure Construct (C : Cai.Types.Capability)
   is
      Success : Boolean;
   begin
      if not Cai.Log.Client.Initialized (Log) then
         Cai.Log.Client.Initialize (Log, C, "Drop");
      end if;
      if not Cai.Log.Client.Initialized (Log) then
         Main.Vacate (C, Main.Failure);
         return;
      end if;
      Cai.Log.Client.Info (Log, "Starting drop server...");
      Block.Service.Start (C, Success);
      if Success then
         Cai.Log.Client.Info (Log, "Server started.");
      else
         Cai.Log.Client.Error (Log, "Failed to start server");
         Main.Vacate (C, Main.Failure);
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
