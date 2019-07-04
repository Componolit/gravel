
with Block;
with Block.Service;
with Config;
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
         Config.Initialize (Cap, Success);
         if Success and then Config.Initialized then
            Cai.Log.Client.Info (Log, "Delay: " & Cai.Log.Image (Config.Get_Delay));
            Cai.Log.Client.Info (LOg, "Client: " & Config.Get_Client_Id);
            Block.Service.Start (Cap, Success);
            if Success then
               Cai.Log.Client.Info (Log, "Delay server started...");
            else
               Cai.Log.Client.Error (Log, "Delay server start failed.");
               Main.Vacate (Cap, Main.Failure);
            end if;
         else
            Cai.Log.Client.Error (Log, "Failed to parse config.");
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
