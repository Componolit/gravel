
with Block;
with Block.Service;
with Config;
with Componolit.Gneiss.Log;
with Componolit.Gneiss.Log.Client;
with Componolit.Gneiss.Strings;

package body Component with
   SPARK_Mode
is

   Log : Cai.Log.Client_Session;

   procedure Construct (Cap : Cai.Types.Capability)
   is
      use type Config.Distribution;
      use type Config.Mode;
      Success : Boolean;
   begin
      if not Cai.Log.Initialized (Log) then
         Cai.Log.Client.Initialize (Log, Cap, "");
      end if;
      if Cai.Log.Initialized (Log) then
         Cai.Log.Client.Info (Log, "Delay server");
         Config.Initialize (Cap, Success);
         if Success and then Config.Initialized then
            Cai.Log.Client.Info (Log, "Mode: " & (if Config.Get_Mode = Config.Continuous
                                                  then "continuous"
                                                  else "sliced"));
            Cai.Log.Client.Info (Log, "Client: " & Config.Get_Client_Id);
            case Config.Get_Mode is
               when Config.Continuous =>
                  Cai.Log.Client.Info (Log, "Delay: " & Cai.Strings.Image (Config.Get_Delay));
                  Cai.Log.Client.Info
                     (Log, "Jitter: " & Cai.Strings.Image (Config.Get_Jitter) & " ("
                           & (if Config.Get_Jitter_Distribution = Config.Uniform then "uniform" else "none")
                           & ")");
               when Config.Sliced =>
                  Cai.Log.Client.Info (Log, "Slice: " & Cai.Strings.Image (Config.Get_Slice));
            end case;
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
      if Cai.Log.Initialized (Log) then
         Cai.Log.Client.Finalize (Log);
      end if;
   end Destruct;

end Component;
