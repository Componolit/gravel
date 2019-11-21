
with Block;
with Block.Service;
with Config;
with Gneiss.Log;
with Gneiss.Log.Client;
with Basalt.Strings;

package body Component with
   SPARK_Mode
is

   Log : Gneiss.Log.Client_Session;

   procedure Construct (Cap : Gneiss.Types.Capability)
   is
      use type Config.Distribution;
      use type Config.Mode;
      Success : Boolean;
   begin
      if not Gneiss.Log.Initialized (Log) then
         Gneiss.Log.Client.Initialize (Log, Cap, "");
      end if;
      if Gneiss.Log.Initialized (Log) then
         Gneiss.Log.Client.Info (Log, "Delay server");
         Config.Initialize (Cap, Success);
         if Success and then Config.Initialized then
            Gneiss.Log.Client.Info (Log, "Mode: " & (if Config.Get_Mode = Config.Continuous
                                                  then "continuous"
                                                  else "sliced"));
            Gneiss.Log.Client.Info (Log, "Client: " & Config.Get_Client_Id);
            case Config.Get_Mode is
               when Config.Continuous =>
                  Gneiss.Log.Client.Info (Log, "Delay: " & Basalt.Strings.Image (Config.Get_Delay));
                  Gneiss.Log.Client.Info
                     (Log, "Jitter: " & Basalt.Strings.Image (Config.Get_Jitter) & " ("
                           & (if Config.Get_Jitter_Distribution = Config.Uniform then "uniform" else "none")
                           & ")");
               when Config.Sliced =>
                  Gneiss.Log.Client.Info (Log, "Slice: " & Basalt.Strings.Image (Config.Get_Slice));
            end case;
            Block.Service.Start (Cap, Success);
            if Success then
               Gneiss.Log.Client.Info (Log, "Delay server started...");
            else
               Gneiss.Log.Client.Error (Log, "Delay server start failed.");
               Main.Vacate (Cap, Main.Failure);
            end if;
         else
            Gneiss.Log.Client.Error (Log, "Failed to parse config.");
            Main.Vacate (Cap, Main.Failure);
         end if;
      else
         Main.Vacate (Cap, Main.Failure);
      end if;
   end Construct;

   procedure Destruct
   is
   begin
      if Gneiss.Log.Initialized (Log) then
         Gneiss.Log.Client.Finalize (Log);
      end if;
   end Destruct;

end Component;
