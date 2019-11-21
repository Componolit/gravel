
with Gneiss.Log;
with Gneiss.Log.Client;
with Completeness;

package body Component with
   SPARK_Mode
is

   Capability : Gneiss.Types.Capability;
   Log        : Gneiss.Log.Client_Session;

   procedure Construct (C : Gneiss.Types.Capability)
   is
   begin
      Capability := C;
      if not Gneiss.Log.Initialized (Log) then
         Gneiss.Log.Client.Initialize (Log, Capability, "Completeness");
      end if;
      if not Gneiss.Log.Initialized (Log) then
         Main.Vacate (Capability, Main.Failure);
         return;
      end if;
      Event;
   end Construct;

   procedure Destruct
   is
   begin
      if Gneiss.Log.Initialized (Log) then
         Gneiss.Log.Client.Finalize (Log);
      end if;
   end Destruct;

   procedure Event
   is
   begin
      Completeness.Initialize (Capability, 1.0, "dev0");
      Completeness.Event (Log);
   end Event;

end Component;
