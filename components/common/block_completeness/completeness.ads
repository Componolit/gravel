
with Gneiss.Types;
with Gneiss.Log;
with Interfaces;

package Completeness with
   SPARK_Mode
is

   Sent     : Interfaces.Unsigned_64 := 0;
   Received : Interfaces.Unsigned_64 := 0;
   Ok       : Interfaces.Unsigned_64 := 0;
   Error    : Interfaces.Unsigned_64 := 0;
   Timeout  : Interfaces.Unsigned_64 := 0;

   Raw      : Interfaces.Unsigned_64 := 0;
   Pending  : Interfaces.Unsigned_64 := 0;
   Other    : Interfaces.Unsigned_64 := 0;

   procedure Initialize (C : Gneiss.Types.Capability;
                         T : Duration;
                         L : String);
   procedure Event (Log : in out Gneiss.Log.Client_Session);

end Completeness;
