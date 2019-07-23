
with Componolit.Interfaces.Types;
with Componolit.Interfaces.Log;
with Interfaces;

package Completeness with
   SPARK_Mode
is
   package Cai renames Componolit.Interfaces;

   Sent     : Interfaces.Unsigned_64 := 0;
   Received : Interfaces.Unsigned_64 := 0;
   Ok       : Interfaces.Unsigned_64 := 0;
   Error    : Interfaces.Unsigned_64 := 0;
   Timeout  : Interfaces.Unsigned_64 := 0;

   Raw      : Interfaces.Unsigned_64 := 0;
   Pending  : Interfaces.Unsigned_64 := 0;
   Other    : Interfaces.Unsigned_64 := 0;

   procedure Initialize (C : Cai.Types.Capability;
                         T : Duration;
                         L : String);
   procedure Event (Log : in out Cai.Log.Client_Session);

end Completeness;
