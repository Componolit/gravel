
package Jitter with
   SPARK_Mode
is

   procedure Seed (D : Duration;
                   N : Duration);

   procedure Apply (D : in out Duration);

end Jitter;
