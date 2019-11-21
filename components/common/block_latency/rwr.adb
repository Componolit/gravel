
with Gneiss.Log.Client;

package body Rwr is

   procedure Initialize (R : out Rwr_Run;
                         C :     Gneiss.Types.Capability)
   is
   begin
      RR1.Initialize (R.R1, True, C);
      WR.Initialize (R.W, True, C);
      RR2.Initialize (R.R2, True, C);
   end Initialize;

   Printed : Boolean := False;

   procedure Run (C   : in out Block.Client_Session;
                  R   : in out Rwr_Run;
                  Log : in out Gneiss.Log.Client_Session)
   is
   begin
      if not RR1.Finished (R.R1) then
         if not Printed then
            Gneiss.Log.Client.Info (Log, "Run: read 1");
            Printed := True;
         end if;
         RR1.Run (C, R.R1, Log);
         if RR1.Finished (R.R1) then
            Gneiss.Log.Client.Flush (Log);
            Printed := False;
         end if;
      end if;
      if RR1.Finished (R.R1) and not WR.Finished (R.W) then
         if not Printed then
            Gneiss.Log.Client.Info (Log, "Run: write");
            Printed := True;
         end if;
         WR.Run (C, R.W, Log);
         if WR.Finished (R.W) then
            Gneiss.Log.Client.Flush (Log);
            Printed := False;
         end if;
      end if;
      if WR.Finished (R.W) and not RR2.Finished (R.R2) then
         if not Printed then
            Gneiss.Log.Client.Info (Log, "Run: read 2");
            Printed := True;
         end if;
         RR2.Run (C, R.R2, Log);
         if RR2.Finished (R.R2) then
            Gneiss.Log.Client.Flush (Log);
         end if;
      end if;
   end Run;

   function Finished (R : Rwr_Run) return Boolean
   is
   begin
      return RR1.Finished (R.R1) and WR.Finished (R.W) and RR2.Finished (R.R2);
   end Finished;

   procedure Xml (Xml_Log : in out Gneiss.Log.Client_Session;
                  R       :        Rwr_Run;
                  Log     : in out Gneiss.Log.Client_Session)
   is
   begin
      Gneiss.Log.Client.Info (Log, "r1: ", False);
      RR1.Xml (Xml_Log, R.R1, True, Log);
      Gneiss.Log.Client.Flush (Log);
      Gneiss.Log.Client.Info (Log, "w: ", False);
      WR.Xml (Xml_Log, R.W, False, Log);
      Gneiss.Log.Client.Flush (Log);
      Gneiss.Log.Client.Info (Log, "r2: ", False);
      RR2.Xml (Xml_Log, R.R2, False, Log);
      Gneiss.Log.Client.Flush (Log);
   end Xml;

end Rwr;
