
with Ada.Unchecked_Conversion;
with LSC.Types;
with LSC.SHA1;

package body Jitter with
   SPARK_Mode
is
   subtype Message is LSC.SHA1.Hash_Type;

   Current_Hash  : LSC.SHA1.Hash_Type;
   Normalization : Duration;

   function Duration_To_Bytes (D : Duration) return Message;
   function Truncate (H : LSC.SHA1.Hash_Type) return Duration;
   function Normalize (D : Duration) return Duration;

   function Duration_To_Bytes (D : Duration) return Message
   is
      subtype Byte_Duration is LSC.Types.Bytes (1 .. 8);
      function To_Bytes is new Ada.Unchecked_Conversion (Duration, Byte_Duration);
      Msg : Message := (others => LSC.Types.Byte'First);
   begin
      Msg (1 .. 8) := To_Bytes (D);
      return Msg;
   end Duration_To_Bytes;

   function Truncate (H : LSC.SHA1.Hash_Type) return Duration
   is
      subtype Byte_Duration is LSC.Types.Bytes (1 .. 8);
      function To_Duration is new Ada.Unchecked_Conversion (Byte_Duration, Duration);
   begin
      return To_Duration (H (1 .. 8));
   end Truncate;

   function Normalize (D : Duration) return Duration
   is
      function Convert is new Ada.Unchecked_Conversion (Duration, Long_Integer);
      function Convert is new Ada.Unchecked_Conversion (Long_Integer, Duration);
   begin
      return Convert (Convert (D) rem Convert (Normalization));
   end Normalize;

   procedure Seed (D : Duration;
                   N : Duration)
   is
   begin
      Current_Hash  := LSC.SHA1.Hash (Duration_To_Bytes (D));
      Normalization := N;
   end Seed;

   procedure Apply (D : in out Duration)
   is
      J : constant Duration := Normalize (Truncate (Current_Hash));
   begin
      Current_Hash := LSC.SHA1.Hash (Duration_To_Bytes (J));
      D := D + J;
   end Apply;

end Jitter;
