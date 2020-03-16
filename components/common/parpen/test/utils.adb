with Ada.Text_IO;

package body Utils is

   -----------
   -- Digit --
   -----------

   function Digit (Value : Natural) return Character;
   function Digit (Value : Natural) return Character
   is
      V : constant Natural := Value mod 16;
   begin
      if V <= 9 then
         return Character'Val (V + Character'Pos ('0'));
      elsif V <= 16 then
         return Character'Val (V + Character'Pos ('A') - 16#A#);
      end if;
      return '*';
   end Digit;

   function Hex (Value : Character) return String;
   function Hex (Value : Character) return String
   is
      Val : constant Natural := Natural (Character'Pos (Value));
   begin
      return Digit (Val / 16) & Digit (Val mod 16);
   end Hex;

   ---------------
   -- Printable --
   ---------------

   function Printable (Value : Character) return Character;
   function Printable (Value : Character) return Character
   is
      P : constant Natural := Character'Pos (Value);
   begin
      if P > 31 and P < 127 then
         return Value;
      else
         return '.';
      end if;
   end Printable;

   -------------
   -- Hexdump --
   -------------

   procedure Hexdump (Label : String;
                      Data  : String)
   is
      use Ada.Text_IO;
      First : Boolean := True;
   begin
      Put_Line (Label & " (len:" & Data'Length'Img & ")");
      Put ("   ");
      for D of Data loop
         if not First then
            Put (" ");
         end if;
         First := False;
         Put (Hex (D));
      end loop;
      New_Line;
      Put ("   ");
      for D of Data loop
         Put (" " & Printable (D) & " ");
      end loop;
      New_Line;
   end Hexdump;

end Utils;
