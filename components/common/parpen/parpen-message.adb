package body Parpen.Message is

   ---------------
   -- Translate --
   ---------------

   procedure Translate (Data    : in out Types.Bytes_Ptr;
                        Offsets :        Types.Bytes_Ptr;
                        Result  :    out Result_Type)
   is
      pragma Unreferenced (Data, Offsets);
   begin
      Result := Result_Invalid;
   end Translate;

end Parpen.Message;
