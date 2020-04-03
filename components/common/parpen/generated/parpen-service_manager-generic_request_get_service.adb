package body Parpen.Service_Manager.Generic_Request_Get_Service with
  SPARK_Mode
is

   function Create return Context is
     ((Types.Index'First, Types.Index'First, Types.Bit_Index'First, Types.Bit_Index'First, null, (F_Len => (State => S_Invalid, Predecessor => F_Initial), others => (State => S_Invalid, Predecessor => F_Final))));

   procedure Initialize (Ctx : out Context; Buffer : in out Types.Bytes_Ptr) is
   begin
      Initialize (Ctx, Buffer, Types.First_Bit_Index (Buffer'First), Types.Last_Bit_Index (Buffer'Last));
   end Initialize;

   procedure Initialize (Ctx : out Context; Buffer : in out Types.Bytes_Ptr; First, Last : Types.Bit_Index) is
      Buffer_First : constant Types.Index := Buffer'First;
      Buffer_Last : constant Types.Index := Buffer'Last;
   begin
      Ctx := (Buffer_First, Buffer_Last, First, Last, Buffer, (F_Len => (State => S_Invalid, Predecessor => F_Initial), others => (State => S_Invalid, Predecessor => F_Final)));
      Buffer := null;
   end Initialize;

   function Initialized (Ctx : Context) return Boolean is
     (Valid_Next (Ctx, F_Len)
      and then Available_Space (Ctx, F_Len) = (Types.Last_Bit_Index (Ctx.Buffer_Last) - Ctx.First + 1)
      and then Invalid (Ctx, F_Len)
      and then Invalid (Ctx, F_Name));

   procedure Take_Buffer (Ctx : in out Context; Buffer : out Types.Bytes_Ptr) is
   begin
      Buffer := Ctx.Buffer;
      Ctx.Buffer := null;
   end Take_Buffer;

   function Has_Buffer (Ctx : Context) return Boolean is
     (Ctx.Buffer /= null);

   function Message_Last (Ctx : Context) return Types.Bit_Index is
     ((if Structural_Valid (Ctx.Cursors (F_Name)) then
       Ctx.Cursors (F_Name).Last
    else
       Types.Unreachable_Bit_Length));

   function Path_Condition (Ctx : Context; Fld : Field) return Boolean is
     ((case Ctx.Cursors (Fld).Predecessor is
         when F_Initial =>
            (case Fld is
                  when F_Len =>
                     True,
                  when others =>
                     False),
         when F_Len =>
            (case Fld is
                  when F_Name =>
                     True,
                  when others =>
                     False),
         when F_Name | F_Final =>
            False));

   function Field_Condition (Ctx : Context; Val : Field_Dependent_Value) return Boolean is
     ((case Val.Fld is
         when F_Initial | F_Len | F_Name =>
            True,
         when F_Final =>
            False));

   function Field_Length (Ctx : Context; Fld : Field) return Types.Bit_Length is
     ((case Ctx.Cursors (Fld).Predecessor is
         when F_Initial =>
            (case Fld is
                  when F_Len =>
                     Parpen.Service_Manager.Len'Size,
                  when others =>
                     Types.Unreachable_Bit_Length),
         when F_Len =>
            (case Fld is
                  when F_Name =>
                     Types.Bit_Length (Ctx.Cursors (F_Len).Value.Len_Value) * 8,
                  when others =>
                     Types.Unreachable_Bit_Length),
         when F_Name | F_Final =>
            0));

   function Field_First (Ctx : Context; Fld : Field) return Types.Bit_Index is
     ((case Fld is
         when F_Len =>
            Ctx.First,
         when F_Name =>
            (if Ctx.Cursors (Fld).Predecessor = F_Len then
                (Ctx.Cursors (Ctx.Cursors (Fld).Predecessor).Last + 1)
             else
                Types.Unreachable_Bit_Length)));

   function Field_Last (Ctx : Context; Fld : Field) return Types.Bit_Index is
     ((Field_First (Ctx, Fld) + Field_Length (Ctx, Fld) - 1));

   function Predecessor (Ctx : Context; Fld : Virtual_Field) return Virtual_Field is
     ((case Fld is
         when F_Initial =>
            F_Initial,
         when others =>
            Ctx.Cursors (Fld).Predecessor));

   function Successor (Ctx : Context; Fld : Field) return Virtual_Field is
     ((case Fld is
         when F_Len =>
            F_Name,
         when F_Name =>
            F_Final))
    with
     Pre =>
       Structural_Valid (Ctx, Fld)
          and Valid_Predecessor (Ctx, Fld);

   function Valid_Predecessor (Ctx : Context; Fld : Virtual_Field) return Boolean is
     ((case Fld is
         when F_Initial =>
            True,
         when F_Len =>
            Ctx.Cursors (Fld).Predecessor = F_Initial,
         when F_Name =>
            (Valid (Ctx.Cursors (F_Len))
                 and Ctx.Cursors (Fld).Predecessor = F_Len),
         when F_Final =>
            (Structural_Valid (Ctx.Cursors (F_Name))
                 and Ctx.Cursors (Fld).Predecessor = F_Name)));

   function Invalid_Successor (Ctx : Context; Fld : Field) return Boolean is
     ((case Fld is
         when F_Len =>
            Invalid (Ctx.Cursors (F_Name)),
         when F_Name =>
            True));

   function Valid_Next (Ctx : Context; Fld : Field) return Boolean is
     (Valid_Predecessor (Ctx, Fld)
      and then Path_Condition (Ctx, Fld));

   function Available_Space (Ctx : Context; Fld : Field) return Types.Bit_Length is
     ((Types.Last_Bit_Index (Ctx.Buffer_Last) - Field_First (Ctx, Fld) + 1));

   procedure Reset_Dependent_Fields (Ctx : in out Context; Fld : Field) with
     Pre =>
       Valid_Next (Ctx, Fld),
     Post =>
       Valid_Next (Ctx, Fld)
          and Invalid (Ctx.Cursors (Fld))
          and Invalid_Successor (Ctx, Fld)
          and Ctx.Buffer_First = Ctx.Buffer_First'Old
          and Ctx.Buffer_Last = Ctx.Buffer_Last'Old
          and Ctx.First = Ctx.First'Old
          and Ctx.Last = Ctx.Last'Old
          and Ctx.Cursors (Fld).Predecessor = Ctx.Cursors (Fld).Predecessor'Old
          and Has_Buffer (Ctx) = Has_Buffer (Ctx)'Old
          and Field_First (Ctx, Fld) = Field_First (Ctx, Fld)'Old
          and Field_Length (Ctx, Fld) = Field_Length (Ctx, Fld)'Old
          and (case Fld is
               when F_Len =>
                  Invalid (Ctx, F_Len)
                     and Invalid (Ctx, F_Name),
               when F_Name =>
                  Ctx.Cursors (F_Len) = Ctx.Cursors (F_Len)'Old
                     and Invalid (Ctx, F_Name))
   is
      First : constant Types.Bit_Length := Field_First (Ctx, Fld) with
        Ghost;
      Length : constant Types.Bit_Length := Field_Length (Ctx, Fld) with
        Ghost;
   begin
      pragma Assert (Field_First (Ctx, Fld) = First
         and Field_Length (Ctx, Fld) = Length);
      case Fld is
         when F_Len =>
            Ctx.Cursors (F_Name) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Len) := (S_Invalid, Ctx.Cursors (F_Len).Predecessor);
            pragma Assert (Field_First (Ctx, Fld) = First
               and Field_Length (Ctx, Fld) = Length);
         when F_Name =>
            Ctx.Cursors (F_Name) := (S_Invalid, Ctx.Cursors (F_Name).Predecessor);
            pragma Assert (Field_First (Ctx, Fld) = First
               and Field_Length (Ctx, Fld) = Length);
      end case;
   end Reset_Dependent_Fields;

   function Sufficient_Buffer_Length (Ctx : Context; Fld : Field) return Boolean is
     (Ctx.Buffer /= null
      and Ctx.First <= Types.Bit_Index'Last / 2
      and Field_First (Ctx, Fld) <= Types.Bit_Index'Last / 2
      and Field_Length (Ctx, Fld) >= 0
      and Field_Length (Ctx, Fld) <= Types.Bit_Length'Last / 2
      and (Field_First (Ctx, Fld) + Field_Length (Ctx, Fld)) <= Types.Bit_Length'Last / 2
      and Ctx.First <= Field_First (Ctx, Fld)
      and Ctx.Last >= Field_Last (Ctx, Fld))
    with
     Pre =>
       Has_Buffer (Ctx)
          and Valid_Next (Ctx, Fld);

   function Composite_Field (Fld : Field) return Boolean is
     ((case Fld is
         when F_Len =>
            False,
         when F_Name =>
            True));

   function Get_Field_Value (Ctx : Context; Fld : Field) return Field_Dependent_Value with
     Pre =>
       Has_Buffer (Ctx)
          and then Valid_Next (Ctx, Fld)
          and then Sufficient_Buffer_Length (Ctx, Fld),
     Post =>
       Get_Field_Value'Result.Fld = Fld
   is
      First : constant Types.Bit_Index := Field_First (Ctx, Fld);
      Last : constant Types.Bit_Index := Field_Last (Ctx, Fld);
      function Buffer_First return Types.Index is
        (Types.Byte_Index (First));
      function Buffer_Last return Types.Index is
        (Types.Byte_Index (Last));
      function Offset return Types.Offset is
        (Types.Offset ((8 - Last mod 8) mod 8));
      function Extract is new Types.Extract (Parpen.Service_Manager.Len);
   begin
      return ((case Fld is
            when F_Len =>
               (Fld => F_Len, Len_Value => Extract (Ctx.Buffer.all (Buffer_First .. Buffer_Last), Offset)),
            when F_Name =>
               (Fld => F_Name)));
   end Get_Field_Value;

   procedure Verify (Ctx : in out Context; Fld : Field) is
      Value : Field_Dependent_Value;
   begin
      if Has_Buffer (Ctx)
         and then Invalid (Ctx.Cursors (Fld))
         and then Valid_Predecessor (Ctx, Fld)
         and then Path_Condition (Ctx, Fld) then
         if Sufficient_Buffer_Length (Ctx, Fld) then
            Value := Get_Field_Value (Ctx, Fld);
            if Valid_Value (Value)
               and Field_Condition (Ctx, Value) then
               if Composite_Field (Fld) then
                  Ctx.Cursors (Fld) := (State => S_Structural_Valid, First => Field_First (Ctx, Fld), Last => Field_Last (Ctx, Fld), Value => Value, Predecessor => Ctx.Cursors (Fld).Predecessor);
               else
                  Ctx.Cursors (Fld) := (State => S_Valid, First => Field_First (Ctx, Fld), Last => Field_Last (Ctx, Fld), Value => Value, Predecessor => Ctx.Cursors (Fld).Predecessor);
               end if;
               pragma Assert ((if Structural_Valid (Ctx.Cursors (F_Len)) then
                   (Ctx.Cursors (F_Len).Last - Ctx.Cursors (F_Len).First + 1) = Parpen.Service_Manager.Len'Size
                     and then Ctx.Cursors (F_Len).Predecessor = F_Initial
                     and then Ctx.Cursors (F_Len).First = Ctx.First
                     and then (if Structural_Valid (Ctx.Cursors (F_Name)) then
                        (Ctx.Cursors (F_Name).Last - Ctx.Cursors (F_Name).First + 1) = Types.Bit_Length (Ctx.Cursors (F_Len).Value.Len_Value) * 8
                          and then Ctx.Cursors (F_Name).Predecessor = F_Len
                          and then Ctx.Cursors (F_Name).First = (Ctx.Cursors (F_Len).Last + 1))));
               if Fld = F_Len then
                  Ctx.Cursors (Successor (Ctx, Fld)) := (State => S_Invalid, Predecessor => Fld);
               elsif Fld = F_Name then
                  Ctx.Cursors (Successor (Ctx, Fld)) := (State => S_Invalid, Predecessor => Fld);
               end if;
            else
               Ctx.Cursors (Fld) := (State => S_Invalid, Predecessor => F_Final);
            end if;
         else
            Ctx.Cursors (Fld) := (State => S_Incomplete, Predecessor => F_Final);
         end if;
      end if;
   end Verify;

   procedure Verify_Message (Ctx : in out Context) is
   begin
      Verify (Ctx, F_Len);
      Verify (Ctx, F_Name);
   end Verify_Message;

   function Present (Ctx : Context; Fld : Field) return Boolean is
     (Structural_Valid (Ctx.Cursors (Fld))
      and then Ctx.Cursors (Fld).First < (Ctx.Cursors (Fld).Last + 1));

   function Structural_Valid (Ctx : Context; Fld : Field) return Boolean is
     ((Ctx.Cursors (Fld).State = S_Valid
        or Ctx.Cursors (Fld).State = S_Structural_Valid));

   function Valid (Ctx : Context; Fld : Field) return Boolean is
     (Ctx.Cursors (Fld).State = S_Valid
      and then Ctx.Cursors (Fld).First < (Ctx.Cursors (Fld).Last + 1));

   function Incomplete (Ctx : Context; Fld : Field) return Boolean is
     (Ctx.Cursors (Fld).State = S_Incomplete);

   function Invalid (Ctx : Context; Fld : Field) return Boolean is
     (Ctx.Cursors (Fld).State = S_Invalid
      or Ctx.Cursors (Fld).State = S_Incomplete);

   function Structural_Valid_Message (Ctx : Context) return Boolean is
     (Valid (Ctx, F_Len)
      and then Structural_Valid (Ctx, F_Name));

   function Valid_Message (Ctx : Context) return Boolean is
     (Valid (Ctx, F_Len)
      and then Valid (Ctx, F_Name));

   function Incomplete_Message (Ctx : Context) return Boolean is
     (Incomplete (Ctx, F_Len)
      or Incomplete (Ctx, F_Name));

   function Get_Len (Ctx : Context) return Parpen.Service_Manager.Len is
     (Ctx.Cursors (F_Len).Value.Len_Value);

   procedure Get_Name (Ctx : Context) is
      First : constant Types.Index := Types.Byte_Index (Ctx.Cursors (F_Name).First);
      Last : constant Types.Index := Types.Byte_Index (Ctx.Cursors (F_Name).Last);
   begin
      Process_Name (Ctx.Buffer.all (First .. Last));
   end Get_Name;

   procedure Set_Field_Value (Ctx : in out Context; Val : Field_Dependent_Value; Fst, Lst : out Types.Bit_Index) with
     Pre =>
       not Ctx'Constrained
          and then Has_Buffer (Ctx)
          and then Val.Fld in Field'Range
          and then Valid_Next (Ctx, Val.Fld)
          and then Available_Space (Ctx, Val.Fld) >= Field_Length (Ctx, Val.Fld)
          and then (for all F in Field'Range =>
            (if Structural_Valid (Ctx.Cursors (F)) then
             Ctx.Cursors (F).Last <= Field_Last (Ctx, Val.Fld))),
     Post =>
       Has_Buffer (Ctx)
          and Fst = Field_First (Ctx, Val.Fld)
          and Lst = Field_Last (Ctx, Val.Fld)
          and Fst >= Ctx.First
          and Fst <= (Lst + 1)
          and Types.Byte_Index (Lst) <= Ctx.Buffer_Last
          and (for all F in Field'Range =>
            (if Structural_Valid (Ctx.Cursors (F)) then
             Ctx.Cursors (F).Last <= Lst))
          and Ctx.Buffer_First = Ctx.Buffer_First'Old
          and Ctx.Buffer_Last = Ctx.Buffer_Last'Old
          and Ctx.First = Ctx.First'Old
          and Ctx.Cursors = Ctx.Cursors'Old
   is
      First : constant Types.Bit_Index := Field_First (Ctx, Val.Fld);
      Last : constant Types.Bit_Index := Field_Last (Ctx, Val.Fld);
      function Buffer_First return Types.Index is
        (Types.Byte_Index (First));
      function Buffer_Last return Types.Index is
        (Types.Byte_Index (Last));
      function Offset return Types.Offset is
        (Types.Offset ((8 - Last mod 8) mod 8));
      procedure Insert is new Types.Insert (Parpen.Service_Manager.Len);
   begin
      Fst := First;
      Lst := Last;
      case Val.Fld is
         when F_Initial =>
            null;
         when F_Len =>
            Insert (Val.Len_Value, Ctx.Buffer.all (Buffer_First .. Buffer_Last), Offset);
         when F_Name | F_Final =>
            null;
      end case;
   end Set_Field_Value;

   procedure Set_Len (Ctx : in out Context; Val : Parpen.Service_Manager.Len) is
      Field_Value : constant Field_Dependent_Value := (F_Len, Val);
      First, Last : Types.Bit_Index;
   begin
      Reset_Dependent_Fields (Ctx, F_Len);
      Set_Field_Value (Ctx, Field_Value, First, Last);
      Ctx := (Ctx.Buffer_First, Ctx.Buffer_Last, Ctx.First, Last, Ctx.Buffer, Ctx.Cursors);
      Ctx.Cursors (F_Len) := (State => S_Valid, First => First, Last => Last, Value => Field_Value, Predecessor => Ctx.Cursors (F_Len).Predecessor);
      Ctx.Cursors (Successor (Ctx, F_Len)) := (State => S_Invalid, Predecessor => F_Len);
   end Set_Len;

   procedure Set_Name (Ctx : in out Context) is
      First : constant Types.Bit_Index := Field_First (Ctx, F_Name);
      Last : constant Types.Bit_Index := Field_Last (Ctx, F_Name);
      function Buffer_First return Types.Index is
        (Types.Byte_Index (First));
      function Buffer_Last return Types.Index is
        (Types.Byte_Index (Last));
   begin
      Initialize_Name (Ctx);
      Process_Name (Ctx.Buffer.all (Buffer_First .. Buffer_Last));
   end Set_Name;

   procedure Initialize_Name (Ctx : in out Context) is
      First : constant Types.Bit_Index := Field_First (Ctx, F_Name);
      Last : constant Types.Bit_Index := Field_Last (Ctx, F_Name);
   begin
      Reset_Dependent_Fields (Ctx, F_Name);
      Ctx := (Ctx.Buffer_First, Ctx.Buffer_Last, Ctx.First, Last, Ctx.Buffer, Ctx.Cursors);
      pragma Assert ((if Structural_Valid (Ctx.Cursors (F_Len)) then
          (Ctx.Cursors (F_Len).Last - Ctx.Cursors (F_Len).First + 1) = Parpen.Service_Manager.Len'Size
            and then Ctx.Cursors (F_Len).Predecessor = F_Initial
            and then Ctx.Cursors (F_Len).First = Ctx.First
            and then (if Structural_Valid (Ctx.Cursors (F_Name)) then
               (Ctx.Cursors (F_Name).Last - Ctx.Cursors (F_Name).First + 1) = Types.Bit_Length (Ctx.Cursors (F_Len).Value.Len_Value) * 8
                 and then Ctx.Cursors (F_Name).Predecessor = F_Len
                 and then Ctx.Cursors (F_Name).First = (Ctx.Cursors (F_Len).Last + 1))));
      Ctx.Cursors (F_Name) := (State => S_Structural_Valid, First => First, Last => Last, Value => (Fld => F_Name), Predecessor => Ctx.Cursors (F_Name).Predecessor);
      Ctx.Cursors (Successor (Ctx, F_Name)) := (State => S_Invalid, Predecessor => F_Name);
   end Initialize_Name;

end Parpen.Service_Manager.Generic_Request_Get_Service;
