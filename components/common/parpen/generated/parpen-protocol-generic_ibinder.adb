package body Parpen.Protocol.Generic_IBinder with
  SPARK_Mode
is

   function Create return Context is
     ((Types.Index'First, Types.Index'First, Types.Bit_Index'First, Types.Bit_Index'First, null, (F_Kind => (State => S_Invalid, Predecessor => F_Initial), others => (State => S_Invalid, Predecessor => F_Final))));

   procedure Initialize (Ctx : out Context; Buffer : in out Types.Bytes_Ptr) is
   begin
      Initialize (Ctx, Buffer, Types.First_Bit_Index (Buffer'First), Types.Last_Bit_Index (Buffer'Last));
   end Initialize;

   procedure Initialize (Ctx : out Context; Buffer : in out Types.Bytes_Ptr; First, Last : Types.Bit_Index) is
      Buffer_First : constant Types.Index := Buffer'First;
      Buffer_Last : constant Types.Index := Buffer'Last;
   begin
      Ctx := (Buffer_First, Buffer_Last, First, Last, Buffer, (F_Kind => (State => S_Invalid, Predecessor => F_Initial), others => (State => S_Invalid, Predecessor => F_Final)));
      Buffer := null;
   end Initialize;

   function Initialized (Ctx : Context) return Boolean is
     (Valid_Next (Ctx, F_Kind)
      and then Available_Space (Ctx, F_Kind) = (Types.Last_Bit_Index (Ctx.Buffer_Last) - Ctx.First + 1)
      and then Invalid (Ctx, F_Kind)
      and then Invalid (Ctx, F_Arity)
      and then Invalid (Ctx, F_Tag)
      and then Invalid (Ctx, F_Flags)
      and then Invalid (Ctx, F_Binder)
      and then Invalid (Ctx, F_Handle)
      and then Invalid (Ctx, F_Cookie));

   procedure Take_Buffer (Ctx : in out Context; Buffer : out Types.Bytes_Ptr) is
   begin
      Buffer := Ctx.Buffer;
      Ctx.Buffer := null;
   end Take_Buffer;

   function Has_Buffer (Ctx : Context) return Boolean is
     (Ctx.Buffer /= null);

   function Message_Last (Ctx : Context) return Types.Bit_Index is
     ((if Structural_Valid (Ctx.Cursors (F_Cookie)) then
       Ctx.Cursors (F_Cookie).Last
    else
       Types.Unreachable_Bit_Length));

   function Path_Condition (Ctx : Context; Fld : Field) return Boolean is
     ((case Ctx.Cursors (Fld).Predecessor is
         when F_Initial =>
            (case Fld is
                  when F_Kind =>
                     True,
                  when others =>
                     False),
         when F_Kind =>
            (case Fld is
                  when F_Arity =>
                     True,
                  when others =>
                     False),
         when F_Arity =>
            (case Fld is
                  when F_Tag =>
                     Types.Bit_Length (Ctx.Cursors (F_Arity).Value.Arity_Value) = Types.Bit_Length (Convert (BA_SINGLE))
                        or (Types.Bit_Length (Ctx.Cursors (F_Kind).Value.Kind_Value) = Types.Bit_Length (Convert (BK_FD))
                          and Types.Bit_Length (Ctx.Cursors (F_Arity).Value.Arity_Value) = Types.Bit_Length (Convert (BA_ARRAY))),
                  when others =>
                     False),
         when F_Tag =>
            (case Fld is
                  when F_Flags =>
                     True,
                  when others =>
                     False),
         when F_Flags =>
            (case Fld is
                  when F_Binder =>
                     Types.Bit_Length (Ctx.Cursors (F_Kind).Value.Kind_Value) = Types.Bit_Length (Convert (BK_STRONG_BINDER))
                        or Types.Bit_Length (Ctx.Cursors (F_Kind).Value.Kind_Value) = Types.Bit_Length (Convert (BK_WEAK_BINDER)),
                  when F_Handle =>
                     Types.Bit_Length (Ctx.Cursors (F_Kind).Value.Kind_Value) = Types.Bit_Length (Convert (BK_STRONG_HANDLE))
                        or Types.Bit_Length (Ctx.Cursors (F_Kind).Value.Kind_Value) = Types.Bit_Length (Convert (BK_WEAK_HANDLE)),
                  when others =>
                     False),
         when F_Binder | F_Handle =>
            (case Fld is
                  when F_Cookie =>
                     True,
                  when others =>
                     False),
         when F_Cookie | F_Final =>
            False));

   function Field_Condition (Ctx : Context; Val : Field_Dependent_Value) return Boolean is
     ((case Val.Fld is
         when F_Initial | F_Kind =>
            True,
         when F_Arity =>
            Types.Bit_Length (Val.Arity_Value) = Types.Bit_Length (Convert (BA_SINGLE))
               or (Types.Bit_Length (Ctx.Cursors (F_Kind).Value.Kind_Value) = Types.Bit_Length (Convert (BK_FD))
                 and Types.Bit_Length (Val.Arity_Value) = Types.Bit_Length (Convert (BA_ARRAY))),
         when F_Tag =>
            True,
         when F_Flags =>
            Types.Bit_Length (Ctx.Cursors (F_Kind).Value.Kind_Value) = Types.Bit_Length (Convert (BK_STRONG_BINDER))
               or Types.Bit_Length (Ctx.Cursors (F_Kind).Value.Kind_Value) = Types.Bit_Length (Convert (BK_WEAK_BINDER))
               or Types.Bit_Length (Ctx.Cursors (F_Kind).Value.Kind_Value) = Types.Bit_Length (Convert (BK_STRONG_HANDLE))
               or Types.Bit_Length (Ctx.Cursors (F_Kind).Value.Kind_Value) = Types.Bit_Length (Convert (BK_WEAK_HANDLE)),
         when F_Binder | F_Handle | F_Cookie =>
            True,
         when F_Final =>
            False));

   function Field_Length (Ctx : Context; Fld : Field) return Types.Bit_Length is
     ((case Ctx.Cursors (Fld).Predecessor is
         when F_Initial =>
            (case Fld is
                  when F_Kind =>
                     Protocol.Binder_Kind_Base'Size,
                  when others =>
                     Types.Unreachable_Bit_Length),
         when F_Kind =>
            (case Fld is
                  when F_Arity =>
                     Protocol.Binder_Arity_Base'Size,
                  when others =>
                     Types.Unreachable_Bit_Length),
         when F_Arity =>
            (case Fld is
                  when F_Tag =>
                     Protocol.Binder_Tag_Base'Size,
                  when others =>
                     Types.Unreachable_Bit_Length),
         when F_Tag =>
            (case Fld is
                  when F_Flags =>
                     Protocol.Flat_Binder_Flags_Base'Size,
                  when others =>
                     Types.Unreachable_Bit_Length),
         when F_Flags =>
            (case Fld is
                  when F_Binder =>
                     Protocol.Binder'Size,
                  when F_Handle =>
                     Protocol.Handle_Base'Size,
                  when others =>
                     Types.Unreachable_Bit_Length),
         when F_Binder | F_Handle =>
            (case Fld is
                  when F_Cookie =>
                     Protocol.Cookie'Size,
                  when others =>
                     Types.Unreachable_Bit_Length),
         when F_Cookie | F_Final =>
            0));

   function Field_First (Ctx : Context; Fld : Field) return Types.Bit_Index is
     ((case Fld is
         when F_Kind =>
            Ctx.First,
         when F_Arity =>
            (if Ctx.Cursors (Fld).Predecessor = F_Kind then
                (Ctx.Cursors (Ctx.Cursors (Fld).Predecessor).Last + 1)
             else
                Types.Unreachable_Bit_Length),
         when F_Tag =>
            (if Ctx.Cursors (Fld).Predecessor = F_Arity
                  and (Types.Bit_Length (Ctx.Cursors (F_Arity).Value.Arity_Value) = Types.Bit_Length (Convert (BA_SINGLE))
                    or (Types.Bit_Length (Ctx.Cursors (F_Kind).Value.Kind_Value) = Types.Bit_Length (Convert (BK_FD))
                      and Types.Bit_Length (Ctx.Cursors (F_Arity).Value.Arity_Value) = Types.Bit_Length (Convert (BA_ARRAY)))) then
                (Ctx.Cursors (Ctx.Cursors (Fld).Predecessor).Last + 1)
             else
                Types.Unreachable_Bit_Length),
         when F_Flags =>
            (if Ctx.Cursors (Fld).Predecessor = F_Tag then
                (Ctx.Cursors (Ctx.Cursors (Fld).Predecessor).Last + 1)
             else
                Types.Unreachable_Bit_Length),
         when F_Binder =>
            (if Ctx.Cursors (Fld).Predecessor = F_Flags
                  and (Types.Bit_Length (Ctx.Cursors (F_Kind).Value.Kind_Value) = Types.Bit_Length (Convert (BK_STRONG_BINDER))
                    or Types.Bit_Length (Ctx.Cursors (F_Kind).Value.Kind_Value) = Types.Bit_Length (Convert (BK_WEAK_BINDER))) then
                (Ctx.Cursors (Ctx.Cursors (Fld).Predecessor).Last + 1)
             else
                Types.Unreachable_Bit_Length),
         when F_Handle =>
            (if Ctx.Cursors (Fld).Predecessor = F_Flags
                  and (Types.Bit_Length (Ctx.Cursors (F_Kind).Value.Kind_Value) = Types.Bit_Length (Convert (BK_STRONG_HANDLE))
                    or Types.Bit_Length (Ctx.Cursors (F_Kind).Value.Kind_Value) = Types.Bit_Length (Convert (BK_WEAK_HANDLE))) then
                (Ctx.Cursors (Ctx.Cursors (Fld).Predecessor).Last + 1)
             else
                Types.Unreachable_Bit_Length),
         when F_Cookie =>
            (if Ctx.Cursors (Fld).Predecessor = F_Binder then
                (Ctx.Cursors (Ctx.Cursors (Fld).Predecessor).Last + 1)
             elsif Ctx.Cursors (Fld).Predecessor = F_Handle then
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
         when F_Kind =>
            F_Arity,
         when F_Arity =>
            (if Types.Bit_Length (Ctx.Cursors (F_Arity).Value.Arity_Value) = Types.Bit_Length (Convert (BA_SINGLE))
                  or (Types.Bit_Length (Ctx.Cursors (F_Kind).Value.Kind_Value) = Types.Bit_Length (Convert (BK_FD))
                    and Types.Bit_Length (Ctx.Cursors (F_Arity).Value.Arity_Value) = Types.Bit_Length (Convert (BA_ARRAY))) then
                F_Tag
             else
                F_Initial),
         when F_Tag =>
            F_Flags,
         when F_Flags =>
            (if Types.Bit_Length (Ctx.Cursors (F_Kind).Value.Kind_Value) = Types.Bit_Length (Convert (BK_STRONG_BINDER))
                  or Types.Bit_Length (Ctx.Cursors (F_Kind).Value.Kind_Value) = Types.Bit_Length (Convert (BK_WEAK_BINDER)) then
                F_Binder
             elsif Types.Bit_Length (Ctx.Cursors (F_Kind).Value.Kind_Value) = Types.Bit_Length (Convert (BK_STRONG_HANDLE))
                     or Types.Bit_Length (Ctx.Cursors (F_Kind).Value.Kind_Value) = Types.Bit_Length (Convert (BK_WEAK_HANDLE)) then
                F_Handle
             else
                F_Initial),
         when F_Binder | F_Handle =>
            F_Cookie,
         when F_Cookie =>
            F_Final))
    with
     Pre =>
       Structural_Valid (Ctx, Fld)
          and Valid_Predecessor (Ctx, Fld);

   function Valid_Predecessor (Ctx : Context; Fld : Virtual_Field) return Boolean is
     ((case Fld is
         when F_Initial =>
            True,
         when F_Kind =>
            Ctx.Cursors (Fld).Predecessor = F_Initial,
         when F_Arity =>
            (Valid (Ctx.Cursors (F_Kind))
                 and Ctx.Cursors (Fld).Predecessor = F_Kind),
         when F_Tag =>
            (Valid (Ctx.Cursors (F_Arity))
                 and Ctx.Cursors (Fld).Predecessor = F_Arity),
         when F_Flags =>
            (Valid (Ctx.Cursors (F_Tag))
                 and Ctx.Cursors (Fld).Predecessor = F_Tag),
         when F_Binder | F_Handle =>
            (Valid (Ctx.Cursors (F_Flags))
                 and Ctx.Cursors (Fld).Predecessor = F_Flags),
         when F_Cookie =>
            (Valid (Ctx.Cursors (F_Binder))
                 and Ctx.Cursors (Fld).Predecessor = F_Binder)
               or (Valid (Ctx.Cursors (F_Handle))
                 and Ctx.Cursors (Fld).Predecessor = F_Handle),
         when F_Final =>
            (Valid (Ctx.Cursors (F_Cookie))
                 and Ctx.Cursors (Fld).Predecessor = F_Cookie)));

   function Invalid_Successor (Ctx : Context; Fld : Field) return Boolean is
     ((case Fld is
         when F_Kind =>
            Invalid (Ctx.Cursors (F_Arity)),
         when F_Arity =>
            Invalid (Ctx.Cursors (F_Tag)),
         when F_Tag =>
            Invalid (Ctx.Cursors (F_Flags)),
         when F_Flags =>
            Invalid (Ctx.Cursors (F_Binder))
               and Invalid (Ctx.Cursors (F_Handle)),
         when F_Binder | F_Handle =>
            Invalid (Ctx.Cursors (F_Cookie)),
         when F_Cookie =>
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
               when F_Kind =>
                  Invalid (Ctx, F_Kind)
                     and Invalid (Ctx, F_Arity)
                     and Invalid (Ctx, F_Tag)
                     and Invalid (Ctx, F_Flags)
                     and Invalid (Ctx, F_Binder)
                     and Invalid (Ctx, F_Handle)
                     and Invalid (Ctx, F_Cookie),
               when F_Arity =>
                  Ctx.Cursors (F_Kind) = Ctx.Cursors (F_Kind)'Old
                     and Invalid (Ctx, F_Arity)
                     and Invalid (Ctx, F_Tag)
                     and Invalid (Ctx, F_Flags)
                     and Invalid (Ctx, F_Binder)
                     and Invalid (Ctx, F_Handle)
                     and Invalid (Ctx, F_Cookie),
               when F_Tag =>
                  Ctx.Cursors (F_Kind) = Ctx.Cursors (F_Kind)'Old
                     and Ctx.Cursors (F_Arity) = Ctx.Cursors (F_Arity)'Old
                     and Invalid (Ctx, F_Tag)
                     and Invalid (Ctx, F_Flags)
                     and Invalid (Ctx, F_Binder)
                     and Invalid (Ctx, F_Handle)
                     and Invalid (Ctx, F_Cookie),
               when F_Flags =>
                  Ctx.Cursors (F_Kind) = Ctx.Cursors (F_Kind)'Old
                     and Ctx.Cursors (F_Arity) = Ctx.Cursors (F_Arity)'Old
                     and Ctx.Cursors (F_Tag) = Ctx.Cursors (F_Tag)'Old
                     and Invalid (Ctx, F_Flags)
                     and Invalid (Ctx, F_Binder)
                     and Invalid (Ctx, F_Handle)
                     and Invalid (Ctx, F_Cookie),
               when F_Binder =>
                  Ctx.Cursors (F_Kind) = Ctx.Cursors (F_Kind)'Old
                     and Ctx.Cursors (F_Arity) = Ctx.Cursors (F_Arity)'Old
                     and Ctx.Cursors (F_Tag) = Ctx.Cursors (F_Tag)'Old
                     and Ctx.Cursors (F_Flags) = Ctx.Cursors (F_Flags)'Old
                     and Invalid (Ctx, F_Binder)
                     and Invalid (Ctx, F_Handle)
                     and Invalid (Ctx, F_Cookie),
               when F_Handle =>
                  Ctx.Cursors (F_Kind) = Ctx.Cursors (F_Kind)'Old
                     and Ctx.Cursors (F_Arity) = Ctx.Cursors (F_Arity)'Old
                     and Ctx.Cursors (F_Tag) = Ctx.Cursors (F_Tag)'Old
                     and Ctx.Cursors (F_Flags) = Ctx.Cursors (F_Flags)'Old
                     and Ctx.Cursors (F_Binder) = Ctx.Cursors (F_Binder)'Old
                     and Invalid (Ctx, F_Handle)
                     and Invalid (Ctx, F_Cookie),
               when F_Cookie =>
                  Ctx.Cursors (F_Kind) = Ctx.Cursors (F_Kind)'Old
                     and Ctx.Cursors (F_Arity) = Ctx.Cursors (F_Arity)'Old
                     and Ctx.Cursors (F_Tag) = Ctx.Cursors (F_Tag)'Old
                     and Ctx.Cursors (F_Flags) = Ctx.Cursors (F_Flags)'Old
                     and Ctx.Cursors (F_Binder) = Ctx.Cursors (F_Binder)'Old
                     and Ctx.Cursors (F_Handle) = Ctx.Cursors (F_Handle)'Old
                     and Invalid (Ctx, F_Cookie))
   is
      First : constant Types.Bit_Length := Field_First (Ctx, Fld) with
        Ghost;
      Length : constant Types.Bit_Length := Field_Length (Ctx, Fld) with
        Ghost;
   begin
      pragma Assert (Field_First (Ctx, Fld) = First
         and Field_Length (Ctx, Fld) = Length);
      case Fld is
         when F_Kind =>
            Ctx.Cursors (F_Cookie) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Handle) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Binder) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Flags) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Tag) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Arity) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Kind) := (S_Invalid, Ctx.Cursors (F_Kind).Predecessor);
            pragma Assert (Field_First (Ctx, Fld) = First
               and Field_Length (Ctx, Fld) = Length);
         when F_Arity =>
            Ctx.Cursors (F_Cookie) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Handle) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Binder) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Flags) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Tag) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Arity) := (S_Invalid, Ctx.Cursors (F_Arity).Predecessor);
            pragma Assert (Field_First (Ctx, Fld) = First
               and Field_Length (Ctx, Fld) = Length);
         when F_Tag =>
            Ctx.Cursors (F_Cookie) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Handle) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Binder) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Flags) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Tag) := (S_Invalid, Ctx.Cursors (F_Tag).Predecessor);
            pragma Assert (Field_First (Ctx, Fld) = First
               and Field_Length (Ctx, Fld) = Length);
         when F_Flags =>
            Ctx.Cursors (F_Cookie) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Handle) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Binder) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Flags) := (S_Invalid, Ctx.Cursors (F_Flags).Predecessor);
            pragma Assert (Field_First (Ctx, Fld) = First
               and Field_Length (Ctx, Fld) = Length);
         when F_Binder =>
            Ctx.Cursors (F_Cookie) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Handle) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Binder) := (S_Invalid, Ctx.Cursors (F_Binder).Predecessor);
            pragma Assert (Field_First (Ctx, Fld) = First
               and Field_Length (Ctx, Fld) = Length);
         when F_Handle =>
            Ctx.Cursors (F_Cookie) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Handle) := (S_Invalid, Ctx.Cursors (F_Handle).Predecessor);
            pragma Assert (Field_First (Ctx, Fld) = First
               and Field_Length (Ctx, Fld) = Length);
         when F_Cookie =>
            Ctx.Cursors (F_Cookie) := (S_Invalid, Ctx.Cursors (F_Cookie).Predecessor);
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
         when F_Kind | F_Arity | F_Tag | F_Flags | F_Binder | F_Handle | F_Cookie =>
            False));

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
      function Extract is new Types.Extract (Protocol.Binder_Kind_Base);
      function Extract is new Types.Extract (Protocol.Binder_Arity_Base);
      function Extract is new Types.Extract (Protocol.Binder_Tag_Base);
      function Extract is new Types.Extract (Protocol.Flat_Binder_Flags_Base);
      function Extract is new Types.Extract (Protocol.Binder);
      function Extract is new Types.Extract (Protocol.Handle_Base);
      function Extract is new Types.Extract (Protocol.Cookie);
   begin
      return ((case Fld is
            when F_Kind =>
               (Fld => F_Kind, Kind_Value => Extract (Ctx.Buffer.all (Buffer_First .. Buffer_Last), Offset)),
            when F_Arity =>
               (Fld => F_Arity, Arity_Value => Extract (Ctx.Buffer.all (Buffer_First .. Buffer_Last), Offset)),
            when F_Tag =>
               (Fld => F_Tag, Tag_Value => Extract (Ctx.Buffer.all (Buffer_First .. Buffer_Last), Offset)),
            when F_Flags =>
               (Fld => F_Flags, Flags_Value => Extract (Ctx.Buffer.all (Buffer_First .. Buffer_Last), Offset)),
            when F_Binder =>
               (Fld => F_Binder, Binder_Value => Extract (Ctx.Buffer.all (Buffer_First .. Buffer_Last), Offset)),
            when F_Handle =>
               (Fld => F_Handle, Handle_Value => Extract (Ctx.Buffer.all (Buffer_First .. Buffer_Last), Offset)),
            when F_Cookie =>
               (Fld => F_Cookie, Cookie_Value => Extract (Ctx.Buffer.all (Buffer_First .. Buffer_Last), Offset))));
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
               pragma Assert ((if Structural_Valid (Ctx.Cursors (F_Kind)) then
                   (Ctx.Cursors (F_Kind).Last - Ctx.Cursors (F_Kind).First + 1) = Protocol.Binder_Kind_Base'Size
                     and then Ctx.Cursors (F_Kind).Predecessor = F_Initial
                     and then Ctx.Cursors (F_Kind).First = Ctx.First
                     and then (if Structural_Valid (Ctx.Cursors (F_Arity)) then
                        (Ctx.Cursors (F_Arity).Last - Ctx.Cursors (F_Arity).First + 1) = Protocol.Binder_Arity_Base'Size
                          and then Ctx.Cursors (F_Arity).Predecessor = F_Kind
                          and then Ctx.Cursors (F_Arity).First = (Ctx.Cursors (F_Kind).Last + 1)
                          and then (if Structural_Valid (Ctx.Cursors (F_Tag))
                               and then (Types.Bit_Length (Ctx.Cursors (F_Arity).Value.Arity_Value) = Types.Bit_Length (Convert (BA_SINGLE))
                                 or (Types.Bit_Length (Ctx.Cursors (F_Kind).Value.Kind_Value) = Types.Bit_Length (Convert (BK_FD))
                                   and Types.Bit_Length (Ctx.Cursors (F_Arity).Value.Arity_Value) = Types.Bit_Length (Convert (BA_ARRAY)))) then
                             (Ctx.Cursors (F_Tag).Last - Ctx.Cursors (F_Tag).First + 1) = Protocol.Binder_Tag_Base'Size
                               and then Ctx.Cursors (F_Tag).Predecessor = F_Arity
                               and then Ctx.Cursors (F_Tag).First = (Ctx.Cursors (F_Arity).Last + 1)
                               and then (if Structural_Valid (Ctx.Cursors (F_Flags)) then
                                  (Ctx.Cursors (F_Flags).Last - Ctx.Cursors (F_Flags).First + 1) = Protocol.Flat_Binder_Flags_Base'Size
                                    and then Ctx.Cursors (F_Flags).Predecessor = F_Tag
                                    and then Ctx.Cursors (F_Flags).First = (Ctx.Cursors (F_Tag).Last + 1)
                                    and then (if Structural_Valid (Ctx.Cursors (F_Binder))
                                         and then (Types.Bit_Length (Ctx.Cursors (F_Kind).Value.Kind_Value) = Types.Bit_Length (Convert (BK_STRONG_BINDER))
                                           or Types.Bit_Length (Ctx.Cursors (F_Kind).Value.Kind_Value) = Types.Bit_Length (Convert (BK_WEAK_BINDER))) then
                                       (Ctx.Cursors (F_Binder).Last - Ctx.Cursors (F_Binder).First + 1) = Protocol.Binder'Size
                                         and then Ctx.Cursors (F_Binder).Predecessor = F_Flags
                                         and then Ctx.Cursors (F_Binder).First = (Ctx.Cursors (F_Flags).Last + 1)
                                         and then (if Structural_Valid (Ctx.Cursors (F_Cookie)) then
                                            (Ctx.Cursors (F_Cookie).Last - Ctx.Cursors (F_Cookie).First + 1) = Protocol.Cookie'Size
                                              and then Ctx.Cursors (F_Cookie).Predecessor = F_Binder
                                              and then Ctx.Cursors (F_Cookie).First = (Ctx.Cursors (F_Binder).Last + 1)))
                                    and then (if Structural_Valid (Ctx.Cursors (F_Handle))
                                         and then (Types.Bit_Length (Ctx.Cursors (F_Kind).Value.Kind_Value) = Types.Bit_Length (Convert (BK_STRONG_HANDLE))
                                           or Types.Bit_Length (Ctx.Cursors (F_Kind).Value.Kind_Value) = Types.Bit_Length (Convert (BK_WEAK_HANDLE))) then
                                       (Ctx.Cursors (F_Handle).Last - Ctx.Cursors (F_Handle).First + 1) = Protocol.Handle_Base'Size
                                         and then Ctx.Cursors (F_Handle).Predecessor = F_Flags
                                         and then Ctx.Cursors (F_Handle).First = (Ctx.Cursors (F_Flags).Last + 1)
                                         and then (if Structural_Valid (Ctx.Cursors (F_Cookie)) then
                                            (Ctx.Cursors (F_Cookie).Last - Ctx.Cursors (F_Cookie).First + 1) = Protocol.Cookie'Size
                                              and then Ctx.Cursors (F_Cookie).Predecessor = F_Handle
                                              and then Ctx.Cursors (F_Cookie).First = (Ctx.Cursors (F_Handle).Last + 1))))))));
               if Fld = F_Kind then
                  Ctx.Cursors (Successor (Ctx, Fld)) := (State => S_Invalid, Predecessor => Fld);
               elsif Fld = F_Arity then
                  Ctx.Cursors (Successor (Ctx, Fld)) := (State => S_Invalid, Predecessor => Fld);
               elsif Fld = F_Tag then
                  Ctx.Cursors (Successor (Ctx, Fld)) := (State => S_Invalid, Predecessor => Fld);
               elsif Fld = F_Flags then
                  Ctx.Cursors (Successor (Ctx, Fld)) := (State => S_Invalid, Predecessor => Fld);
               elsif Fld = F_Binder then
                  Ctx.Cursors (Successor (Ctx, Fld)) := (State => S_Invalid, Predecessor => Fld);
               elsif Fld = F_Handle then
                  Ctx.Cursors (Successor (Ctx, Fld)) := (State => S_Invalid, Predecessor => Fld);
               elsif Fld = F_Cookie then
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
      Verify (Ctx, F_Kind);
      Verify (Ctx, F_Arity);
      Verify (Ctx, F_Tag);
      Verify (Ctx, F_Flags);
      Verify (Ctx, F_Binder);
      Verify (Ctx, F_Handle);
      Verify (Ctx, F_Cookie);
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
     (Valid (Ctx, F_Kind)
      and then Valid (Ctx, F_Arity)
      and then Valid (Ctx, F_Tag)
      and then (Types.Bit_Length (Ctx.Cursors (F_Arity).Value.Arity_Value) = Types.Bit_Length (Convert (BA_SINGLE))
        or (Types.Bit_Length (Ctx.Cursors (F_Kind).Value.Kind_Value) = Types.Bit_Length (Convert (BK_FD))
          and Types.Bit_Length (Ctx.Cursors (F_Arity).Value.Arity_Value) = Types.Bit_Length (Convert (BA_ARRAY))))
      and then Valid (Ctx, F_Flags)
      and then ((Valid (Ctx, F_Binder)
          and then (Types.Bit_Length (Ctx.Cursors (F_Kind).Value.Kind_Value) = Types.Bit_Length (Convert (BK_STRONG_BINDER))
            or Types.Bit_Length (Ctx.Cursors (F_Kind).Value.Kind_Value) = Types.Bit_Length (Convert (BK_WEAK_BINDER)))
          and then Valid (Ctx, F_Cookie))
        or (Valid (Ctx, F_Handle)
          and then (Types.Bit_Length (Ctx.Cursors (F_Kind).Value.Kind_Value) = Types.Bit_Length (Convert (BK_STRONG_HANDLE))
            or Types.Bit_Length (Ctx.Cursors (F_Kind).Value.Kind_Value) = Types.Bit_Length (Convert (BK_WEAK_HANDLE)))
          and then Valid (Ctx, F_Cookie))));

   function Valid_Message (Ctx : Context) return Boolean is
     (Valid (Ctx, F_Kind)
      and then Valid (Ctx, F_Arity)
      and then Valid (Ctx, F_Tag)
      and then (Types.Bit_Length (Ctx.Cursors (F_Arity).Value.Arity_Value) = Types.Bit_Length (Convert (BA_SINGLE))
        or (Types.Bit_Length (Ctx.Cursors (F_Kind).Value.Kind_Value) = Types.Bit_Length (Convert (BK_FD))
          and Types.Bit_Length (Ctx.Cursors (F_Arity).Value.Arity_Value) = Types.Bit_Length (Convert (BA_ARRAY))))
      and then Valid (Ctx, F_Flags)
      and then ((Valid (Ctx, F_Binder)
          and then (Types.Bit_Length (Ctx.Cursors (F_Kind).Value.Kind_Value) = Types.Bit_Length (Convert (BK_STRONG_BINDER))
            or Types.Bit_Length (Ctx.Cursors (F_Kind).Value.Kind_Value) = Types.Bit_Length (Convert (BK_WEAK_BINDER)))
          and then Valid (Ctx, F_Cookie))
        or (Valid (Ctx, F_Handle)
          and then (Types.Bit_Length (Ctx.Cursors (F_Kind).Value.Kind_Value) = Types.Bit_Length (Convert (BK_STRONG_HANDLE))
            or Types.Bit_Length (Ctx.Cursors (F_Kind).Value.Kind_Value) = Types.Bit_Length (Convert (BK_WEAK_HANDLE)))
          and then Valid (Ctx, F_Cookie))));

   function Incomplete_Message (Ctx : Context) return Boolean is
     (Incomplete (Ctx, F_Kind)
      or Incomplete (Ctx, F_Arity)
      or Incomplete (Ctx, F_Tag)
      or Incomplete (Ctx, F_Flags)
      or Incomplete (Ctx, F_Binder)
      or Incomplete (Ctx, F_Handle)
      or Incomplete (Ctx, F_Cookie));

   function Get_Kind (Ctx : Context) return Protocol.Binder_Kind is
     (Convert (Ctx.Cursors (F_Kind).Value.Kind_Value));

   function Get_Arity (Ctx : Context) return Protocol.Binder_Arity is
     (Convert (Ctx.Cursors (F_Arity).Value.Arity_Value));

   function Get_Tag (Ctx : Context) return Protocol.Binder_Tag is
     (Ctx.Cursors (F_Tag).Value.Tag_Value);

   function Get_Flags (Ctx : Context) return Protocol.Flat_Binder_Flags is
     (Convert (Ctx.Cursors (F_Flags).Value.Flags_Value));

   function Get_Binder (Ctx : Context) return Protocol.Binder is
     (Ctx.Cursors (F_Binder).Value.Binder_Value);

   function Get_Handle (Ctx : Context) return Protocol.Handle is
     (Ctx.Cursors (F_Handle).Value.Handle_Value);

   function Get_Cookie (Ctx : Context) return Protocol.Cookie is
     (Ctx.Cursors (F_Cookie).Value.Cookie_Value);

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
      procedure Insert is new Types.Insert (Protocol.Binder_Kind_Base);
      procedure Insert is new Types.Insert (Protocol.Binder_Arity_Base);
      procedure Insert is new Types.Insert (Protocol.Binder_Tag_Base);
      procedure Insert is new Types.Insert (Protocol.Flat_Binder_Flags_Base);
      procedure Insert is new Types.Insert (Protocol.Binder);
      procedure Insert is new Types.Insert (Protocol.Handle_Base);
      procedure Insert is new Types.Insert (Protocol.Cookie);
   begin
      Fst := First;
      Lst := Last;
      case Val.Fld is
         when F_Initial =>
            null;
         when F_Kind =>
            Insert (Val.Kind_Value, Ctx.Buffer.all (Buffer_First .. Buffer_Last), Offset);
         when F_Arity =>
            Insert (Val.Arity_Value, Ctx.Buffer.all (Buffer_First .. Buffer_Last), Offset);
         when F_Tag =>
            Insert (Val.Tag_Value, Ctx.Buffer.all (Buffer_First .. Buffer_Last), Offset);
         when F_Flags =>
            Insert (Val.Flags_Value, Ctx.Buffer.all (Buffer_First .. Buffer_Last), Offset);
         when F_Binder =>
            Insert (Val.Binder_Value, Ctx.Buffer.all (Buffer_First .. Buffer_Last), Offset);
         when F_Handle =>
            Insert (Val.Handle_Value, Ctx.Buffer.all (Buffer_First .. Buffer_Last), Offset);
         when F_Cookie =>
            Insert (Val.Cookie_Value, Ctx.Buffer.all (Buffer_First .. Buffer_Last), Offset);
         when F_Final =>
            null;
      end case;
   end Set_Field_Value;

   procedure Set_Kind (Ctx : in out Context; Val : Protocol.Binder_Kind) is
      Field_Value : constant Field_Dependent_Value := (F_Kind, Convert (Val));
      First, Last : Types.Bit_Index;
   begin
      Reset_Dependent_Fields (Ctx, F_Kind);
      Set_Field_Value (Ctx, Field_Value, First, Last);
      Ctx := (Ctx.Buffer_First, Ctx.Buffer_Last, Ctx.First, Last, Ctx.Buffer, Ctx.Cursors);
      Ctx.Cursors (F_Kind) := (State => S_Valid, First => First, Last => Last, Value => Field_Value, Predecessor => Ctx.Cursors (F_Kind).Predecessor);
      Ctx.Cursors (Successor (Ctx, F_Kind)) := (State => S_Invalid, Predecessor => F_Kind);
   end Set_Kind;

   procedure Set_Arity (Ctx : in out Context; Val : Protocol.Binder_Arity) is
      Field_Value : constant Field_Dependent_Value := (F_Arity, Convert (Val));
      First, Last : Types.Bit_Index;
   begin
      Reset_Dependent_Fields (Ctx, F_Arity);
      Set_Field_Value (Ctx, Field_Value, First, Last);
      Ctx := (Ctx.Buffer_First, Ctx.Buffer_Last, Ctx.First, Last, Ctx.Buffer, Ctx.Cursors);
      Ctx.Cursors (F_Arity) := (State => S_Valid, First => First, Last => Last, Value => Field_Value, Predecessor => Ctx.Cursors (F_Arity).Predecessor);
      Ctx.Cursors (Successor (Ctx, F_Arity)) := (State => S_Invalid, Predecessor => F_Arity);
   end Set_Arity;

   procedure Set_Tag (Ctx : in out Context; Val : Protocol.Binder_Tag) is
      Field_Value : constant Field_Dependent_Value := (F_Tag, Val);
      First, Last : Types.Bit_Index;
   begin
      Reset_Dependent_Fields (Ctx, F_Tag);
      Set_Field_Value (Ctx, Field_Value, First, Last);
      Ctx := (Ctx.Buffer_First, Ctx.Buffer_Last, Ctx.First, Last, Ctx.Buffer, Ctx.Cursors);
      Ctx.Cursors (F_Tag) := (State => S_Valid, First => First, Last => Last, Value => Field_Value, Predecessor => Ctx.Cursors (F_Tag).Predecessor);
      Ctx.Cursors (Successor (Ctx, F_Tag)) := (State => S_Invalid, Predecessor => F_Tag);
   end Set_Tag;

   procedure Set_Flags (Ctx : in out Context; Val : Protocol.Flat_Binder_Flags) is
      Field_Value : constant Field_Dependent_Value := (F_Flags, Convert (Val));
      First, Last : Types.Bit_Index;
   begin
      Reset_Dependent_Fields (Ctx, F_Flags);
      Set_Field_Value (Ctx, Field_Value, First, Last);
      Ctx := (Ctx.Buffer_First, Ctx.Buffer_Last, Ctx.First, Last, Ctx.Buffer, Ctx.Cursors);
      Ctx.Cursors (F_Flags) := (State => S_Valid, First => First, Last => Last, Value => Field_Value, Predecessor => Ctx.Cursors (F_Flags).Predecessor);
      Ctx.Cursors (Successor (Ctx, F_Flags)) := (State => S_Invalid, Predecessor => F_Flags);
   end Set_Flags;

   procedure Set_Binder (Ctx : in out Context; Val : Protocol.Binder) is
      Field_Value : constant Field_Dependent_Value := (F_Binder, Val);
      First, Last : Types.Bit_Index;
   begin
      Reset_Dependent_Fields (Ctx, F_Binder);
      Set_Field_Value (Ctx, Field_Value, First, Last);
      Ctx := (Ctx.Buffer_First, Ctx.Buffer_Last, Ctx.First, Last, Ctx.Buffer, Ctx.Cursors);
      Ctx.Cursors (F_Binder) := (State => S_Valid, First => First, Last => Last, Value => Field_Value, Predecessor => Ctx.Cursors (F_Binder).Predecessor);
      Ctx.Cursors (Successor (Ctx, F_Binder)) := (State => S_Invalid, Predecessor => F_Binder);
   end Set_Binder;

   procedure Set_Handle (Ctx : in out Context; Val : Protocol.Handle) is
      Field_Value : constant Field_Dependent_Value := (F_Handle, Val);
      First, Last : Types.Bit_Index;
   begin
      Reset_Dependent_Fields (Ctx, F_Handle);
      Set_Field_Value (Ctx, Field_Value, First, Last);
      Ctx := (Ctx.Buffer_First, Ctx.Buffer_Last, Ctx.First, Last, Ctx.Buffer, Ctx.Cursors);
      Ctx.Cursors (F_Handle) := (State => S_Valid, First => First, Last => Last, Value => Field_Value, Predecessor => Ctx.Cursors (F_Handle).Predecessor);
      Ctx.Cursors (Successor (Ctx, F_Handle)) := (State => S_Invalid, Predecessor => F_Handle);
   end Set_Handle;

   procedure Set_Cookie (Ctx : in out Context; Val : Protocol.Cookie) is
      Field_Value : constant Field_Dependent_Value := (F_Cookie, Val);
      First, Last : Types.Bit_Index;
   begin
      Reset_Dependent_Fields (Ctx, F_Cookie);
      Set_Field_Value (Ctx, Field_Value, First, Last);
      Ctx := (Ctx.Buffer_First, Ctx.Buffer_Last, Ctx.First, Last, Ctx.Buffer, Ctx.Cursors);
      Ctx.Cursors (F_Cookie) := (State => S_Valid, First => First, Last => Last, Value => Field_Value, Predecessor => Ctx.Cursors (F_Cookie).Predecessor);
      Ctx.Cursors (Successor (Ctx, F_Cookie)) := (State => S_Invalid, Predecessor => F_Cookie);
   end Set_Cookie;

end Parpen.Protocol.Generic_IBinder;
