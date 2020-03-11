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
      and then Invalid (Ctx, F_Legacy_Flags)
      and then Invalid (Ctx, F_Has_Parent)
      and then Invalid (Ctx, F_Flags)
      and then Invalid (Ctx, F_FD)
      and then Invalid (Ctx, F_Num_FDs)
      and then Invalid (Ctx, F_Padding)
      and then Invalid (Ctx, F_Binder)
      and then Invalid (Ctx, F_Handle)
      and then Invalid (Ctx, F_Parent)
      and then Invalid (Ctx, F_Buffer)
      and then Invalid (Ctx, F_Unused_Padding)
      and then Invalid (Ctx, F_Parent_Offset)
      and then Invalid (Ctx, F_Length)
      and then Invalid (Ctx, F_Cookie)
      and then Invalid (Ctx, F_Index));

   procedure Take_Buffer (Ctx : in out Context; Buffer : out Types.Bytes_Ptr) is
   begin
      Buffer := Ctx.Buffer;
      Ctx.Buffer := null;
   end Take_Buffer;

   function Has_Buffer (Ctx : Context) return Boolean is
     (Ctx.Buffer /= null);

   function Message_Last (Ctx : Context) return Types.Bit_Index is
     ((if Structural_Valid (Ctx.Cursors (F_Length))
         and Types.Bit_Length (Ctx.Cursors (F_Has_Parent).Value.Has_Parent_Value) = Types.Bit_Length (Convert (False)) then
       Ctx.Cursors (F_Length).Last
    elsif Structural_Valid (Ctx.Cursors (F_Index)) then
       Ctx.Cursors (F_Index).Last
    elsif Structural_Valid (Ctx.Cursors (F_Parent_Offset)) then
       Ctx.Cursors (F_Parent_Offset).Last
    elsif Structural_Valid (Ctx.Cursors (F_Cookie)) then
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
                  when F_Legacy_Flags =>
                     Types.Bit_Length (Ctx.Cursors (F_Kind).Value.Kind_Value) = Types.Bit_Length (Convert (BK_FD)),
                  when F_Has_Parent =>
                     Types.Bit_Length (Ctx.Cursors (F_Kind).Value.Kind_Value) = Types.Bit_Length (Convert (BK_POINTER)),
                  when F_Flags =>
                     Types.Bit_Length (Ctx.Cursors (F_Kind).Value.Kind_Value) /= Types.Bit_Length (Convert (BK_POINTER))
                        and Types.Bit_Length (Ctx.Cursors (F_Kind).Value.Kind_Value) /= Types.Bit_Length (Convert (BK_FD)),
                  when others =>
                     False),
         when F_Legacy_Flags =>
            (case Fld is
                  when F_FD =>
                     Types.Bit_Length (Ctx.Cursors (F_Arity).Value.Arity_Value) = Types.Bit_Length (Convert (BA_SINGLE)),
                  when F_Num_FDs =>
                     Types.Bit_Length (Ctx.Cursors (F_Arity).Value.Arity_Value) = Types.Bit_Length (Convert (BA_ARRAY)),
                  when others =>
                     False),
         when F_Has_Parent =>
            (case Fld is
                  when F_Padding =>
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
         when F_FD =>
            (case Fld is
                  when F_Cookie =>
                     True,
                  when others =>
                     False),
         when F_Num_FDs =>
            (case Fld is
                  when F_Parent =>
                     True,
                  when others =>
                     False),
         when F_Padding =>
            (case Fld is
                  when F_Buffer =>
                     True,
                  when others =>
                     False),
         when F_Binder =>
            (case Fld is
                  when F_Cookie =>
                     True,
                  when others =>
                     False),
         when F_Handle =>
            (case Fld is
                  when F_Unused_Padding =>
                     True,
                  when others =>
                     False),
         when F_Parent =>
            (case Fld is
                  when F_Parent_Offset =>
                     True,
                  when others =>
                     False),
         when F_Buffer =>
            (case Fld is
                  when F_Length =>
                     True,
                  when others =>
                     False),
         when F_Unused_Padding =>
            (case Fld is
                  when F_Cookie =>
                     True,
                  when others =>
                     False),
         when F_Parent_Offset =>
            False,
         when F_Length =>
            (case Fld is
                  when F_Index =>
                     Types.Bit_Length (Ctx.Cursors (F_Has_Parent).Value.Has_Parent_Value) = Types.Bit_Length (Convert (True)),
                  when others =>
                     False),
         when F_Cookie | F_Index | F_Final =>
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
            Types.Bit_Length (Ctx.Cursors (F_Kind).Value.Kind_Value) = Types.Bit_Length (Convert (BK_FD))
               or Types.Bit_Length (Ctx.Cursors (F_Kind).Value.Kind_Value) = Types.Bit_Length (Convert (BK_POINTER))
               or (Types.Bit_Length (Ctx.Cursors (F_Kind).Value.Kind_Value) /= Types.Bit_Length (Convert (BK_POINTER))
                 and Types.Bit_Length (Ctx.Cursors (F_Kind).Value.Kind_Value) /= Types.Bit_Length (Convert (BK_FD))),
         when F_Legacy_Flags =>
            Types.Bit_Length (Ctx.Cursors (F_Arity).Value.Arity_Value) = Types.Bit_Length (Convert (BA_SINGLE))
               or Types.Bit_Length (Ctx.Cursors (F_Arity).Value.Arity_Value) = Types.Bit_Length (Convert (BA_ARRAY)),
         when F_Has_Parent =>
            True,
         when F_Flags =>
            Types.Bit_Length (Ctx.Cursors (F_Kind).Value.Kind_Value) = Types.Bit_Length (Convert (BK_STRONG_BINDER))
               or Types.Bit_Length (Ctx.Cursors (F_Kind).Value.Kind_Value) = Types.Bit_Length (Convert (BK_WEAK_BINDER))
               or Types.Bit_Length (Ctx.Cursors (F_Kind).Value.Kind_Value) = Types.Bit_Length (Convert (BK_STRONG_HANDLE))
               or Types.Bit_Length (Ctx.Cursors (F_Kind).Value.Kind_Value) = Types.Bit_Length (Convert (BK_WEAK_HANDLE)),
         when F_FD | F_Num_FDs | F_Padding | F_Binder | F_Handle | F_Parent | F_Buffer | F_Unused_Padding | F_Parent_Offset =>
            True,
         when F_Length =>
            Types.Bit_Length (Ctx.Cursors (F_Has_Parent).Value.Has_Parent_Value) = Types.Bit_Length (Convert (True))
               or Types.Bit_Length (Ctx.Cursors (F_Has_Parent).Value.Has_Parent_Value) = Types.Bit_Length (Convert (False)),
         when F_Cookie | F_Index =>
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
                  when F_Legacy_Flags =>
                     Protocol.Pad32'Size,
                  when F_Has_Parent =>
                     Builtin_Types.Boolean_Base'Size,
                  when F_Flags =>
                     Protocol.Flat_Binder_Flags_Base'Size,
                  when others =>
                     Types.Unreachable_Bit_Length),
         when F_Legacy_Flags =>
            (case Fld is
                  when F_FD =>
                     Protocol.Handle_Base'Size,
                  when F_Num_FDs =>
                     Protocol.Count'Size,
                  when others =>
                     Types.Unreachable_Bit_Length),
         when F_Has_Parent =>
            (case Fld is
                  when F_Padding =>
                     Protocol.Pad31'Size,
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
         when F_FD =>
            (case Fld is
                  when F_Cookie =>
                     Protocol.Cookie'Size,
                  when others =>
                     Types.Unreachable_Bit_Length),
         when F_Num_FDs =>
            (case Fld is
                  when F_Parent =>
                     Protocol.Index'Size,
                  when others =>
                     Types.Unreachable_Bit_Length),
         when F_Padding =>
            (case Fld is
                  when F_Buffer =>
                     Protocol.Index'Size,
                  when others =>
                     Types.Unreachable_Bit_Length),
         when F_Binder =>
            (case Fld is
                  when F_Cookie =>
                     Protocol.Cookie'Size,
                  when others =>
                     Types.Unreachable_Bit_Length),
         when F_Handle =>
            (case Fld is
                  when F_Unused_Padding =>
                     Protocol.Pad32'Size,
                  when others =>
                     Types.Unreachable_Bit_Length),
         when F_Parent =>
            (case Fld is
                  when F_Parent_Offset =>
                     Protocol.Offset_Base'Size,
                  when others =>
                     Types.Unreachable_Bit_Length),
         when F_Buffer =>
            (case Fld is
                  when F_Length =>
                     Protocol.Length_Base'Size,
                  when others =>
                     Types.Unreachable_Bit_Length),
         when F_Unused_Padding =>
            (case Fld is
                  when F_Cookie =>
                     Protocol.Cookie'Size,
                  when others =>
                     Types.Unreachable_Bit_Length),
         when F_Parent_Offset =>
            0,
         when F_Length =>
            (case Fld is
                  when F_Index =>
                     Protocol.Index'Size,
                  when others =>
                     Types.Unreachable_Bit_Length),
         when F_Cookie | F_Index | F_Final =>
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
         when F_Legacy_Flags =>
            (if Ctx.Cursors (Fld).Predecessor = F_Tag
                  and Types.Bit_Length (Ctx.Cursors (F_Kind).Value.Kind_Value) = Types.Bit_Length (Convert (BK_FD)) then
                (Ctx.Cursors (Ctx.Cursors (Fld).Predecessor).Last + 1)
             else
                Types.Unreachable_Bit_Length),
         when F_Has_Parent =>
            (if Ctx.Cursors (Fld).Predecessor = F_Tag
                  and Types.Bit_Length (Ctx.Cursors (F_Kind).Value.Kind_Value) = Types.Bit_Length (Convert (BK_POINTER)) then
                (Ctx.Cursors (Ctx.Cursors (Fld).Predecessor).Last + 1)
             else
                Types.Unreachable_Bit_Length),
         when F_Flags =>
            (if Ctx.Cursors (Fld).Predecessor = F_Tag
                  and Types.Bit_Length (Ctx.Cursors (F_Kind).Value.Kind_Value) /= Types.Bit_Length (Convert (BK_POINTER))
                  and Types.Bit_Length (Ctx.Cursors (F_Kind).Value.Kind_Value) /= Types.Bit_Length (Convert (BK_FD)) then
                (Ctx.Cursors (Ctx.Cursors (Fld).Predecessor).Last + 1)
             else
                Types.Unreachable_Bit_Length),
         when F_FD =>
            (if Ctx.Cursors (Fld).Predecessor = F_Legacy_Flags
                  and Types.Bit_Length (Ctx.Cursors (F_Arity).Value.Arity_Value) = Types.Bit_Length (Convert (BA_SINGLE)) then
                (Ctx.Cursors (Ctx.Cursors (Fld).Predecessor).Last + 1)
             else
                Types.Unreachable_Bit_Length),
         when F_Num_FDs =>
            (if Ctx.Cursors (Fld).Predecessor = F_Legacy_Flags
                  and Types.Bit_Length (Ctx.Cursors (F_Arity).Value.Arity_Value) = Types.Bit_Length (Convert (BA_ARRAY)) then
                (Ctx.Cursors (Ctx.Cursors (Fld).Predecessor).Last + 1)
             else
                Types.Unreachable_Bit_Length),
         when F_Padding =>
            (if Ctx.Cursors (Fld).Predecessor = F_Has_Parent then
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
         when F_Parent =>
            (if Ctx.Cursors (Fld).Predecessor = F_Num_FDs then
                (Ctx.Cursors (Ctx.Cursors (Fld).Predecessor).Last + 1)
             else
                Types.Unreachable_Bit_Length),
         when F_Buffer =>
            (if Ctx.Cursors (Fld).Predecessor = F_Padding then
                (Ctx.Cursors (Ctx.Cursors (Fld).Predecessor).Last + 1)
             else
                Types.Unreachable_Bit_Length),
         when F_Unused_Padding =>
            (if Ctx.Cursors (Fld).Predecessor = F_Handle then
                (Ctx.Cursors (Ctx.Cursors (Fld).Predecessor).Last + 1)
             else
                Types.Unreachable_Bit_Length),
         when F_Parent_Offset =>
            (if Ctx.Cursors (Fld).Predecessor = F_Parent then
                (Ctx.Cursors (Ctx.Cursors (Fld).Predecessor).Last + 1)
             else
                Types.Unreachable_Bit_Length),
         when F_Length =>
            (if Ctx.Cursors (Fld).Predecessor = F_Buffer then
                (Ctx.Cursors (Ctx.Cursors (Fld).Predecessor).Last + 1)
             else
                Types.Unreachable_Bit_Length),
         when F_Cookie =>
            (if Ctx.Cursors (Fld).Predecessor = F_FD then
                (Ctx.Cursors (Ctx.Cursors (Fld).Predecessor).Last + 1)
             elsif Ctx.Cursors (Fld).Predecessor = F_Binder then
                (Ctx.Cursors (Ctx.Cursors (Fld).Predecessor).Last + 1)
             elsif Ctx.Cursors (Fld).Predecessor = F_Unused_Padding then
                (Ctx.Cursors (Ctx.Cursors (Fld).Predecessor).Last + 1)
             else
                Types.Unreachable_Bit_Length),
         when F_Index =>
            (if Ctx.Cursors (Fld).Predecessor = F_Length
                  and Types.Bit_Length (Ctx.Cursors (F_Has_Parent).Value.Has_Parent_Value) = Types.Bit_Length (Convert (True)) then
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
            (if Types.Bit_Length (Ctx.Cursors (F_Kind).Value.Kind_Value) = Types.Bit_Length (Convert (BK_FD)) then
                F_Legacy_Flags
             elsif Types.Bit_Length (Ctx.Cursors (F_Kind).Value.Kind_Value) = Types.Bit_Length (Convert (BK_POINTER)) then
                F_Has_Parent
             elsif Types.Bit_Length (Ctx.Cursors (F_Kind).Value.Kind_Value) /= Types.Bit_Length (Convert (BK_POINTER))
                     and Types.Bit_Length (Ctx.Cursors (F_Kind).Value.Kind_Value) /= Types.Bit_Length (Convert (BK_FD)) then
                F_Flags
             else
                F_Initial),
         when F_Legacy_Flags =>
            (if Types.Bit_Length (Ctx.Cursors (F_Arity).Value.Arity_Value) = Types.Bit_Length (Convert (BA_SINGLE)) then
                F_FD
             elsif Types.Bit_Length (Ctx.Cursors (F_Arity).Value.Arity_Value) = Types.Bit_Length (Convert (BA_ARRAY)) then
                F_Num_FDs
             else
                F_Initial),
         when F_Has_Parent =>
            F_Padding,
         when F_Flags =>
            (if Types.Bit_Length (Ctx.Cursors (F_Kind).Value.Kind_Value) = Types.Bit_Length (Convert (BK_STRONG_BINDER))
                  or Types.Bit_Length (Ctx.Cursors (F_Kind).Value.Kind_Value) = Types.Bit_Length (Convert (BK_WEAK_BINDER)) then
                F_Binder
             elsif Types.Bit_Length (Ctx.Cursors (F_Kind).Value.Kind_Value) = Types.Bit_Length (Convert (BK_STRONG_HANDLE))
                     or Types.Bit_Length (Ctx.Cursors (F_Kind).Value.Kind_Value) = Types.Bit_Length (Convert (BK_WEAK_HANDLE)) then
                F_Handle
             else
                F_Initial),
         when F_FD =>
            F_Cookie,
         when F_Num_FDs =>
            F_Parent,
         when F_Padding =>
            F_Buffer,
         when F_Binder =>
            F_Cookie,
         when F_Handle =>
            F_Unused_Padding,
         when F_Parent =>
            F_Parent_Offset,
         when F_Buffer =>
            F_Length,
         when F_Unused_Padding =>
            F_Cookie,
         when F_Parent_Offset =>
            F_Final,
         when F_Length =>
            (if Types.Bit_Length (Ctx.Cursors (F_Has_Parent).Value.Has_Parent_Value) = Types.Bit_Length (Convert (True)) then
                F_Index
             elsif Types.Bit_Length (Ctx.Cursors (F_Has_Parent).Value.Has_Parent_Value) = Types.Bit_Length (Convert (False)) then
                F_Final
             else
                F_Initial),
         when F_Cookie | F_Index =>
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
         when F_Legacy_Flags | F_Has_Parent | F_Flags =>
            (Valid (Ctx.Cursors (F_Tag))
                 and Ctx.Cursors (Fld).Predecessor = F_Tag),
         when F_FD | F_Num_FDs =>
            (Valid (Ctx.Cursors (F_Legacy_Flags))
                 and Ctx.Cursors (Fld).Predecessor = F_Legacy_Flags),
         when F_Padding =>
            (Valid (Ctx.Cursors (F_Has_Parent))
                 and Ctx.Cursors (Fld).Predecessor = F_Has_Parent),
         when F_Binder | F_Handle =>
            (Valid (Ctx.Cursors (F_Flags))
                 and Ctx.Cursors (Fld).Predecessor = F_Flags),
         when F_Parent =>
            (Valid (Ctx.Cursors (F_Num_FDs))
                 and Ctx.Cursors (Fld).Predecessor = F_Num_FDs),
         when F_Buffer =>
            (Valid (Ctx.Cursors (F_Padding))
                 and Ctx.Cursors (Fld).Predecessor = F_Padding),
         when F_Unused_Padding =>
            (Valid (Ctx.Cursors (F_Handle))
                 and Ctx.Cursors (Fld).Predecessor = F_Handle),
         when F_Parent_Offset =>
            (Valid (Ctx.Cursors (F_Parent))
                 and Ctx.Cursors (Fld).Predecessor = F_Parent),
         when F_Length =>
            (Valid (Ctx.Cursors (F_Buffer))
                 and Ctx.Cursors (Fld).Predecessor = F_Buffer),
         when F_Cookie =>
            (Valid (Ctx.Cursors (F_FD))
                 and Ctx.Cursors (Fld).Predecessor = F_FD)
               or (Valid (Ctx.Cursors (F_Binder))
                 and Ctx.Cursors (Fld).Predecessor = F_Binder)
               or (Valid (Ctx.Cursors (F_Unused_Padding))
                 and Ctx.Cursors (Fld).Predecessor = F_Unused_Padding),
         when F_Index =>
            (Valid (Ctx.Cursors (F_Length))
                 and Ctx.Cursors (Fld).Predecessor = F_Length),
         when F_Final =>
            (Valid (Ctx.Cursors (F_Length))
                 and Ctx.Cursors (Fld).Predecessor = F_Length)
               or (Valid (Ctx.Cursors (F_Index))
                 and Ctx.Cursors (Fld).Predecessor = F_Index)
               or (Valid (Ctx.Cursors (F_Parent_Offset))
                 and Ctx.Cursors (Fld).Predecessor = F_Parent_Offset)
               or (Valid (Ctx.Cursors (F_Cookie))
                 and Ctx.Cursors (Fld).Predecessor = F_Cookie)));

   function Invalid_Successor (Ctx : Context; Fld : Field) return Boolean is
     ((case Fld is
         when F_Kind =>
            Invalid (Ctx.Cursors (F_Arity)),
         when F_Arity =>
            Invalid (Ctx.Cursors (F_Tag)),
         when F_Tag =>
            Invalid (Ctx.Cursors (F_Legacy_Flags))
               and Invalid (Ctx.Cursors (F_Has_Parent))
               and Invalid (Ctx.Cursors (F_Flags)),
         when F_Legacy_Flags =>
            Invalid (Ctx.Cursors (F_FD))
               and Invalid (Ctx.Cursors (F_Num_FDs)),
         when F_Has_Parent =>
            Invalid (Ctx.Cursors (F_Padding)),
         when F_Flags =>
            Invalid (Ctx.Cursors (F_Binder))
               and Invalid (Ctx.Cursors (F_Handle)),
         when F_FD =>
            Invalid (Ctx.Cursors (F_Cookie)),
         when F_Num_FDs =>
            Invalid (Ctx.Cursors (F_Parent)),
         when F_Padding =>
            Invalid (Ctx.Cursors (F_Buffer)),
         when F_Binder =>
            Invalid (Ctx.Cursors (F_Cookie)),
         when F_Handle =>
            Invalid (Ctx.Cursors (F_Unused_Padding)),
         when F_Parent =>
            Invalid (Ctx.Cursors (F_Parent_Offset)),
         when F_Buffer =>
            Invalid (Ctx.Cursors (F_Length)),
         when F_Unused_Padding =>
            Invalid (Ctx.Cursors (F_Cookie)),
         when F_Parent_Offset =>
            True,
         when F_Length =>
            Invalid (Ctx.Cursors (F_Index)),
         when F_Cookie | F_Index =>
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
                     and Invalid (Ctx, F_Legacy_Flags)
                     and Invalid (Ctx, F_Has_Parent)
                     and Invalid (Ctx, F_Flags)
                     and Invalid (Ctx, F_FD)
                     and Invalid (Ctx, F_Num_FDs)
                     and Invalid (Ctx, F_Padding)
                     and Invalid (Ctx, F_Binder)
                     and Invalid (Ctx, F_Handle)
                     and Invalid (Ctx, F_Parent)
                     and Invalid (Ctx, F_Buffer)
                     and Invalid (Ctx, F_Unused_Padding)
                     and Invalid (Ctx, F_Parent_Offset)
                     and Invalid (Ctx, F_Length)
                     and Invalid (Ctx, F_Cookie)
                     and Invalid (Ctx, F_Index),
               when F_Arity =>
                  Ctx.Cursors (F_Kind) = Ctx.Cursors (F_Kind)'Old
                     and Invalid (Ctx, F_Arity)
                     and Invalid (Ctx, F_Tag)
                     and Invalid (Ctx, F_Legacy_Flags)
                     and Invalid (Ctx, F_Has_Parent)
                     and Invalid (Ctx, F_Flags)
                     and Invalid (Ctx, F_FD)
                     and Invalid (Ctx, F_Num_FDs)
                     and Invalid (Ctx, F_Padding)
                     and Invalid (Ctx, F_Binder)
                     and Invalid (Ctx, F_Handle)
                     and Invalid (Ctx, F_Parent)
                     and Invalid (Ctx, F_Buffer)
                     and Invalid (Ctx, F_Unused_Padding)
                     and Invalid (Ctx, F_Parent_Offset)
                     and Invalid (Ctx, F_Length)
                     and Invalid (Ctx, F_Cookie)
                     and Invalid (Ctx, F_Index),
               when F_Tag =>
                  Ctx.Cursors (F_Kind) = Ctx.Cursors (F_Kind)'Old
                     and Ctx.Cursors (F_Arity) = Ctx.Cursors (F_Arity)'Old
                     and Invalid (Ctx, F_Tag)
                     and Invalid (Ctx, F_Legacy_Flags)
                     and Invalid (Ctx, F_Has_Parent)
                     and Invalid (Ctx, F_Flags)
                     and Invalid (Ctx, F_FD)
                     and Invalid (Ctx, F_Num_FDs)
                     and Invalid (Ctx, F_Padding)
                     and Invalid (Ctx, F_Binder)
                     and Invalid (Ctx, F_Handle)
                     and Invalid (Ctx, F_Parent)
                     and Invalid (Ctx, F_Buffer)
                     and Invalid (Ctx, F_Unused_Padding)
                     and Invalid (Ctx, F_Parent_Offset)
                     and Invalid (Ctx, F_Length)
                     and Invalid (Ctx, F_Cookie)
                     and Invalid (Ctx, F_Index),
               when F_Legacy_Flags =>
                  Ctx.Cursors (F_Kind) = Ctx.Cursors (F_Kind)'Old
                     and Ctx.Cursors (F_Arity) = Ctx.Cursors (F_Arity)'Old
                     and Ctx.Cursors (F_Tag) = Ctx.Cursors (F_Tag)'Old
                     and Invalid (Ctx, F_Legacy_Flags)
                     and Invalid (Ctx, F_Has_Parent)
                     and Invalid (Ctx, F_Flags)
                     and Invalid (Ctx, F_FD)
                     and Invalid (Ctx, F_Num_FDs)
                     and Invalid (Ctx, F_Padding)
                     and Invalid (Ctx, F_Binder)
                     and Invalid (Ctx, F_Handle)
                     and Invalid (Ctx, F_Parent)
                     and Invalid (Ctx, F_Buffer)
                     and Invalid (Ctx, F_Unused_Padding)
                     and Invalid (Ctx, F_Parent_Offset)
                     and Invalid (Ctx, F_Length)
                     and Invalid (Ctx, F_Cookie)
                     and Invalid (Ctx, F_Index),
               when F_Has_Parent =>
                  Ctx.Cursors (F_Kind) = Ctx.Cursors (F_Kind)'Old
                     and Ctx.Cursors (F_Arity) = Ctx.Cursors (F_Arity)'Old
                     and Ctx.Cursors (F_Tag) = Ctx.Cursors (F_Tag)'Old
                     and Ctx.Cursors (F_Legacy_Flags) = Ctx.Cursors (F_Legacy_Flags)'Old
                     and Invalid (Ctx, F_Has_Parent)
                     and Invalid (Ctx, F_Flags)
                     and Invalid (Ctx, F_FD)
                     and Invalid (Ctx, F_Num_FDs)
                     and Invalid (Ctx, F_Padding)
                     and Invalid (Ctx, F_Binder)
                     and Invalid (Ctx, F_Handle)
                     and Invalid (Ctx, F_Parent)
                     and Invalid (Ctx, F_Buffer)
                     and Invalid (Ctx, F_Unused_Padding)
                     and Invalid (Ctx, F_Parent_Offset)
                     and Invalid (Ctx, F_Length)
                     and Invalid (Ctx, F_Cookie)
                     and Invalid (Ctx, F_Index),
               when F_Flags =>
                  Ctx.Cursors (F_Kind) = Ctx.Cursors (F_Kind)'Old
                     and Ctx.Cursors (F_Arity) = Ctx.Cursors (F_Arity)'Old
                     and Ctx.Cursors (F_Tag) = Ctx.Cursors (F_Tag)'Old
                     and Ctx.Cursors (F_Legacy_Flags) = Ctx.Cursors (F_Legacy_Flags)'Old
                     and Ctx.Cursors (F_Has_Parent) = Ctx.Cursors (F_Has_Parent)'Old
                     and Invalid (Ctx, F_Flags)
                     and Invalid (Ctx, F_FD)
                     and Invalid (Ctx, F_Num_FDs)
                     and Invalid (Ctx, F_Padding)
                     and Invalid (Ctx, F_Binder)
                     and Invalid (Ctx, F_Handle)
                     and Invalid (Ctx, F_Parent)
                     and Invalid (Ctx, F_Buffer)
                     and Invalid (Ctx, F_Unused_Padding)
                     and Invalid (Ctx, F_Parent_Offset)
                     and Invalid (Ctx, F_Length)
                     and Invalid (Ctx, F_Cookie)
                     and Invalid (Ctx, F_Index),
               when F_FD =>
                  Ctx.Cursors (F_Kind) = Ctx.Cursors (F_Kind)'Old
                     and Ctx.Cursors (F_Arity) = Ctx.Cursors (F_Arity)'Old
                     and Ctx.Cursors (F_Tag) = Ctx.Cursors (F_Tag)'Old
                     and Ctx.Cursors (F_Legacy_Flags) = Ctx.Cursors (F_Legacy_Flags)'Old
                     and Ctx.Cursors (F_Has_Parent) = Ctx.Cursors (F_Has_Parent)'Old
                     and Ctx.Cursors (F_Flags) = Ctx.Cursors (F_Flags)'Old
                     and Invalid (Ctx, F_FD)
                     and Invalid (Ctx, F_Num_FDs)
                     and Invalid (Ctx, F_Padding)
                     and Invalid (Ctx, F_Binder)
                     and Invalid (Ctx, F_Handle)
                     and Invalid (Ctx, F_Parent)
                     and Invalid (Ctx, F_Buffer)
                     and Invalid (Ctx, F_Unused_Padding)
                     and Invalid (Ctx, F_Parent_Offset)
                     and Invalid (Ctx, F_Length)
                     and Invalid (Ctx, F_Cookie)
                     and Invalid (Ctx, F_Index),
               when F_Num_FDs =>
                  Ctx.Cursors (F_Kind) = Ctx.Cursors (F_Kind)'Old
                     and Ctx.Cursors (F_Arity) = Ctx.Cursors (F_Arity)'Old
                     and Ctx.Cursors (F_Tag) = Ctx.Cursors (F_Tag)'Old
                     and Ctx.Cursors (F_Legacy_Flags) = Ctx.Cursors (F_Legacy_Flags)'Old
                     and Ctx.Cursors (F_Has_Parent) = Ctx.Cursors (F_Has_Parent)'Old
                     and Ctx.Cursors (F_Flags) = Ctx.Cursors (F_Flags)'Old
                     and Ctx.Cursors (F_FD) = Ctx.Cursors (F_FD)'Old
                     and Invalid (Ctx, F_Num_FDs)
                     and Invalid (Ctx, F_Padding)
                     and Invalid (Ctx, F_Binder)
                     and Invalid (Ctx, F_Handle)
                     and Invalid (Ctx, F_Parent)
                     and Invalid (Ctx, F_Buffer)
                     and Invalid (Ctx, F_Unused_Padding)
                     and Invalid (Ctx, F_Parent_Offset)
                     and Invalid (Ctx, F_Length)
                     and Invalid (Ctx, F_Cookie)
                     and Invalid (Ctx, F_Index),
               when F_Padding =>
                  Ctx.Cursors (F_Kind) = Ctx.Cursors (F_Kind)'Old
                     and Ctx.Cursors (F_Arity) = Ctx.Cursors (F_Arity)'Old
                     and Ctx.Cursors (F_Tag) = Ctx.Cursors (F_Tag)'Old
                     and Ctx.Cursors (F_Legacy_Flags) = Ctx.Cursors (F_Legacy_Flags)'Old
                     and Ctx.Cursors (F_Has_Parent) = Ctx.Cursors (F_Has_Parent)'Old
                     and Ctx.Cursors (F_Flags) = Ctx.Cursors (F_Flags)'Old
                     and Ctx.Cursors (F_FD) = Ctx.Cursors (F_FD)'Old
                     and Ctx.Cursors (F_Num_FDs) = Ctx.Cursors (F_Num_FDs)'Old
                     and Invalid (Ctx, F_Padding)
                     and Invalid (Ctx, F_Binder)
                     and Invalid (Ctx, F_Handle)
                     and Invalid (Ctx, F_Parent)
                     and Invalid (Ctx, F_Buffer)
                     and Invalid (Ctx, F_Unused_Padding)
                     and Invalid (Ctx, F_Parent_Offset)
                     and Invalid (Ctx, F_Length)
                     and Invalid (Ctx, F_Cookie)
                     and Invalid (Ctx, F_Index),
               when F_Binder =>
                  Ctx.Cursors (F_Kind) = Ctx.Cursors (F_Kind)'Old
                     and Ctx.Cursors (F_Arity) = Ctx.Cursors (F_Arity)'Old
                     and Ctx.Cursors (F_Tag) = Ctx.Cursors (F_Tag)'Old
                     and Ctx.Cursors (F_Legacy_Flags) = Ctx.Cursors (F_Legacy_Flags)'Old
                     and Ctx.Cursors (F_Has_Parent) = Ctx.Cursors (F_Has_Parent)'Old
                     and Ctx.Cursors (F_Flags) = Ctx.Cursors (F_Flags)'Old
                     and Ctx.Cursors (F_FD) = Ctx.Cursors (F_FD)'Old
                     and Ctx.Cursors (F_Num_FDs) = Ctx.Cursors (F_Num_FDs)'Old
                     and Ctx.Cursors (F_Padding) = Ctx.Cursors (F_Padding)'Old
                     and Invalid (Ctx, F_Binder)
                     and Invalid (Ctx, F_Handle)
                     and Invalid (Ctx, F_Parent)
                     and Invalid (Ctx, F_Buffer)
                     and Invalid (Ctx, F_Unused_Padding)
                     and Invalid (Ctx, F_Parent_Offset)
                     and Invalid (Ctx, F_Length)
                     and Invalid (Ctx, F_Cookie)
                     and Invalid (Ctx, F_Index),
               when F_Handle =>
                  Ctx.Cursors (F_Kind) = Ctx.Cursors (F_Kind)'Old
                     and Ctx.Cursors (F_Arity) = Ctx.Cursors (F_Arity)'Old
                     and Ctx.Cursors (F_Tag) = Ctx.Cursors (F_Tag)'Old
                     and Ctx.Cursors (F_Legacy_Flags) = Ctx.Cursors (F_Legacy_Flags)'Old
                     and Ctx.Cursors (F_Has_Parent) = Ctx.Cursors (F_Has_Parent)'Old
                     and Ctx.Cursors (F_Flags) = Ctx.Cursors (F_Flags)'Old
                     and Ctx.Cursors (F_FD) = Ctx.Cursors (F_FD)'Old
                     and Ctx.Cursors (F_Num_FDs) = Ctx.Cursors (F_Num_FDs)'Old
                     and Ctx.Cursors (F_Padding) = Ctx.Cursors (F_Padding)'Old
                     and Ctx.Cursors (F_Binder) = Ctx.Cursors (F_Binder)'Old
                     and Invalid (Ctx, F_Handle)
                     and Invalid (Ctx, F_Parent)
                     and Invalid (Ctx, F_Buffer)
                     and Invalid (Ctx, F_Unused_Padding)
                     and Invalid (Ctx, F_Parent_Offset)
                     and Invalid (Ctx, F_Length)
                     and Invalid (Ctx, F_Cookie)
                     and Invalid (Ctx, F_Index),
               when F_Parent =>
                  Ctx.Cursors (F_Kind) = Ctx.Cursors (F_Kind)'Old
                     and Ctx.Cursors (F_Arity) = Ctx.Cursors (F_Arity)'Old
                     and Ctx.Cursors (F_Tag) = Ctx.Cursors (F_Tag)'Old
                     and Ctx.Cursors (F_Legacy_Flags) = Ctx.Cursors (F_Legacy_Flags)'Old
                     and Ctx.Cursors (F_Has_Parent) = Ctx.Cursors (F_Has_Parent)'Old
                     and Ctx.Cursors (F_Flags) = Ctx.Cursors (F_Flags)'Old
                     and Ctx.Cursors (F_FD) = Ctx.Cursors (F_FD)'Old
                     and Ctx.Cursors (F_Num_FDs) = Ctx.Cursors (F_Num_FDs)'Old
                     and Ctx.Cursors (F_Padding) = Ctx.Cursors (F_Padding)'Old
                     and Ctx.Cursors (F_Binder) = Ctx.Cursors (F_Binder)'Old
                     and Ctx.Cursors (F_Handle) = Ctx.Cursors (F_Handle)'Old
                     and Invalid (Ctx, F_Parent)
                     and Invalid (Ctx, F_Buffer)
                     and Invalid (Ctx, F_Unused_Padding)
                     and Invalid (Ctx, F_Parent_Offset)
                     and Invalid (Ctx, F_Length)
                     and Invalid (Ctx, F_Cookie)
                     and Invalid (Ctx, F_Index),
               when F_Buffer =>
                  Ctx.Cursors (F_Kind) = Ctx.Cursors (F_Kind)'Old
                     and Ctx.Cursors (F_Arity) = Ctx.Cursors (F_Arity)'Old
                     and Ctx.Cursors (F_Tag) = Ctx.Cursors (F_Tag)'Old
                     and Ctx.Cursors (F_Legacy_Flags) = Ctx.Cursors (F_Legacy_Flags)'Old
                     and Ctx.Cursors (F_Has_Parent) = Ctx.Cursors (F_Has_Parent)'Old
                     and Ctx.Cursors (F_Flags) = Ctx.Cursors (F_Flags)'Old
                     and Ctx.Cursors (F_FD) = Ctx.Cursors (F_FD)'Old
                     and Ctx.Cursors (F_Num_FDs) = Ctx.Cursors (F_Num_FDs)'Old
                     and Ctx.Cursors (F_Padding) = Ctx.Cursors (F_Padding)'Old
                     and Ctx.Cursors (F_Binder) = Ctx.Cursors (F_Binder)'Old
                     and Ctx.Cursors (F_Handle) = Ctx.Cursors (F_Handle)'Old
                     and Ctx.Cursors (F_Parent) = Ctx.Cursors (F_Parent)'Old
                     and Invalid (Ctx, F_Buffer)
                     and Invalid (Ctx, F_Unused_Padding)
                     and Invalid (Ctx, F_Parent_Offset)
                     and Invalid (Ctx, F_Length)
                     and Invalid (Ctx, F_Cookie)
                     and Invalid (Ctx, F_Index),
               when F_Unused_Padding =>
                  Ctx.Cursors (F_Kind) = Ctx.Cursors (F_Kind)'Old
                     and Ctx.Cursors (F_Arity) = Ctx.Cursors (F_Arity)'Old
                     and Ctx.Cursors (F_Tag) = Ctx.Cursors (F_Tag)'Old
                     and Ctx.Cursors (F_Legacy_Flags) = Ctx.Cursors (F_Legacy_Flags)'Old
                     and Ctx.Cursors (F_Has_Parent) = Ctx.Cursors (F_Has_Parent)'Old
                     and Ctx.Cursors (F_Flags) = Ctx.Cursors (F_Flags)'Old
                     and Ctx.Cursors (F_FD) = Ctx.Cursors (F_FD)'Old
                     and Ctx.Cursors (F_Num_FDs) = Ctx.Cursors (F_Num_FDs)'Old
                     and Ctx.Cursors (F_Padding) = Ctx.Cursors (F_Padding)'Old
                     and Ctx.Cursors (F_Binder) = Ctx.Cursors (F_Binder)'Old
                     and Ctx.Cursors (F_Handle) = Ctx.Cursors (F_Handle)'Old
                     and Ctx.Cursors (F_Parent) = Ctx.Cursors (F_Parent)'Old
                     and Ctx.Cursors (F_Buffer) = Ctx.Cursors (F_Buffer)'Old
                     and Invalid (Ctx, F_Unused_Padding)
                     and Invalid (Ctx, F_Parent_Offset)
                     and Invalid (Ctx, F_Length)
                     and Invalid (Ctx, F_Cookie)
                     and Invalid (Ctx, F_Index),
               when F_Parent_Offset =>
                  Ctx.Cursors (F_Kind) = Ctx.Cursors (F_Kind)'Old
                     and Ctx.Cursors (F_Arity) = Ctx.Cursors (F_Arity)'Old
                     and Ctx.Cursors (F_Tag) = Ctx.Cursors (F_Tag)'Old
                     and Ctx.Cursors (F_Legacy_Flags) = Ctx.Cursors (F_Legacy_Flags)'Old
                     and Ctx.Cursors (F_Has_Parent) = Ctx.Cursors (F_Has_Parent)'Old
                     and Ctx.Cursors (F_Flags) = Ctx.Cursors (F_Flags)'Old
                     and Ctx.Cursors (F_FD) = Ctx.Cursors (F_FD)'Old
                     and Ctx.Cursors (F_Num_FDs) = Ctx.Cursors (F_Num_FDs)'Old
                     and Ctx.Cursors (F_Padding) = Ctx.Cursors (F_Padding)'Old
                     and Ctx.Cursors (F_Binder) = Ctx.Cursors (F_Binder)'Old
                     and Ctx.Cursors (F_Handle) = Ctx.Cursors (F_Handle)'Old
                     and Ctx.Cursors (F_Parent) = Ctx.Cursors (F_Parent)'Old
                     and Ctx.Cursors (F_Buffer) = Ctx.Cursors (F_Buffer)'Old
                     and Ctx.Cursors (F_Unused_Padding) = Ctx.Cursors (F_Unused_Padding)'Old
                     and Invalid (Ctx, F_Parent_Offset)
                     and Invalid (Ctx, F_Length)
                     and Invalid (Ctx, F_Cookie)
                     and Invalid (Ctx, F_Index),
               when F_Length =>
                  Ctx.Cursors (F_Kind) = Ctx.Cursors (F_Kind)'Old
                     and Ctx.Cursors (F_Arity) = Ctx.Cursors (F_Arity)'Old
                     and Ctx.Cursors (F_Tag) = Ctx.Cursors (F_Tag)'Old
                     and Ctx.Cursors (F_Legacy_Flags) = Ctx.Cursors (F_Legacy_Flags)'Old
                     and Ctx.Cursors (F_Has_Parent) = Ctx.Cursors (F_Has_Parent)'Old
                     and Ctx.Cursors (F_Flags) = Ctx.Cursors (F_Flags)'Old
                     and Ctx.Cursors (F_FD) = Ctx.Cursors (F_FD)'Old
                     and Ctx.Cursors (F_Num_FDs) = Ctx.Cursors (F_Num_FDs)'Old
                     and Ctx.Cursors (F_Padding) = Ctx.Cursors (F_Padding)'Old
                     and Ctx.Cursors (F_Binder) = Ctx.Cursors (F_Binder)'Old
                     and Ctx.Cursors (F_Handle) = Ctx.Cursors (F_Handle)'Old
                     and Ctx.Cursors (F_Parent) = Ctx.Cursors (F_Parent)'Old
                     and Ctx.Cursors (F_Buffer) = Ctx.Cursors (F_Buffer)'Old
                     and Ctx.Cursors (F_Unused_Padding) = Ctx.Cursors (F_Unused_Padding)'Old
                     and Ctx.Cursors (F_Parent_Offset) = Ctx.Cursors (F_Parent_Offset)'Old
                     and Invalid (Ctx, F_Length)
                     and Invalid (Ctx, F_Cookie)
                     and Invalid (Ctx, F_Index),
               when F_Cookie =>
                  Ctx.Cursors (F_Kind) = Ctx.Cursors (F_Kind)'Old
                     and Ctx.Cursors (F_Arity) = Ctx.Cursors (F_Arity)'Old
                     and Ctx.Cursors (F_Tag) = Ctx.Cursors (F_Tag)'Old
                     and Ctx.Cursors (F_Legacy_Flags) = Ctx.Cursors (F_Legacy_Flags)'Old
                     and Ctx.Cursors (F_Has_Parent) = Ctx.Cursors (F_Has_Parent)'Old
                     and Ctx.Cursors (F_Flags) = Ctx.Cursors (F_Flags)'Old
                     and Ctx.Cursors (F_FD) = Ctx.Cursors (F_FD)'Old
                     and Ctx.Cursors (F_Num_FDs) = Ctx.Cursors (F_Num_FDs)'Old
                     and Ctx.Cursors (F_Padding) = Ctx.Cursors (F_Padding)'Old
                     and Ctx.Cursors (F_Binder) = Ctx.Cursors (F_Binder)'Old
                     and Ctx.Cursors (F_Handle) = Ctx.Cursors (F_Handle)'Old
                     and Ctx.Cursors (F_Parent) = Ctx.Cursors (F_Parent)'Old
                     and Ctx.Cursors (F_Buffer) = Ctx.Cursors (F_Buffer)'Old
                     and Ctx.Cursors (F_Unused_Padding) = Ctx.Cursors (F_Unused_Padding)'Old
                     and Ctx.Cursors (F_Parent_Offset) = Ctx.Cursors (F_Parent_Offset)'Old
                     and Ctx.Cursors (F_Length) = Ctx.Cursors (F_Length)'Old
                     and Invalid (Ctx, F_Cookie)
                     and Invalid (Ctx, F_Index),
               when F_Index =>
                  Ctx.Cursors (F_Kind) = Ctx.Cursors (F_Kind)'Old
                     and Ctx.Cursors (F_Arity) = Ctx.Cursors (F_Arity)'Old
                     and Ctx.Cursors (F_Tag) = Ctx.Cursors (F_Tag)'Old
                     and Ctx.Cursors (F_Legacy_Flags) = Ctx.Cursors (F_Legacy_Flags)'Old
                     and Ctx.Cursors (F_Has_Parent) = Ctx.Cursors (F_Has_Parent)'Old
                     and Ctx.Cursors (F_Flags) = Ctx.Cursors (F_Flags)'Old
                     and Ctx.Cursors (F_FD) = Ctx.Cursors (F_FD)'Old
                     and Ctx.Cursors (F_Num_FDs) = Ctx.Cursors (F_Num_FDs)'Old
                     and Ctx.Cursors (F_Padding) = Ctx.Cursors (F_Padding)'Old
                     and Ctx.Cursors (F_Binder) = Ctx.Cursors (F_Binder)'Old
                     and Ctx.Cursors (F_Handle) = Ctx.Cursors (F_Handle)'Old
                     and Ctx.Cursors (F_Parent) = Ctx.Cursors (F_Parent)'Old
                     and Ctx.Cursors (F_Buffer) = Ctx.Cursors (F_Buffer)'Old
                     and Ctx.Cursors (F_Unused_Padding) = Ctx.Cursors (F_Unused_Padding)'Old
                     and Ctx.Cursors (F_Parent_Offset) = Ctx.Cursors (F_Parent_Offset)'Old
                     and Ctx.Cursors (F_Length) = Ctx.Cursors (F_Length)'Old
                     and Ctx.Cursors (F_Cookie) = Ctx.Cursors (F_Cookie)'Old
                     and Invalid (Ctx, F_Index))
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
            Ctx.Cursors (F_Index) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Cookie) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Length) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Parent_Offset) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Unused_Padding) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Buffer) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Parent) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Handle) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Binder) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Padding) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Num_FDs) := (S_Invalid, F_Final);
            Ctx.Cursors (F_FD) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Flags) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Has_Parent) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Legacy_Flags) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Tag) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Arity) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Kind) := (S_Invalid, Ctx.Cursors (F_Kind).Predecessor);
            pragma Assert (Field_First (Ctx, Fld) = First
               and Field_Length (Ctx, Fld) = Length);
         when F_Arity =>
            Ctx.Cursors (F_Index) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Cookie) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Length) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Parent_Offset) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Unused_Padding) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Buffer) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Parent) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Handle) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Binder) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Padding) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Num_FDs) := (S_Invalid, F_Final);
            Ctx.Cursors (F_FD) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Flags) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Has_Parent) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Legacy_Flags) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Tag) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Arity) := (S_Invalid, Ctx.Cursors (F_Arity).Predecessor);
            pragma Assert (Field_First (Ctx, Fld) = First
               and Field_Length (Ctx, Fld) = Length);
         when F_Tag =>
            Ctx.Cursors (F_Index) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Cookie) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Length) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Parent_Offset) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Unused_Padding) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Buffer) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Parent) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Handle) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Binder) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Padding) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Num_FDs) := (S_Invalid, F_Final);
            Ctx.Cursors (F_FD) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Flags) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Has_Parent) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Legacy_Flags) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Tag) := (S_Invalid, Ctx.Cursors (F_Tag).Predecessor);
            pragma Assert (Field_First (Ctx, Fld) = First
               and Field_Length (Ctx, Fld) = Length);
         when F_Legacy_Flags =>
            Ctx.Cursors (F_Index) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Cookie) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Length) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Parent_Offset) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Unused_Padding) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Buffer) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Parent) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Handle) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Binder) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Padding) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Num_FDs) := (S_Invalid, F_Final);
            Ctx.Cursors (F_FD) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Flags) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Has_Parent) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Legacy_Flags) := (S_Invalid, Ctx.Cursors (F_Legacy_Flags).Predecessor);
            pragma Assert (Field_First (Ctx, Fld) = First
               and Field_Length (Ctx, Fld) = Length);
         when F_Has_Parent =>
            Ctx.Cursors (F_Index) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Cookie) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Length) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Parent_Offset) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Unused_Padding) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Buffer) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Parent) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Handle) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Binder) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Padding) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Num_FDs) := (S_Invalid, F_Final);
            Ctx.Cursors (F_FD) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Flags) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Has_Parent) := (S_Invalid, Ctx.Cursors (F_Has_Parent).Predecessor);
            pragma Assert (Field_First (Ctx, Fld) = First
               and Field_Length (Ctx, Fld) = Length);
         when F_Flags =>
            Ctx.Cursors (F_Index) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Cookie) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Length) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Parent_Offset) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Unused_Padding) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Buffer) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Parent) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Handle) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Binder) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Padding) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Num_FDs) := (S_Invalid, F_Final);
            Ctx.Cursors (F_FD) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Flags) := (S_Invalid, Ctx.Cursors (F_Flags).Predecessor);
            pragma Assert (Field_First (Ctx, Fld) = First
               and Field_Length (Ctx, Fld) = Length);
         when F_FD =>
            Ctx.Cursors (F_Index) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Cookie) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Length) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Parent_Offset) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Unused_Padding) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Buffer) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Parent) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Handle) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Binder) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Padding) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Num_FDs) := (S_Invalid, F_Final);
            Ctx.Cursors (F_FD) := (S_Invalid, Ctx.Cursors (F_FD).Predecessor);
            pragma Assert (Field_First (Ctx, Fld) = First
               and Field_Length (Ctx, Fld) = Length);
         when F_Num_FDs =>
            Ctx.Cursors (F_Index) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Cookie) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Length) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Parent_Offset) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Unused_Padding) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Buffer) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Parent) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Handle) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Binder) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Padding) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Num_FDs) := (S_Invalid, Ctx.Cursors (F_Num_FDs).Predecessor);
            pragma Assert (Field_First (Ctx, Fld) = First
               and Field_Length (Ctx, Fld) = Length);
         when F_Padding =>
            Ctx.Cursors (F_Index) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Cookie) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Length) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Parent_Offset) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Unused_Padding) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Buffer) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Parent) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Handle) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Binder) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Padding) := (S_Invalid, Ctx.Cursors (F_Padding).Predecessor);
            pragma Assert (Field_First (Ctx, Fld) = First
               and Field_Length (Ctx, Fld) = Length);
         when F_Binder =>
            Ctx.Cursors (F_Index) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Cookie) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Length) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Parent_Offset) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Unused_Padding) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Buffer) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Parent) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Handle) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Binder) := (S_Invalid, Ctx.Cursors (F_Binder).Predecessor);
            pragma Assert (Field_First (Ctx, Fld) = First
               and Field_Length (Ctx, Fld) = Length);
         when F_Handle =>
            Ctx.Cursors (F_Index) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Cookie) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Length) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Parent_Offset) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Unused_Padding) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Buffer) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Parent) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Handle) := (S_Invalid, Ctx.Cursors (F_Handle).Predecessor);
            pragma Assert (Field_First (Ctx, Fld) = First
               and Field_Length (Ctx, Fld) = Length);
         when F_Parent =>
            Ctx.Cursors (F_Index) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Cookie) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Length) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Parent_Offset) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Unused_Padding) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Buffer) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Parent) := (S_Invalid, Ctx.Cursors (F_Parent).Predecessor);
            pragma Assert (Field_First (Ctx, Fld) = First
               and Field_Length (Ctx, Fld) = Length);
         when F_Buffer =>
            Ctx.Cursors (F_Index) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Cookie) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Length) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Parent_Offset) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Unused_Padding) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Buffer) := (S_Invalid, Ctx.Cursors (F_Buffer).Predecessor);
            pragma Assert (Field_First (Ctx, Fld) = First
               and Field_Length (Ctx, Fld) = Length);
         when F_Unused_Padding =>
            Ctx.Cursors (F_Index) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Cookie) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Length) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Parent_Offset) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Unused_Padding) := (S_Invalid, Ctx.Cursors (F_Unused_Padding).Predecessor);
            pragma Assert (Field_First (Ctx, Fld) = First
               and Field_Length (Ctx, Fld) = Length);
         when F_Parent_Offset =>
            Ctx.Cursors (F_Index) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Cookie) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Length) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Parent_Offset) := (S_Invalid, Ctx.Cursors (F_Parent_Offset).Predecessor);
            pragma Assert (Field_First (Ctx, Fld) = First
               and Field_Length (Ctx, Fld) = Length);
         when F_Length =>
            Ctx.Cursors (F_Index) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Cookie) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Length) := (S_Invalid, Ctx.Cursors (F_Length).Predecessor);
            pragma Assert (Field_First (Ctx, Fld) = First
               and Field_Length (Ctx, Fld) = Length);
         when F_Cookie =>
            Ctx.Cursors (F_Index) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Cookie) := (S_Invalid, Ctx.Cursors (F_Cookie).Predecessor);
            pragma Assert (Field_First (Ctx, Fld) = First
               and Field_Length (Ctx, Fld) = Length);
         when F_Index =>
            Ctx.Cursors (F_Index) := (S_Invalid, Ctx.Cursors (F_Index).Predecessor);
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
         when F_Kind | F_Arity | F_Tag | F_Legacy_Flags | F_Has_Parent | F_Flags | F_FD | F_Num_FDs | F_Padding | F_Binder | F_Handle | F_Parent | F_Buffer | F_Unused_Padding | F_Parent_Offset | F_Length | F_Cookie | F_Index =>
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
      function Extract is new Types.Extract (Protocol.Pad32);
      function Extract is new Types.Extract (Builtin_Types.Boolean_Base);
      function Extract is new Types.Extract (Protocol.Flat_Binder_Flags_Base);
      function Extract is new Types.Extract (Protocol.Handle_Base);
      function Extract is new Types.Extract (Protocol.Count);
      function Extract is new Types.Extract (Protocol.Pad31);
      function Extract is new Types.Extract (Protocol.Binder);
      function Extract is new Types.Extract (Protocol.Index);
      function Extract is new Types.Extract (Protocol.Offset_Base);
      function Extract is new Types.Extract (Protocol.Length_Base);
      function Extract is new Types.Extract (Protocol.Cookie);
   begin
      return ((case Fld is
            when F_Kind =>
               (Fld => F_Kind, Kind_Value => Extract (Ctx.Buffer.all (Buffer_First .. Buffer_Last), Offset)),
            when F_Arity =>
               (Fld => F_Arity, Arity_Value => Extract (Ctx.Buffer.all (Buffer_First .. Buffer_Last), Offset)),
            when F_Tag =>
               (Fld => F_Tag, Tag_Value => Extract (Ctx.Buffer.all (Buffer_First .. Buffer_Last), Offset)),
            when F_Legacy_Flags =>
               (Fld => F_Legacy_Flags, Legacy_Flags_Value => Extract (Ctx.Buffer.all (Buffer_First .. Buffer_Last), Offset)),
            when F_Has_Parent =>
               (Fld => F_Has_Parent, Has_Parent_Value => Extract (Ctx.Buffer.all (Buffer_First .. Buffer_Last), Offset)),
            when F_Flags =>
               (Fld => F_Flags, Flags_Value => Extract (Ctx.Buffer.all (Buffer_First .. Buffer_Last), Offset)),
            when F_FD =>
               (Fld => F_FD, FD_Value => Extract (Ctx.Buffer.all (Buffer_First .. Buffer_Last), Offset)),
            when F_Num_FDs =>
               (Fld => F_Num_FDs, Num_FDs_Value => Extract (Ctx.Buffer.all (Buffer_First .. Buffer_Last), Offset)),
            when F_Padding =>
               (Fld => F_Padding, Padding_Value => Extract (Ctx.Buffer.all (Buffer_First .. Buffer_Last), Offset)),
            when F_Binder =>
               (Fld => F_Binder, Binder_Value => Extract (Ctx.Buffer.all (Buffer_First .. Buffer_Last), Offset)),
            when F_Handle =>
               (Fld => F_Handle, Handle_Value => Extract (Ctx.Buffer.all (Buffer_First .. Buffer_Last), Offset)),
            when F_Parent =>
               (Fld => F_Parent, Parent_Value => Extract (Ctx.Buffer.all (Buffer_First .. Buffer_Last), Offset)),
            when F_Buffer =>
               (Fld => F_Buffer, Buffer_Value => Extract (Ctx.Buffer.all (Buffer_First .. Buffer_Last), Offset)),
            when F_Unused_Padding =>
               (Fld => F_Unused_Padding, Unused_Padding_Value => Extract (Ctx.Buffer.all (Buffer_First .. Buffer_Last), Offset)),
            when F_Parent_Offset =>
               (Fld => F_Parent_Offset, Parent_Offset_Value => Extract (Ctx.Buffer.all (Buffer_First .. Buffer_Last), Offset)),
            when F_Length =>
               (Fld => F_Length, Length_Value => Extract (Ctx.Buffer.all (Buffer_First .. Buffer_Last), Offset)),
            when F_Cookie =>
               (Fld => F_Cookie, Cookie_Value => Extract (Ctx.Buffer.all (Buffer_First .. Buffer_Last), Offset)),
            when F_Index =>
               (Fld => F_Index, Index_Value => Extract (Ctx.Buffer.all (Buffer_First .. Buffer_Last), Offset))));
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
                               and then (if Structural_Valid (Ctx.Cursors (F_Legacy_Flags))
                                    and then Types.Bit_Length (Ctx.Cursors (F_Kind).Value.Kind_Value) = Types.Bit_Length (Convert (BK_FD)) then
                                  (Ctx.Cursors (F_Legacy_Flags).Last - Ctx.Cursors (F_Legacy_Flags).First + 1) = Protocol.Pad32'Size
                                    and then Ctx.Cursors (F_Legacy_Flags).Predecessor = F_Tag
                                    and then Ctx.Cursors (F_Legacy_Flags).First = (Ctx.Cursors (F_Tag).Last + 1)
                                    and then (if Structural_Valid (Ctx.Cursors (F_FD))
                                         and then Types.Bit_Length (Ctx.Cursors (F_Arity).Value.Arity_Value) = Types.Bit_Length (Convert (BA_SINGLE)) then
                                       (Ctx.Cursors (F_FD).Last - Ctx.Cursors (F_FD).First + 1) = Protocol.Handle_Base'Size
                                         and then Ctx.Cursors (F_FD).Predecessor = F_Legacy_Flags
                                         and then Ctx.Cursors (F_FD).First = (Ctx.Cursors (F_Legacy_Flags).Last + 1)
                                         and then (if Structural_Valid (Ctx.Cursors (F_Cookie)) then
                                            (Ctx.Cursors (F_Cookie).Last - Ctx.Cursors (F_Cookie).First + 1) = Protocol.Cookie'Size
                                              and then Ctx.Cursors (F_Cookie).Predecessor = F_FD
                                              and then Ctx.Cursors (F_Cookie).First = (Ctx.Cursors (F_FD).Last + 1)))
                                    and then (if Structural_Valid (Ctx.Cursors (F_Num_FDs))
                                         and then Types.Bit_Length (Ctx.Cursors (F_Arity).Value.Arity_Value) = Types.Bit_Length (Convert (BA_ARRAY)) then
                                       (Ctx.Cursors (F_Num_FDs).Last - Ctx.Cursors (F_Num_FDs).First + 1) = Protocol.Count'Size
                                         and then Ctx.Cursors (F_Num_FDs).Predecessor = F_Legacy_Flags
                                         and then Ctx.Cursors (F_Num_FDs).First = (Ctx.Cursors (F_Legacy_Flags).Last + 1)
                                         and then (if Structural_Valid (Ctx.Cursors (F_Parent)) then
                                            (Ctx.Cursors (F_Parent).Last - Ctx.Cursors (F_Parent).First + 1) = Protocol.Index'Size
                                              and then Ctx.Cursors (F_Parent).Predecessor = F_Num_FDs
                                              and then Ctx.Cursors (F_Parent).First = (Ctx.Cursors (F_Num_FDs).Last + 1)
                                              and then (if Structural_Valid (Ctx.Cursors (F_Parent_Offset)) then
                                                 (Ctx.Cursors (F_Parent_Offset).Last - Ctx.Cursors (F_Parent_Offset).First + 1) = Protocol.Offset_Base'Size
                                                   and then Ctx.Cursors (F_Parent_Offset).Predecessor = F_Parent
                                                   and then Ctx.Cursors (F_Parent_Offset).First = (Ctx.Cursors (F_Parent).Last + 1)))))
                               and then (if Structural_Valid (Ctx.Cursors (F_Has_Parent))
                                    and then Types.Bit_Length (Ctx.Cursors (F_Kind).Value.Kind_Value) = Types.Bit_Length (Convert (BK_POINTER)) then
                                  (Ctx.Cursors (F_Has_Parent).Last - Ctx.Cursors (F_Has_Parent).First + 1) = Builtin_Types.Boolean_Base'Size
                                    and then Ctx.Cursors (F_Has_Parent).Predecessor = F_Tag
                                    and then Ctx.Cursors (F_Has_Parent).First = (Ctx.Cursors (F_Tag).Last + 1)
                                    and then (if Structural_Valid (Ctx.Cursors (F_Padding)) then
                                       (Ctx.Cursors (F_Padding).Last - Ctx.Cursors (F_Padding).First + 1) = Protocol.Pad31'Size
                                         and then Ctx.Cursors (F_Padding).Predecessor = F_Has_Parent
                                         and then Ctx.Cursors (F_Padding).First = (Ctx.Cursors (F_Has_Parent).Last + 1)
                                         and then (if Structural_Valid (Ctx.Cursors (F_Buffer)) then
                                            (Ctx.Cursors (F_Buffer).Last - Ctx.Cursors (F_Buffer).First + 1) = Protocol.Index'Size
                                              and then Ctx.Cursors (F_Buffer).Predecessor = F_Padding
                                              and then Ctx.Cursors (F_Buffer).First = (Ctx.Cursors (F_Padding).Last + 1)
                                              and then (if Structural_Valid (Ctx.Cursors (F_Length)) then
                                                 (Ctx.Cursors (F_Length).Last - Ctx.Cursors (F_Length).First + 1) = Protocol.Length_Base'Size
                                                   and then Ctx.Cursors (F_Length).Predecessor = F_Buffer
                                                   and then Ctx.Cursors (F_Length).First = (Ctx.Cursors (F_Buffer).Last + 1)
                                                   and then (if Structural_Valid (Ctx.Cursors (F_Index))
                                                        and then Types.Bit_Length (Ctx.Cursors (F_Has_Parent).Value.Has_Parent_Value) = Types.Bit_Length (Convert (True)) then
                                                      (Ctx.Cursors (F_Index).Last - Ctx.Cursors (F_Index).First + 1) = Protocol.Index'Size
                                                        and then Ctx.Cursors (F_Index).Predecessor = F_Length
                                                        and then Ctx.Cursors (F_Index).First = (Ctx.Cursors (F_Length).Last + 1))))))
                               and then (if Structural_Valid (Ctx.Cursors (F_Flags))
                                    and then (Types.Bit_Length (Ctx.Cursors (F_Kind).Value.Kind_Value) /= Types.Bit_Length (Convert (BK_POINTER))
                                      and Types.Bit_Length (Ctx.Cursors (F_Kind).Value.Kind_Value) /= Types.Bit_Length (Convert (BK_FD))) then
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
                                         and then (if Structural_Valid (Ctx.Cursors (F_Unused_Padding)) then
                                            (Ctx.Cursors (F_Unused_Padding).Last - Ctx.Cursors (F_Unused_Padding).First + 1) = Protocol.Pad32'Size
                                              and then Ctx.Cursors (F_Unused_Padding).Predecessor = F_Handle
                                              and then Ctx.Cursors (F_Unused_Padding).First = (Ctx.Cursors (F_Handle).Last + 1)
                                              and then (if Structural_Valid (Ctx.Cursors (F_Cookie)) then
                                                 (Ctx.Cursors (F_Cookie).Last - Ctx.Cursors (F_Cookie).First + 1) = Protocol.Cookie'Size
                                                   and then Ctx.Cursors (F_Cookie).Predecessor = F_Unused_Padding
                                                   and then Ctx.Cursors (F_Cookie).First = (Ctx.Cursors (F_Unused_Padding).Last + 1)))))))));
               if Fld = F_Kind then
                  Ctx.Cursors (Successor (Ctx, Fld)) := (State => S_Invalid, Predecessor => Fld);
               elsif Fld = F_Arity then
                  Ctx.Cursors (Successor (Ctx, Fld)) := (State => S_Invalid, Predecessor => Fld);
               elsif Fld = F_Tag then
                  Ctx.Cursors (Successor (Ctx, Fld)) := (State => S_Invalid, Predecessor => Fld);
               elsif Fld = F_Legacy_Flags then
                  Ctx.Cursors (Successor (Ctx, Fld)) := (State => S_Invalid, Predecessor => Fld);
               elsif Fld = F_Has_Parent then
                  Ctx.Cursors (Successor (Ctx, Fld)) := (State => S_Invalid, Predecessor => Fld);
               elsif Fld = F_Flags then
                  Ctx.Cursors (Successor (Ctx, Fld)) := (State => S_Invalid, Predecessor => Fld);
               elsif Fld = F_FD then
                  Ctx.Cursors (Successor (Ctx, Fld)) := (State => S_Invalid, Predecessor => Fld);
               elsif Fld = F_Num_FDs then
                  Ctx.Cursors (Successor (Ctx, Fld)) := (State => S_Invalid, Predecessor => Fld);
               elsif Fld = F_Padding then
                  Ctx.Cursors (Successor (Ctx, Fld)) := (State => S_Invalid, Predecessor => Fld);
               elsif Fld = F_Binder then
                  Ctx.Cursors (Successor (Ctx, Fld)) := (State => S_Invalid, Predecessor => Fld);
               elsif Fld = F_Handle then
                  Ctx.Cursors (Successor (Ctx, Fld)) := (State => S_Invalid, Predecessor => Fld);
               elsif Fld = F_Parent then
                  Ctx.Cursors (Successor (Ctx, Fld)) := (State => S_Invalid, Predecessor => Fld);
               elsif Fld = F_Buffer then
                  Ctx.Cursors (Successor (Ctx, Fld)) := (State => S_Invalid, Predecessor => Fld);
               elsif Fld = F_Unused_Padding then
                  Ctx.Cursors (Successor (Ctx, Fld)) := (State => S_Invalid, Predecessor => Fld);
               elsif Fld = F_Parent_Offset then
                  Ctx.Cursors (Successor (Ctx, Fld)) := (State => S_Invalid, Predecessor => Fld);
               elsif Fld = F_Length then
                  Ctx.Cursors (Successor (Ctx, Fld)) := (State => S_Invalid, Predecessor => Fld);
               elsif Fld = F_Cookie then
                  Ctx.Cursors (Successor (Ctx, Fld)) := (State => S_Invalid, Predecessor => Fld);
               elsif Fld = F_Index then
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
      Verify (Ctx, F_Legacy_Flags);
      Verify (Ctx, F_Has_Parent);
      Verify (Ctx, F_Flags);
      Verify (Ctx, F_FD);
      Verify (Ctx, F_Num_FDs);
      Verify (Ctx, F_Padding);
      Verify (Ctx, F_Binder);
      Verify (Ctx, F_Handle);
      Verify (Ctx, F_Parent);
      Verify (Ctx, F_Buffer);
      Verify (Ctx, F_Unused_Padding);
      Verify (Ctx, F_Parent_Offset);
      Verify (Ctx, F_Length);
      Verify (Ctx, F_Cookie);
      Verify (Ctx, F_Index);
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
      and then ((Valid (Ctx, F_Legacy_Flags)
          and then Types.Bit_Length (Ctx.Cursors (F_Kind).Value.Kind_Value) = Types.Bit_Length (Convert (BK_FD))
          and then ((Valid (Ctx, F_FD)
              and then Types.Bit_Length (Ctx.Cursors (F_Arity).Value.Arity_Value) = Types.Bit_Length (Convert (BA_SINGLE))
              and then Valid (Ctx, F_Cookie))
            or (Valid (Ctx, F_Num_FDs)
              and then Types.Bit_Length (Ctx.Cursors (F_Arity).Value.Arity_Value) = Types.Bit_Length (Convert (BA_ARRAY))
              and then Valid (Ctx, F_Parent)
              and then Valid (Ctx, F_Parent_Offset))))
        or (Valid (Ctx, F_Has_Parent)
          and then Types.Bit_Length (Ctx.Cursors (F_Kind).Value.Kind_Value) = Types.Bit_Length (Convert (BK_POINTER))
          and then Valid (Ctx, F_Padding)
          and then Valid (Ctx, F_Buffer)
          and then Valid (Ctx, F_Length)
          and then ((Valid (Ctx, F_Index)
              and then Types.Bit_Length (Ctx.Cursors (F_Has_Parent).Value.Has_Parent_Value) = Types.Bit_Length (Convert (True)))
            or Types.Bit_Length (Ctx.Cursors (F_Has_Parent).Value.Has_Parent_Value) = Types.Bit_Length (Convert (False))))
        or (Valid (Ctx, F_Flags)
          and then (Types.Bit_Length (Ctx.Cursors (F_Kind).Value.Kind_Value) /= Types.Bit_Length (Convert (BK_POINTER))
            and Types.Bit_Length (Ctx.Cursors (F_Kind).Value.Kind_Value) /= Types.Bit_Length (Convert (BK_FD)))
          and then ((Valid (Ctx, F_Binder)
              and then (Types.Bit_Length (Ctx.Cursors (F_Kind).Value.Kind_Value) = Types.Bit_Length (Convert (BK_STRONG_BINDER))
                or Types.Bit_Length (Ctx.Cursors (F_Kind).Value.Kind_Value) = Types.Bit_Length (Convert (BK_WEAK_BINDER)))
              and then Valid (Ctx, F_Cookie))
            or (Valid (Ctx, F_Handle)
              and then (Types.Bit_Length (Ctx.Cursors (F_Kind).Value.Kind_Value) = Types.Bit_Length (Convert (BK_STRONG_HANDLE))
                or Types.Bit_Length (Ctx.Cursors (F_Kind).Value.Kind_Value) = Types.Bit_Length (Convert (BK_WEAK_HANDLE)))
              and then Valid (Ctx, F_Unused_Padding)
              and then Valid (Ctx, F_Cookie))))));

   function Valid_Message (Ctx : Context) return Boolean is
     (Valid (Ctx, F_Kind)
      and then Valid (Ctx, F_Arity)
      and then Valid (Ctx, F_Tag)
      and then (Types.Bit_Length (Ctx.Cursors (F_Arity).Value.Arity_Value) = Types.Bit_Length (Convert (BA_SINGLE))
        or (Types.Bit_Length (Ctx.Cursors (F_Kind).Value.Kind_Value) = Types.Bit_Length (Convert (BK_FD))
          and Types.Bit_Length (Ctx.Cursors (F_Arity).Value.Arity_Value) = Types.Bit_Length (Convert (BA_ARRAY))))
      and then ((Valid (Ctx, F_Legacy_Flags)
          and then Types.Bit_Length (Ctx.Cursors (F_Kind).Value.Kind_Value) = Types.Bit_Length (Convert (BK_FD))
          and then ((Valid (Ctx, F_FD)
              and then Types.Bit_Length (Ctx.Cursors (F_Arity).Value.Arity_Value) = Types.Bit_Length (Convert (BA_SINGLE))
              and then Valid (Ctx, F_Cookie))
            or (Valid (Ctx, F_Num_FDs)
              and then Types.Bit_Length (Ctx.Cursors (F_Arity).Value.Arity_Value) = Types.Bit_Length (Convert (BA_ARRAY))
              and then Valid (Ctx, F_Parent)
              and then Valid (Ctx, F_Parent_Offset))))
        or (Valid (Ctx, F_Has_Parent)
          and then Types.Bit_Length (Ctx.Cursors (F_Kind).Value.Kind_Value) = Types.Bit_Length (Convert (BK_POINTER))
          and then Valid (Ctx, F_Padding)
          and then Valid (Ctx, F_Buffer)
          and then Valid (Ctx, F_Length)
          and then ((Valid (Ctx, F_Index)
              and then Types.Bit_Length (Ctx.Cursors (F_Has_Parent).Value.Has_Parent_Value) = Types.Bit_Length (Convert (True)))
            or Types.Bit_Length (Ctx.Cursors (F_Has_Parent).Value.Has_Parent_Value) = Types.Bit_Length (Convert (False))))
        or (Valid (Ctx, F_Flags)
          and then (Types.Bit_Length (Ctx.Cursors (F_Kind).Value.Kind_Value) /= Types.Bit_Length (Convert (BK_POINTER))
            and Types.Bit_Length (Ctx.Cursors (F_Kind).Value.Kind_Value) /= Types.Bit_Length (Convert (BK_FD)))
          and then ((Valid (Ctx, F_Binder)
              and then (Types.Bit_Length (Ctx.Cursors (F_Kind).Value.Kind_Value) = Types.Bit_Length (Convert (BK_STRONG_BINDER))
                or Types.Bit_Length (Ctx.Cursors (F_Kind).Value.Kind_Value) = Types.Bit_Length (Convert (BK_WEAK_BINDER)))
              and then Valid (Ctx, F_Cookie))
            or (Valid (Ctx, F_Handle)
              and then (Types.Bit_Length (Ctx.Cursors (F_Kind).Value.Kind_Value) = Types.Bit_Length (Convert (BK_STRONG_HANDLE))
                or Types.Bit_Length (Ctx.Cursors (F_Kind).Value.Kind_Value) = Types.Bit_Length (Convert (BK_WEAK_HANDLE)))
              and then Valid (Ctx, F_Unused_Padding)
              and then Valid (Ctx, F_Cookie))))));

   function Incomplete_Message (Ctx : Context) return Boolean is
     (Incomplete (Ctx, F_Kind)
      or Incomplete (Ctx, F_Arity)
      or Incomplete (Ctx, F_Tag)
      or Incomplete (Ctx, F_Legacy_Flags)
      or Incomplete (Ctx, F_Has_Parent)
      or Incomplete (Ctx, F_Flags)
      or Incomplete (Ctx, F_FD)
      or Incomplete (Ctx, F_Num_FDs)
      or Incomplete (Ctx, F_Padding)
      or Incomplete (Ctx, F_Binder)
      or Incomplete (Ctx, F_Handle)
      or Incomplete (Ctx, F_Parent)
      or Incomplete (Ctx, F_Buffer)
      or Incomplete (Ctx, F_Unused_Padding)
      or Incomplete (Ctx, F_Parent_Offset)
      or Incomplete (Ctx, F_Length)
      or Incomplete (Ctx, F_Cookie)
      or Incomplete (Ctx, F_Index));

   function Get_Kind (Ctx : Context) return Protocol.Binder_Kind is
     (Convert (Ctx.Cursors (F_Kind).Value.Kind_Value));

   function Get_Arity (Ctx : Context) return Protocol.Binder_Arity is
     (Convert (Ctx.Cursors (F_Arity).Value.Arity_Value));

   function Get_Tag (Ctx : Context) return Protocol.Binder_Tag is
     (Ctx.Cursors (F_Tag).Value.Tag_Value);

   function Get_Legacy_Flags (Ctx : Context) return Protocol.Pad32 is
     (Ctx.Cursors (F_Legacy_Flags).Value.Legacy_Flags_Value);

   function Get_Has_Parent (Ctx : Context) return Boolean is
     (Convert (Ctx.Cursors (F_Has_Parent).Value.Has_Parent_Value));

   function Get_Flags (Ctx : Context) return Protocol.Flat_Binder_Flags is
     (Convert (Ctx.Cursors (F_Flags).Value.Flags_Value));

   function Get_FD (Ctx : Context) return Protocol.Handle is
     (Ctx.Cursors (F_FD).Value.FD_Value);

   function Get_Num_FDs (Ctx : Context) return Protocol.Count is
     (Ctx.Cursors (F_Num_FDs).Value.Num_FDs_Value);

   function Get_Padding (Ctx : Context) return Protocol.Pad31 is
     (Ctx.Cursors (F_Padding).Value.Padding_Value);

   function Get_Binder (Ctx : Context) return Protocol.Binder is
     (Ctx.Cursors (F_Binder).Value.Binder_Value);

   function Get_Handle (Ctx : Context) return Protocol.Handle is
     (Ctx.Cursors (F_Handle).Value.Handle_Value);

   function Get_Parent (Ctx : Context) return Protocol.Index is
     (Ctx.Cursors (F_Parent).Value.Parent_Value);

   function Get_Buffer (Ctx : Context) return Protocol.Index is
     (Ctx.Cursors (F_Buffer).Value.Buffer_Value);

   function Get_Unused_Padding (Ctx : Context) return Protocol.Pad32 is
     (Ctx.Cursors (F_Unused_Padding).Value.Unused_Padding_Value);

   function Get_Parent_Offset (Ctx : Context) return Protocol.Offset is
     (Ctx.Cursors (F_Parent_Offset).Value.Parent_Offset_Value);

   function Get_Length (Ctx : Context) return Protocol.Length is
     (Ctx.Cursors (F_Length).Value.Length_Value);

   function Get_Cookie (Ctx : Context) return Protocol.Cookie is
     (Ctx.Cursors (F_Cookie).Value.Cookie_Value);

   function Get_Index (Ctx : Context) return Protocol.Index is
     (Ctx.Cursors (F_Index).Value.Index_Value);

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
      procedure Insert is new Types.Insert (Protocol.Pad32);
      procedure Insert is new Types.Insert (Builtin_Types.Boolean_Base);
      procedure Insert is new Types.Insert (Protocol.Flat_Binder_Flags_Base);
      procedure Insert is new Types.Insert (Protocol.Handle_Base);
      procedure Insert is new Types.Insert (Protocol.Count);
      procedure Insert is new Types.Insert (Protocol.Pad31);
      procedure Insert is new Types.Insert (Protocol.Binder);
      procedure Insert is new Types.Insert (Protocol.Index);
      procedure Insert is new Types.Insert (Protocol.Offset_Base);
      procedure Insert is new Types.Insert (Protocol.Length_Base);
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
         when F_Legacy_Flags =>
            Insert (Val.Legacy_Flags_Value, Ctx.Buffer.all (Buffer_First .. Buffer_Last), Offset);
         when F_Has_Parent =>
            Insert (Val.Has_Parent_Value, Ctx.Buffer.all (Buffer_First .. Buffer_Last), Offset);
         when F_Flags =>
            Insert (Val.Flags_Value, Ctx.Buffer.all (Buffer_First .. Buffer_Last), Offset);
         when F_FD =>
            Insert (Val.FD_Value, Ctx.Buffer.all (Buffer_First .. Buffer_Last), Offset);
         when F_Num_FDs =>
            Insert (Val.Num_FDs_Value, Ctx.Buffer.all (Buffer_First .. Buffer_Last), Offset);
         when F_Padding =>
            Insert (Val.Padding_Value, Ctx.Buffer.all (Buffer_First .. Buffer_Last), Offset);
         when F_Binder =>
            Insert (Val.Binder_Value, Ctx.Buffer.all (Buffer_First .. Buffer_Last), Offset);
         when F_Handle =>
            Insert (Val.Handle_Value, Ctx.Buffer.all (Buffer_First .. Buffer_Last), Offset);
         when F_Parent =>
            Insert (Val.Parent_Value, Ctx.Buffer.all (Buffer_First .. Buffer_Last), Offset);
         when F_Buffer =>
            Insert (Val.Buffer_Value, Ctx.Buffer.all (Buffer_First .. Buffer_Last), Offset);
         when F_Unused_Padding =>
            Insert (Val.Unused_Padding_Value, Ctx.Buffer.all (Buffer_First .. Buffer_Last), Offset);
         when F_Parent_Offset =>
            Insert (Val.Parent_Offset_Value, Ctx.Buffer.all (Buffer_First .. Buffer_Last), Offset);
         when F_Length =>
            Insert (Val.Length_Value, Ctx.Buffer.all (Buffer_First .. Buffer_Last), Offset);
         when F_Cookie =>
            Insert (Val.Cookie_Value, Ctx.Buffer.all (Buffer_First .. Buffer_Last), Offset);
         when F_Index =>
            Insert (Val.Index_Value, Ctx.Buffer.all (Buffer_First .. Buffer_Last), Offset);
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

   procedure Set_Legacy_Flags (Ctx : in out Context; Val : Protocol.Pad32) is
      Field_Value : constant Field_Dependent_Value := (F_Legacy_Flags, Val);
      First, Last : Types.Bit_Index;
   begin
      Reset_Dependent_Fields (Ctx, F_Legacy_Flags);
      Set_Field_Value (Ctx, Field_Value, First, Last);
      Ctx := (Ctx.Buffer_First, Ctx.Buffer_Last, Ctx.First, Last, Ctx.Buffer, Ctx.Cursors);
      Ctx.Cursors (F_Legacy_Flags) := (State => S_Valid, First => First, Last => Last, Value => Field_Value, Predecessor => Ctx.Cursors (F_Legacy_Flags).Predecessor);
      Ctx.Cursors (Successor (Ctx, F_Legacy_Flags)) := (State => S_Invalid, Predecessor => F_Legacy_Flags);
   end Set_Legacy_Flags;

   procedure Set_Has_Parent (Ctx : in out Context; Val : Boolean) is
      Field_Value : constant Field_Dependent_Value := (F_Has_Parent, Convert (Val));
      First, Last : Types.Bit_Index;
   begin
      Reset_Dependent_Fields (Ctx, F_Has_Parent);
      Set_Field_Value (Ctx, Field_Value, First, Last);
      Ctx := (Ctx.Buffer_First, Ctx.Buffer_Last, Ctx.First, Last, Ctx.Buffer, Ctx.Cursors);
      Ctx.Cursors (F_Has_Parent) := (State => S_Valid, First => First, Last => Last, Value => Field_Value, Predecessor => Ctx.Cursors (F_Has_Parent).Predecessor);
      Ctx.Cursors (Successor (Ctx, F_Has_Parent)) := (State => S_Invalid, Predecessor => F_Has_Parent);
   end Set_Has_Parent;

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

   procedure Set_FD (Ctx : in out Context; Val : Protocol.Handle) is
      Field_Value : constant Field_Dependent_Value := (F_FD, Val);
      First, Last : Types.Bit_Index;
   begin
      Reset_Dependent_Fields (Ctx, F_FD);
      Set_Field_Value (Ctx, Field_Value, First, Last);
      Ctx := (Ctx.Buffer_First, Ctx.Buffer_Last, Ctx.First, Last, Ctx.Buffer, Ctx.Cursors);
      Ctx.Cursors (F_FD) := (State => S_Valid, First => First, Last => Last, Value => Field_Value, Predecessor => Ctx.Cursors (F_FD).Predecessor);
      Ctx.Cursors (Successor (Ctx, F_FD)) := (State => S_Invalid, Predecessor => F_FD);
   end Set_FD;

   procedure Set_Num_FDs (Ctx : in out Context; Val : Protocol.Count) is
      Field_Value : constant Field_Dependent_Value := (F_Num_FDs, Val);
      First, Last : Types.Bit_Index;
   begin
      Reset_Dependent_Fields (Ctx, F_Num_FDs);
      Set_Field_Value (Ctx, Field_Value, First, Last);
      Ctx := (Ctx.Buffer_First, Ctx.Buffer_Last, Ctx.First, Last, Ctx.Buffer, Ctx.Cursors);
      Ctx.Cursors (F_Num_FDs) := (State => S_Valid, First => First, Last => Last, Value => Field_Value, Predecessor => Ctx.Cursors (F_Num_FDs).Predecessor);
      Ctx.Cursors (Successor (Ctx, F_Num_FDs)) := (State => S_Invalid, Predecessor => F_Num_FDs);
   end Set_Num_FDs;

   procedure Set_Padding (Ctx : in out Context; Val : Protocol.Pad31) is
      Field_Value : constant Field_Dependent_Value := (F_Padding, Val);
      First, Last : Types.Bit_Index;
   begin
      Reset_Dependent_Fields (Ctx, F_Padding);
      Set_Field_Value (Ctx, Field_Value, First, Last);
      Ctx := (Ctx.Buffer_First, Ctx.Buffer_Last, Ctx.First, Last, Ctx.Buffer, Ctx.Cursors);
      Ctx.Cursors (F_Padding) := (State => S_Valid, First => First, Last => Last, Value => Field_Value, Predecessor => Ctx.Cursors (F_Padding).Predecessor);
      Ctx.Cursors (Successor (Ctx, F_Padding)) := (State => S_Invalid, Predecessor => F_Padding);
   end Set_Padding;

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

   procedure Set_Parent (Ctx : in out Context; Val : Protocol.Index) is
      Field_Value : constant Field_Dependent_Value := (F_Parent, Val);
      First, Last : Types.Bit_Index;
   begin
      Reset_Dependent_Fields (Ctx, F_Parent);
      Set_Field_Value (Ctx, Field_Value, First, Last);
      Ctx := (Ctx.Buffer_First, Ctx.Buffer_Last, Ctx.First, Last, Ctx.Buffer, Ctx.Cursors);
      Ctx.Cursors (F_Parent) := (State => S_Valid, First => First, Last => Last, Value => Field_Value, Predecessor => Ctx.Cursors (F_Parent).Predecessor);
      Ctx.Cursors (Successor (Ctx, F_Parent)) := (State => S_Invalid, Predecessor => F_Parent);
   end Set_Parent;

   procedure Set_Buffer (Ctx : in out Context; Val : Protocol.Index) is
      Field_Value : constant Field_Dependent_Value := (F_Buffer, Val);
      First, Last : Types.Bit_Index;
   begin
      Reset_Dependent_Fields (Ctx, F_Buffer);
      Set_Field_Value (Ctx, Field_Value, First, Last);
      Ctx := (Ctx.Buffer_First, Ctx.Buffer_Last, Ctx.First, Last, Ctx.Buffer, Ctx.Cursors);
      Ctx.Cursors (F_Buffer) := (State => S_Valid, First => First, Last => Last, Value => Field_Value, Predecessor => Ctx.Cursors (F_Buffer).Predecessor);
      Ctx.Cursors (Successor (Ctx, F_Buffer)) := (State => S_Invalid, Predecessor => F_Buffer);
   end Set_Buffer;

   procedure Set_Unused_Padding (Ctx : in out Context; Val : Protocol.Pad32) is
      Field_Value : constant Field_Dependent_Value := (F_Unused_Padding, Val);
      First, Last : Types.Bit_Index;
   begin
      Reset_Dependent_Fields (Ctx, F_Unused_Padding);
      Set_Field_Value (Ctx, Field_Value, First, Last);
      Ctx := (Ctx.Buffer_First, Ctx.Buffer_Last, Ctx.First, Last, Ctx.Buffer, Ctx.Cursors);
      Ctx.Cursors (F_Unused_Padding) := (State => S_Valid, First => First, Last => Last, Value => Field_Value, Predecessor => Ctx.Cursors (F_Unused_Padding).Predecessor);
      Ctx.Cursors (Successor (Ctx, F_Unused_Padding)) := (State => S_Invalid, Predecessor => F_Unused_Padding);
   end Set_Unused_Padding;

   procedure Set_Parent_Offset (Ctx : in out Context; Val : Protocol.Offset) is
      Field_Value : constant Field_Dependent_Value := (F_Parent_Offset, Val);
      First, Last : Types.Bit_Index;
   begin
      Reset_Dependent_Fields (Ctx, F_Parent_Offset);
      Set_Field_Value (Ctx, Field_Value, First, Last);
      Ctx := (Ctx.Buffer_First, Ctx.Buffer_Last, Ctx.First, Last, Ctx.Buffer, Ctx.Cursors);
      Ctx.Cursors (F_Parent_Offset) := (State => S_Valid, First => First, Last => Last, Value => Field_Value, Predecessor => Ctx.Cursors (F_Parent_Offset).Predecessor);
      Ctx.Cursors (Successor (Ctx, F_Parent_Offset)) := (State => S_Invalid, Predecessor => F_Parent_Offset);
   end Set_Parent_Offset;

   procedure Set_Length (Ctx : in out Context; Val : Protocol.Length) is
      Field_Value : constant Field_Dependent_Value := (F_Length, Val);
      First, Last : Types.Bit_Index;
   begin
      Reset_Dependent_Fields (Ctx, F_Length);
      Set_Field_Value (Ctx, Field_Value, First, Last);
      Ctx := (Ctx.Buffer_First, Ctx.Buffer_Last, Ctx.First, Last, Ctx.Buffer, Ctx.Cursors);
      Ctx.Cursors (F_Length) := (State => S_Valid, First => First, Last => Last, Value => Field_Value, Predecessor => Ctx.Cursors (F_Length).Predecessor);
      Ctx.Cursors (Successor (Ctx, F_Length)) := (State => S_Invalid, Predecessor => F_Length);
   end Set_Length;

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

   procedure Set_Index (Ctx : in out Context; Val : Protocol.Index) is
      Field_Value : constant Field_Dependent_Value := (F_Index, Val);
      First, Last : Types.Bit_Index;
   begin
      Reset_Dependent_Fields (Ctx, F_Index);
      Set_Field_Value (Ctx, Field_Value, First, Last);
      Ctx := (Ctx.Buffer_First, Ctx.Buffer_Last, Ctx.First, Last, Ctx.Buffer, Ctx.Cursors);
      Ctx.Cursors (F_Index) := (State => S_Valid, First => First, Last => Last, Value => Field_Value, Predecessor => Ctx.Cursors (F_Index).Predecessor);
      Ctx.Cursors (Successor (Ctx, F_Index)) := (State => S_Invalid, Predecessor => F_Index);
   end Set_Index;

end Parpen.Protocol.Generic_IBinder;
