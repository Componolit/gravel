package body Parpen.Service_Manager.Generic_Reply_Get_Service with
  SPARK_Mode
is

   function Create return Context is
     ((Types.Index'First, Types.Index'First, Types.Bit_Index'First, Types.Bit_Index'First, null, (F_Result_Kind => (State => S_Invalid, Predecessor => F_Initial), others => (State => S_Invalid, Predecessor => F_Final))));

   procedure Initialize (Ctx : out Context; Buffer : in out Types.Bytes_Ptr) is
   begin
      Initialize (Ctx, Buffer, Types.First_Bit_Index (Buffer'First), Types.Last_Bit_Index (Buffer'Last));
   end Initialize;

   procedure Initialize (Ctx : out Context; Buffer : in out Types.Bytes_Ptr; First, Last : Types.Bit_Index) is
      Buffer_First : constant Types.Index := Buffer'First;
      Buffer_Last : constant Types.Index := Buffer'Last;
   begin
      Ctx := (Buffer_First, Buffer_Last, First, Last, Buffer, (F_Result_Kind => (State => S_Invalid, Predecessor => F_Initial), others => (State => S_Invalid, Predecessor => F_Final)));
      Buffer := null;
   end Initialize;

   function Initialized (Ctx : Context) return Boolean is
     (Valid_Next (Ctx, F_Result_Kind)
      and then Available_Space (Ctx, F_Result_Kind) = (Types.Last_Bit_Index (Ctx.Buffer_Last) - Ctx.First + 1)
      and then Invalid (Ctx, F_Result_Kind)
      and then Invalid (Ctx, F_Result_Arity)
      and then Invalid (Ctx, F_Result_Tag)
      and then Invalid (Ctx, F_Result_Legacy_Flags)
      and then Invalid (Ctx, F_Result_Has_Parent)
      and then Invalid (Ctx, F_Result_Flags)
      and then Invalid (Ctx, F_Result_FD)
      and then Invalid (Ctx, F_Result_Num_FDs)
      and then Invalid (Ctx, F_Result_Padding)
      and then Invalid (Ctx, F_Result_Binder)
      and then Invalid (Ctx, F_Result_Handle)
      and then Invalid (Ctx, F_Result_Parent)
      and then Invalid (Ctx, F_Result_Buffer)
      and then Invalid (Ctx, F_Result_Unused_Padding)
      and then Invalid (Ctx, F_Result_Parent_Offset)
      and then Invalid (Ctx, F_Result_Length)
      and then Invalid (Ctx, F_Result_Cookie)
      and then Invalid (Ctx, F_Result_Index));

   procedure Take_Buffer (Ctx : in out Context; Buffer : out Types.Bytes_Ptr) is
   begin
      Buffer := Ctx.Buffer;
      Ctx.Buffer := null;
   end Take_Buffer;

   function Has_Buffer (Ctx : Context) return Boolean is
     (Ctx.Buffer /= null);

   function Message_Last (Ctx : Context) return Types.Bit_Index is
     ((if Structural_Valid (Ctx.Cursors (F_Result_Length))
         and Types.Bit_Length (Ctx.Cursors (F_Result_Has_Parent).Value.Result_Has_Parent_Value) = Types.Bit_Length (Convert (False)) then
       Ctx.Cursors (F_Result_Length).Last
    elsif Structural_Valid (Ctx.Cursors (F_Result_Index)) then
       Ctx.Cursors (F_Result_Index).Last
    elsif Structural_Valid (Ctx.Cursors (F_Result_Parent_Offset)) then
       Ctx.Cursors (F_Result_Parent_Offset).Last
    elsif Structural_Valid (Ctx.Cursors (F_Result_Cookie)) then
       Ctx.Cursors (F_Result_Cookie).Last
    else
       Types.Unreachable_Bit_Length));

   function Path_Condition (Ctx : Context; Fld : Field) return Boolean is
     ((case Ctx.Cursors (Fld).Predecessor is
         when F_Initial =>
            (case Fld is
                  when F_Result_Kind =>
                     True,
                  when others =>
                     False),
         when F_Result_Kind =>
            (case Fld is
                  when F_Result_Arity =>
                     True,
                  when others =>
                     False),
         when F_Result_Arity =>
            (case Fld is
                  when F_Result_Tag =>
                     Types.Bit_Length (Ctx.Cursors (F_Result_Arity).Value.Result_Arity_Value) = Types.Bit_Length (Convert (BA_SINGLE))
                        or (Types.Bit_Length (Ctx.Cursors (F_Result_Kind).Value.Result_Kind_Value) = Types.Bit_Length (Convert (BK_FD))
                          and Types.Bit_Length (Ctx.Cursors (F_Result_Arity).Value.Result_Arity_Value) = Types.Bit_Length (Convert (BA_ARRAY))),
                  when others =>
                     False),
         when F_Result_Tag =>
            (case Fld is
                  when F_Result_Legacy_Flags =>
                     Types.Bit_Length (Ctx.Cursors (F_Result_Kind).Value.Result_Kind_Value) = Types.Bit_Length (Convert (BK_FD)),
                  when F_Result_Has_Parent =>
                     Types.Bit_Length (Ctx.Cursors (F_Result_Kind).Value.Result_Kind_Value) = Types.Bit_Length (Convert (BK_POINTER)),
                  when F_Result_Flags =>
                     Types.Bit_Length (Ctx.Cursors (F_Result_Kind).Value.Result_Kind_Value) /= Types.Bit_Length (Convert (BK_POINTER))
                        and Types.Bit_Length (Ctx.Cursors (F_Result_Kind).Value.Result_Kind_Value) /= Types.Bit_Length (Convert (BK_FD)),
                  when others =>
                     False),
         when F_Result_Legacy_Flags =>
            (case Fld is
                  when F_Result_FD =>
                     Types.Bit_Length (Ctx.Cursors (F_Result_Arity).Value.Result_Arity_Value) = Types.Bit_Length (Convert (BA_SINGLE)),
                  when F_Result_Num_FDs =>
                     Types.Bit_Length (Ctx.Cursors (F_Result_Arity).Value.Result_Arity_Value) = Types.Bit_Length (Convert (BA_ARRAY)),
                  when others =>
                     False),
         when F_Result_Has_Parent =>
            (case Fld is
                  when F_Result_Padding =>
                     True,
                  when others =>
                     False),
         when F_Result_Flags =>
            (case Fld is
                  when F_Result_Binder =>
                     Types.Bit_Length (Ctx.Cursors (F_Result_Kind).Value.Result_Kind_Value) = Types.Bit_Length (Convert (BK_STRONG_BINDER))
                        or Types.Bit_Length (Ctx.Cursors (F_Result_Kind).Value.Result_Kind_Value) = Types.Bit_Length (Convert (BK_WEAK_BINDER)),
                  when F_Result_Handle =>
                     Types.Bit_Length (Ctx.Cursors (F_Result_Kind).Value.Result_Kind_Value) = Types.Bit_Length (Convert (BK_STRONG_HANDLE))
                        or Types.Bit_Length (Ctx.Cursors (F_Result_Kind).Value.Result_Kind_Value) = Types.Bit_Length (Convert (BK_WEAK_HANDLE)),
                  when others =>
                     False),
         when F_Result_FD =>
            (case Fld is
                  when F_Result_Cookie =>
                     True,
                  when others =>
                     False),
         when F_Result_Num_FDs =>
            (case Fld is
                  when F_Result_Parent =>
                     True,
                  when others =>
                     False),
         when F_Result_Padding =>
            (case Fld is
                  when F_Result_Buffer =>
                     True,
                  when others =>
                     False),
         when F_Result_Binder =>
            (case Fld is
                  when F_Result_Cookie =>
                     True,
                  when others =>
                     False),
         when F_Result_Handle =>
            (case Fld is
                  when F_Result_Unused_Padding =>
                     True,
                  when others =>
                     False),
         when F_Result_Parent =>
            (case Fld is
                  when F_Result_Parent_Offset =>
                     True,
                  when others =>
                     False),
         when F_Result_Buffer =>
            (case Fld is
                  when F_Result_Length =>
                     True,
                  when others =>
                     False),
         when F_Result_Unused_Padding =>
            (case Fld is
                  when F_Result_Cookie =>
                     True,
                  when others =>
                     False),
         when F_Result_Parent_Offset =>
            False,
         when F_Result_Length =>
            (case Fld is
                  when F_Result_Index =>
                     Types.Bit_Length (Ctx.Cursors (F_Result_Has_Parent).Value.Result_Has_Parent_Value) = Types.Bit_Length (Convert (True)),
                  when others =>
                     False),
         when F_Result_Cookie | F_Result_Index | F_Final =>
            False));

   function Field_Condition (Ctx : Context; Val : Field_Dependent_Value) return Boolean is
     ((case Val.Fld is
         when F_Initial | F_Result_Kind =>
            True,
         when F_Result_Arity =>
            Types.Bit_Length (Val.Result_Arity_Value) = Types.Bit_Length (Convert (BA_SINGLE))
               or (Types.Bit_Length (Ctx.Cursors (F_Result_Kind).Value.Result_Kind_Value) = Types.Bit_Length (Convert (BK_FD))
                 and Types.Bit_Length (Val.Result_Arity_Value) = Types.Bit_Length (Convert (BA_ARRAY))),
         when F_Result_Tag =>
            Types.Bit_Length (Ctx.Cursors (F_Result_Kind).Value.Result_Kind_Value) = Types.Bit_Length (Convert (BK_FD))
               or Types.Bit_Length (Ctx.Cursors (F_Result_Kind).Value.Result_Kind_Value) = Types.Bit_Length (Convert (BK_POINTER))
               or (Types.Bit_Length (Ctx.Cursors (F_Result_Kind).Value.Result_Kind_Value) /= Types.Bit_Length (Convert (BK_POINTER))
                 and Types.Bit_Length (Ctx.Cursors (F_Result_Kind).Value.Result_Kind_Value) /= Types.Bit_Length (Convert (BK_FD))),
         when F_Result_Legacy_Flags =>
            Types.Bit_Length (Ctx.Cursors (F_Result_Arity).Value.Result_Arity_Value) = Types.Bit_Length (Convert (BA_SINGLE))
               or Types.Bit_Length (Ctx.Cursors (F_Result_Arity).Value.Result_Arity_Value) = Types.Bit_Length (Convert (BA_ARRAY)),
         when F_Result_Has_Parent =>
            True,
         when F_Result_Flags =>
            Types.Bit_Length (Ctx.Cursors (F_Result_Kind).Value.Result_Kind_Value) = Types.Bit_Length (Convert (BK_STRONG_BINDER))
               or Types.Bit_Length (Ctx.Cursors (F_Result_Kind).Value.Result_Kind_Value) = Types.Bit_Length (Convert (BK_WEAK_BINDER))
               or Types.Bit_Length (Ctx.Cursors (F_Result_Kind).Value.Result_Kind_Value) = Types.Bit_Length (Convert (BK_STRONG_HANDLE))
               or Types.Bit_Length (Ctx.Cursors (F_Result_Kind).Value.Result_Kind_Value) = Types.Bit_Length (Convert (BK_WEAK_HANDLE)),
         when F_Result_FD | F_Result_Num_FDs | F_Result_Padding | F_Result_Binder | F_Result_Handle | F_Result_Parent | F_Result_Buffer | F_Result_Unused_Padding | F_Result_Parent_Offset =>
            True,
         when F_Result_Length =>
            Types.Bit_Length (Ctx.Cursors (F_Result_Has_Parent).Value.Result_Has_Parent_Value) = Types.Bit_Length (Convert (False))
               or Types.Bit_Length (Ctx.Cursors (F_Result_Has_Parent).Value.Result_Has_Parent_Value) = Types.Bit_Length (Convert (True)),
         when F_Result_Cookie | F_Result_Index =>
            True,
         when F_Final =>
            False));

   function Field_Length (Ctx : Context; Fld : Field) return Types.Bit_Length is
     ((case Ctx.Cursors (Fld).Predecessor is
         when F_Initial =>
            (case Fld is
                  when F_Result_Kind =>
                     Binder.Binder_Kind_Base'Size,
                  when others =>
                     Types.Unreachable_Bit_Length),
         when F_Result_Kind =>
            (case Fld is
                  when F_Result_Arity =>
                     Binder.Binder_Arity_Base'Size,
                  when others =>
                     Types.Unreachable_Bit_Length),
         when F_Result_Arity =>
            (case Fld is
                  when F_Result_Tag =>
                     Binder.Binder_Tag_Base'Size,
                  when others =>
                     Types.Unreachable_Bit_Length),
         when F_Result_Tag =>
            (case Fld is
                  when F_Result_Legacy_Flags =>
                     Binder.MBZ32_Base'Size,
                  when F_Result_Has_Parent =>
                     Builtin_Types.Boolean_Base'Size,
                  when F_Result_Flags =>
                     Binder.Flat_Binder_Flags_Base'Size,
                  when others =>
                     Types.Unreachable_Bit_Length),
         when F_Result_Legacy_Flags =>
            (case Fld is
                  when F_Result_FD =>
                     Binder.Handle_Base'Size,
                  when F_Result_Num_FDs =>
                     Binder.Count'Size,
                  when others =>
                     Types.Unreachable_Bit_Length),
         when F_Result_Has_Parent =>
            (case Fld is
                  when F_Result_Padding =>
                     Binder.MBZ31_Base'Size,
                  when others =>
                     Types.Unreachable_Bit_Length),
         when F_Result_Flags =>
            (case Fld is
                  when F_Result_Binder =>
                     Binder.Value'Size,
                  when F_Result_Handle =>
                     Binder.Handle_Base'Size,
                  when others =>
                     Types.Unreachable_Bit_Length),
         when F_Result_FD =>
            (case Fld is
                  when F_Result_Cookie =>
                     Binder.Cookie'Size,
                  when others =>
                     Types.Unreachable_Bit_Length),
         when F_Result_Num_FDs =>
            (case Fld is
                  when F_Result_Parent =>
                     Binder.Index'Size,
                  when others =>
                     Types.Unreachable_Bit_Length),
         when F_Result_Padding =>
            (case Fld is
                  when F_Result_Buffer =>
                     Binder.Index'Size,
                  when others =>
                     Types.Unreachable_Bit_Length),
         when F_Result_Binder =>
            (case Fld is
                  when F_Result_Cookie =>
                     Binder.Cookie'Size,
                  when others =>
                     Types.Unreachable_Bit_Length),
         when F_Result_Handle =>
            (case Fld is
                  when F_Result_Unused_Padding =>
                     Binder.MBZ32_Base'Size,
                  when others =>
                     Types.Unreachable_Bit_Length),
         when F_Result_Parent =>
            (case Fld is
                  when F_Result_Parent_Offset =>
                     Binder.Offset'Size,
                  when others =>
                     Types.Unreachable_Bit_Length),
         when F_Result_Buffer =>
            (case Fld is
                  when F_Result_Length =>
                     Binder.Length_Base'Size,
                  when others =>
                     Types.Unreachable_Bit_Length),
         when F_Result_Unused_Padding =>
            (case Fld is
                  when F_Result_Cookie =>
                     Binder.Cookie'Size,
                  when others =>
                     Types.Unreachable_Bit_Length),
         when F_Result_Parent_Offset =>
            0,
         when F_Result_Length =>
            (case Fld is
                  when F_Result_Index =>
                     Binder.Index'Size,
                  when others =>
                     Types.Unreachable_Bit_Length),
         when F_Result_Cookie | F_Result_Index | F_Final =>
            0));

   function Field_First (Ctx : Context; Fld : Field) return Types.Bit_Index is
     ((case Fld is
         when F_Result_Kind =>
            Ctx.First,
         when F_Result_Arity =>
            (if Ctx.Cursors (Fld).Predecessor = F_Result_Kind then
                (Ctx.Cursors (Ctx.Cursors (Fld).Predecessor).Last + 1)
             else
                Types.Unreachable_Bit_Length),
         when F_Result_Tag =>
            (if Ctx.Cursors (Fld).Predecessor = F_Result_Arity
                  and (Types.Bit_Length (Ctx.Cursors (F_Result_Arity).Value.Result_Arity_Value) = Types.Bit_Length (Convert (BA_SINGLE))
                    or (Types.Bit_Length (Ctx.Cursors (F_Result_Kind).Value.Result_Kind_Value) = Types.Bit_Length (Convert (BK_FD))
                      and Types.Bit_Length (Ctx.Cursors (F_Result_Arity).Value.Result_Arity_Value) = Types.Bit_Length (Convert (BA_ARRAY)))) then
                (Ctx.Cursors (Ctx.Cursors (Fld).Predecessor).Last + 1)
             else
                Types.Unreachable_Bit_Length),
         when F_Result_Legacy_Flags =>
            (if Ctx.Cursors (Fld).Predecessor = F_Result_Tag
                  and Types.Bit_Length (Ctx.Cursors (F_Result_Kind).Value.Result_Kind_Value) = Types.Bit_Length (Convert (BK_FD)) then
                (Ctx.Cursors (Ctx.Cursors (Fld).Predecessor).Last + 1)
             else
                Types.Unreachable_Bit_Length),
         when F_Result_Has_Parent =>
            (if Ctx.Cursors (Fld).Predecessor = F_Result_Tag
                  and Types.Bit_Length (Ctx.Cursors (F_Result_Kind).Value.Result_Kind_Value) = Types.Bit_Length (Convert (BK_POINTER)) then
                (Ctx.Cursors (Ctx.Cursors (Fld).Predecessor).Last + 1)
             else
                Types.Unreachable_Bit_Length),
         when F_Result_Flags =>
            (if Ctx.Cursors (Fld).Predecessor = F_Result_Tag
                  and Types.Bit_Length (Ctx.Cursors (F_Result_Kind).Value.Result_Kind_Value) /= Types.Bit_Length (Convert (BK_POINTER))
                  and Types.Bit_Length (Ctx.Cursors (F_Result_Kind).Value.Result_Kind_Value) /= Types.Bit_Length (Convert (BK_FD)) then
                (Ctx.Cursors (Ctx.Cursors (Fld).Predecessor).Last + 1)
             else
                Types.Unreachable_Bit_Length),
         when F_Result_FD =>
            (if Ctx.Cursors (Fld).Predecessor = F_Result_Legacy_Flags
                  and Types.Bit_Length (Ctx.Cursors (F_Result_Arity).Value.Result_Arity_Value) = Types.Bit_Length (Convert (BA_SINGLE)) then
                (Ctx.Cursors (Ctx.Cursors (Fld).Predecessor).Last + 1)
             else
                Types.Unreachable_Bit_Length),
         when F_Result_Num_FDs =>
            (if Ctx.Cursors (Fld).Predecessor = F_Result_Legacy_Flags
                  and Types.Bit_Length (Ctx.Cursors (F_Result_Arity).Value.Result_Arity_Value) = Types.Bit_Length (Convert (BA_ARRAY)) then
                (Ctx.Cursors (Ctx.Cursors (Fld).Predecessor).Last + 1)
             else
                Types.Unreachable_Bit_Length),
         when F_Result_Padding =>
            (if Ctx.Cursors (Fld).Predecessor = F_Result_Has_Parent then
                (Ctx.Cursors (Ctx.Cursors (Fld).Predecessor).Last + 1)
             else
                Types.Unreachable_Bit_Length),
         when F_Result_Binder =>
            (if Ctx.Cursors (Fld).Predecessor = F_Result_Flags
                  and (Types.Bit_Length (Ctx.Cursors (F_Result_Kind).Value.Result_Kind_Value) = Types.Bit_Length (Convert (BK_STRONG_BINDER))
                    or Types.Bit_Length (Ctx.Cursors (F_Result_Kind).Value.Result_Kind_Value) = Types.Bit_Length (Convert (BK_WEAK_BINDER))) then
                (Ctx.Cursors (Ctx.Cursors (Fld).Predecessor).Last + 1)
             else
                Types.Unreachable_Bit_Length),
         when F_Result_Handle =>
            (if Ctx.Cursors (Fld).Predecessor = F_Result_Flags
                  and (Types.Bit_Length (Ctx.Cursors (F_Result_Kind).Value.Result_Kind_Value) = Types.Bit_Length (Convert (BK_STRONG_HANDLE))
                    or Types.Bit_Length (Ctx.Cursors (F_Result_Kind).Value.Result_Kind_Value) = Types.Bit_Length (Convert (BK_WEAK_HANDLE))) then
                (Ctx.Cursors (Ctx.Cursors (Fld).Predecessor).Last + 1)
             else
                Types.Unreachable_Bit_Length),
         when F_Result_Parent =>
            (if Ctx.Cursors (Fld).Predecessor = F_Result_Num_FDs then
                (Ctx.Cursors (Ctx.Cursors (Fld).Predecessor).Last + 1)
             else
                Types.Unreachable_Bit_Length),
         when F_Result_Buffer =>
            (if Ctx.Cursors (Fld).Predecessor = F_Result_Padding then
                (Ctx.Cursors (Ctx.Cursors (Fld).Predecessor).Last + 1)
             else
                Types.Unreachable_Bit_Length),
         when F_Result_Unused_Padding =>
            (if Ctx.Cursors (Fld).Predecessor = F_Result_Handle then
                (Ctx.Cursors (Ctx.Cursors (Fld).Predecessor).Last + 1)
             else
                Types.Unreachable_Bit_Length),
         when F_Result_Parent_Offset =>
            (if Ctx.Cursors (Fld).Predecessor = F_Result_Parent then
                (Ctx.Cursors (Ctx.Cursors (Fld).Predecessor).Last + 1)
             else
                Types.Unreachable_Bit_Length),
         when F_Result_Length =>
            (if Ctx.Cursors (Fld).Predecessor = F_Result_Buffer then
                (Ctx.Cursors (Ctx.Cursors (Fld).Predecessor).Last + 1)
             else
                Types.Unreachable_Bit_Length),
         when F_Result_Cookie =>
            (if Ctx.Cursors (Fld).Predecessor = F_Result_FD then
                (Ctx.Cursors (Ctx.Cursors (Fld).Predecessor).Last + 1)
             elsif Ctx.Cursors (Fld).Predecessor = F_Result_Binder then
                (Ctx.Cursors (Ctx.Cursors (Fld).Predecessor).Last + 1)
             elsif Ctx.Cursors (Fld).Predecessor = F_Result_Unused_Padding then
                (Ctx.Cursors (Ctx.Cursors (Fld).Predecessor).Last + 1)
             else
                Types.Unreachable_Bit_Length),
         when F_Result_Index =>
            (if Ctx.Cursors (Fld).Predecessor = F_Result_Length
                  and Types.Bit_Length (Ctx.Cursors (F_Result_Has_Parent).Value.Result_Has_Parent_Value) = Types.Bit_Length (Convert (True)) then
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
         when F_Result_Kind =>
            F_Result_Arity,
         when F_Result_Arity =>
            (if Types.Bit_Length (Ctx.Cursors (F_Result_Arity).Value.Result_Arity_Value) = Types.Bit_Length (Convert (BA_SINGLE))
                  or (Types.Bit_Length (Ctx.Cursors (F_Result_Kind).Value.Result_Kind_Value) = Types.Bit_Length (Convert (BK_FD))
                    and Types.Bit_Length (Ctx.Cursors (F_Result_Arity).Value.Result_Arity_Value) = Types.Bit_Length (Convert (BA_ARRAY))) then
                F_Result_Tag
             else
                F_Initial),
         when F_Result_Tag =>
            (if Types.Bit_Length (Ctx.Cursors (F_Result_Kind).Value.Result_Kind_Value) = Types.Bit_Length (Convert (BK_FD)) then
                F_Result_Legacy_Flags
             elsif Types.Bit_Length (Ctx.Cursors (F_Result_Kind).Value.Result_Kind_Value) = Types.Bit_Length (Convert (BK_POINTER)) then
                F_Result_Has_Parent
             elsif Types.Bit_Length (Ctx.Cursors (F_Result_Kind).Value.Result_Kind_Value) /= Types.Bit_Length (Convert (BK_POINTER))
                     and Types.Bit_Length (Ctx.Cursors (F_Result_Kind).Value.Result_Kind_Value) /= Types.Bit_Length (Convert (BK_FD)) then
                F_Result_Flags
             else
                F_Initial),
         when F_Result_Legacy_Flags =>
            (if Types.Bit_Length (Ctx.Cursors (F_Result_Arity).Value.Result_Arity_Value) = Types.Bit_Length (Convert (BA_SINGLE)) then
                F_Result_FD
             elsif Types.Bit_Length (Ctx.Cursors (F_Result_Arity).Value.Result_Arity_Value) = Types.Bit_Length (Convert (BA_ARRAY)) then
                F_Result_Num_FDs
             else
                F_Initial),
         when F_Result_Has_Parent =>
            F_Result_Padding,
         when F_Result_Flags =>
            (if Types.Bit_Length (Ctx.Cursors (F_Result_Kind).Value.Result_Kind_Value) = Types.Bit_Length (Convert (BK_STRONG_BINDER))
                  or Types.Bit_Length (Ctx.Cursors (F_Result_Kind).Value.Result_Kind_Value) = Types.Bit_Length (Convert (BK_WEAK_BINDER)) then
                F_Result_Binder
             elsif Types.Bit_Length (Ctx.Cursors (F_Result_Kind).Value.Result_Kind_Value) = Types.Bit_Length (Convert (BK_STRONG_HANDLE))
                     or Types.Bit_Length (Ctx.Cursors (F_Result_Kind).Value.Result_Kind_Value) = Types.Bit_Length (Convert (BK_WEAK_HANDLE)) then
                F_Result_Handle
             else
                F_Initial),
         when F_Result_FD =>
            F_Result_Cookie,
         when F_Result_Num_FDs =>
            F_Result_Parent,
         when F_Result_Padding =>
            F_Result_Buffer,
         when F_Result_Binder =>
            F_Result_Cookie,
         when F_Result_Handle =>
            F_Result_Unused_Padding,
         when F_Result_Parent =>
            F_Result_Parent_Offset,
         when F_Result_Buffer =>
            F_Result_Length,
         when F_Result_Unused_Padding =>
            F_Result_Cookie,
         when F_Result_Parent_Offset =>
            F_Final,
         when F_Result_Length =>
            (if Types.Bit_Length (Ctx.Cursors (F_Result_Has_Parent).Value.Result_Has_Parent_Value) = Types.Bit_Length (Convert (False)) then
                F_Final
             elsif Types.Bit_Length (Ctx.Cursors (F_Result_Has_Parent).Value.Result_Has_Parent_Value) = Types.Bit_Length (Convert (True)) then
                F_Result_Index
             else
                F_Initial),
         when F_Result_Cookie | F_Result_Index =>
            F_Final))
    with
     Pre =>
       Structural_Valid (Ctx, Fld)
          and Valid_Predecessor (Ctx, Fld);

   function Valid_Predecessor (Ctx : Context; Fld : Virtual_Field) return Boolean is
     ((case Fld is
         when F_Initial =>
            True,
         when F_Result_Kind =>
            Ctx.Cursors (Fld).Predecessor = F_Initial,
         when F_Result_Arity =>
            (Valid (Ctx.Cursors (F_Result_Kind))
                 and Ctx.Cursors (Fld).Predecessor = F_Result_Kind),
         when F_Result_Tag =>
            (Valid (Ctx.Cursors (F_Result_Arity))
                 and Ctx.Cursors (Fld).Predecessor = F_Result_Arity),
         when F_Result_Legacy_Flags | F_Result_Has_Parent | F_Result_Flags =>
            (Valid (Ctx.Cursors (F_Result_Tag))
                 and Ctx.Cursors (Fld).Predecessor = F_Result_Tag),
         when F_Result_FD | F_Result_Num_FDs =>
            (Valid (Ctx.Cursors (F_Result_Legacy_Flags))
                 and Ctx.Cursors (Fld).Predecessor = F_Result_Legacy_Flags),
         when F_Result_Padding =>
            (Valid (Ctx.Cursors (F_Result_Has_Parent))
                 and Ctx.Cursors (Fld).Predecessor = F_Result_Has_Parent),
         when F_Result_Binder | F_Result_Handle =>
            (Valid (Ctx.Cursors (F_Result_Flags))
                 and Ctx.Cursors (Fld).Predecessor = F_Result_Flags),
         when F_Result_Parent =>
            (Valid (Ctx.Cursors (F_Result_Num_FDs))
                 and Ctx.Cursors (Fld).Predecessor = F_Result_Num_FDs),
         when F_Result_Buffer =>
            (Valid (Ctx.Cursors (F_Result_Padding))
                 and Ctx.Cursors (Fld).Predecessor = F_Result_Padding),
         when F_Result_Unused_Padding =>
            (Valid (Ctx.Cursors (F_Result_Handle))
                 and Ctx.Cursors (Fld).Predecessor = F_Result_Handle),
         when F_Result_Parent_Offset =>
            (Valid (Ctx.Cursors (F_Result_Parent))
                 and Ctx.Cursors (Fld).Predecessor = F_Result_Parent),
         when F_Result_Length =>
            (Valid (Ctx.Cursors (F_Result_Buffer))
                 and Ctx.Cursors (Fld).Predecessor = F_Result_Buffer),
         when F_Result_Cookie =>
            (Valid (Ctx.Cursors (F_Result_FD))
                 and Ctx.Cursors (Fld).Predecessor = F_Result_FD)
               or (Valid (Ctx.Cursors (F_Result_Binder))
                 and Ctx.Cursors (Fld).Predecessor = F_Result_Binder)
               or (Valid (Ctx.Cursors (F_Result_Unused_Padding))
                 and Ctx.Cursors (Fld).Predecessor = F_Result_Unused_Padding),
         when F_Result_Index =>
            (Valid (Ctx.Cursors (F_Result_Length))
                 and Ctx.Cursors (Fld).Predecessor = F_Result_Length),
         when F_Final =>
            (Valid (Ctx.Cursors (F_Result_Length))
                 and Ctx.Cursors (Fld).Predecessor = F_Result_Length)
               or (Valid (Ctx.Cursors (F_Result_Index))
                 and Ctx.Cursors (Fld).Predecessor = F_Result_Index)
               or (Valid (Ctx.Cursors (F_Result_Parent_Offset))
                 and Ctx.Cursors (Fld).Predecessor = F_Result_Parent_Offset)
               or (Valid (Ctx.Cursors (F_Result_Cookie))
                 and Ctx.Cursors (Fld).Predecessor = F_Result_Cookie)));

   function Invalid_Successor (Ctx : Context; Fld : Field) return Boolean is
     ((case Fld is
         when F_Result_Kind =>
            Invalid (Ctx.Cursors (F_Result_Arity)),
         when F_Result_Arity =>
            Invalid (Ctx.Cursors (F_Result_Tag)),
         when F_Result_Tag =>
            Invalid (Ctx.Cursors (F_Result_Legacy_Flags))
               and Invalid (Ctx.Cursors (F_Result_Has_Parent))
               and Invalid (Ctx.Cursors (F_Result_Flags)),
         when F_Result_Legacy_Flags =>
            Invalid (Ctx.Cursors (F_Result_FD))
               and Invalid (Ctx.Cursors (F_Result_Num_FDs)),
         when F_Result_Has_Parent =>
            Invalid (Ctx.Cursors (F_Result_Padding)),
         when F_Result_Flags =>
            Invalid (Ctx.Cursors (F_Result_Binder))
               and Invalid (Ctx.Cursors (F_Result_Handle)),
         when F_Result_FD =>
            Invalid (Ctx.Cursors (F_Result_Cookie)),
         when F_Result_Num_FDs =>
            Invalid (Ctx.Cursors (F_Result_Parent)),
         when F_Result_Padding =>
            Invalid (Ctx.Cursors (F_Result_Buffer)),
         when F_Result_Binder =>
            Invalid (Ctx.Cursors (F_Result_Cookie)),
         when F_Result_Handle =>
            Invalid (Ctx.Cursors (F_Result_Unused_Padding)),
         when F_Result_Parent =>
            Invalid (Ctx.Cursors (F_Result_Parent_Offset)),
         when F_Result_Buffer =>
            Invalid (Ctx.Cursors (F_Result_Length)),
         when F_Result_Unused_Padding =>
            Invalid (Ctx.Cursors (F_Result_Cookie)),
         when F_Result_Parent_Offset =>
            True,
         when F_Result_Length =>
            Invalid (Ctx.Cursors (F_Result_Index)),
         when F_Result_Cookie | F_Result_Index =>
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
               when F_Result_Kind =>
                  Invalid (Ctx, F_Result_Kind)
                     and Invalid (Ctx, F_Result_Arity)
                     and Invalid (Ctx, F_Result_Tag)
                     and Invalid (Ctx, F_Result_Legacy_Flags)
                     and Invalid (Ctx, F_Result_Has_Parent)
                     and Invalid (Ctx, F_Result_Flags)
                     and Invalid (Ctx, F_Result_FD)
                     and Invalid (Ctx, F_Result_Num_FDs)
                     and Invalid (Ctx, F_Result_Padding)
                     and Invalid (Ctx, F_Result_Binder)
                     and Invalid (Ctx, F_Result_Handle)
                     and Invalid (Ctx, F_Result_Parent)
                     and Invalid (Ctx, F_Result_Buffer)
                     and Invalid (Ctx, F_Result_Unused_Padding)
                     and Invalid (Ctx, F_Result_Parent_Offset)
                     and Invalid (Ctx, F_Result_Length)
                     and Invalid (Ctx, F_Result_Cookie)
                     and Invalid (Ctx, F_Result_Index),
               when F_Result_Arity =>
                  Ctx.Cursors (F_Result_Kind) = Ctx.Cursors (F_Result_Kind)'Old
                     and Invalid (Ctx, F_Result_Arity)
                     and Invalid (Ctx, F_Result_Tag)
                     and Invalid (Ctx, F_Result_Legacy_Flags)
                     and Invalid (Ctx, F_Result_Has_Parent)
                     and Invalid (Ctx, F_Result_Flags)
                     and Invalid (Ctx, F_Result_FD)
                     and Invalid (Ctx, F_Result_Num_FDs)
                     and Invalid (Ctx, F_Result_Padding)
                     and Invalid (Ctx, F_Result_Binder)
                     and Invalid (Ctx, F_Result_Handle)
                     and Invalid (Ctx, F_Result_Parent)
                     and Invalid (Ctx, F_Result_Buffer)
                     and Invalid (Ctx, F_Result_Unused_Padding)
                     and Invalid (Ctx, F_Result_Parent_Offset)
                     and Invalid (Ctx, F_Result_Length)
                     and Invalid (Ctx, F_Result_Cookie)
                     and Invalid (Ctx, F_Result_Index),
               when F_Result_Tag =>
                  Ctx.Cursors (F_Result_Kind) = Ctx.Cursors (F_Result_Kind)'Old
                     and Ctx.Cursors (F_Result_Arity) = Ctx.Cursors (F_Result_Arity)'Old
                     and Invalid (Ctx, F_Result_Tag)
                     and Invalid (Ctx, F_Result_Legacy_Flags)
                     and Invalid (Ctx, F_Result_Has_Parent)
                     and Invalid (Ctx, F_Result_Flags)
                     and Invalid (Ctx, F_Result_FD)
                     and Invalid (Ctx, F_Result_Num_FDs)
                     and Invalid (Ctx, F_Result_Padding)
                     and Invalid (Ctx, F_Result_Binder)
                     and Invalid (Ctx, F_Result_Handle)
                     and Invalid (Ctx, F_Result_Parent)
                     and Invalid (Ctx, F_Result_Buffer)
                     and Invalid (Ctx, F_Result_Unused_Padding)
                     and Invalid (Ctx, F_Result_Parent_Offset)
                     and Invalid (Ctx, F_Result_Length)
                     and Invalid (Ctx, F_Result_Cookie)
                     and Invalid (Ctx, F_Result_Index),
               when F_Result_Legacy_Flags =>
                  Ctx.Cursors (F_Result_Kind) = Ctx.Cursors (F_Result_Kind)'Old
                     and Ctx.Cursors (F_Result_Arity) = Ctx.Cursors (F_Result_Arity)'Old
                     and Ctx.Cursors (F_Result_Tag) = Ctx.Cursors (F_Result_Tag)'Old
                     and Invalid (Ctx, F_Result_Legacy_Flags)
                     and Invalid (Ctx, F_Result_Has_Parent)
                     and Invalid (Ctx, F_Result_Flags)
                     and Invalid (Ctx, F_Result_FD)
                     and Invalid (Ctx, F_Result_Num_FDs)
                     and Invalid (Ctx, F_Result_Padding)
                     and Invalid (Ctx, F_Result_Binder)
                     and Invalid (Ctx, F_Result_Handle)
                     and Invalid (Ctx, F_Result_Parent)
                     and Invalid (Ctx, F_Result_Buffer)
                     and Invalid (Ctx, F_Result_Unused_Padding)
                     and Invalid (Ctx, F_Result_Parent_Offset)
                     and Invalid (Ctx, F_Result_Length)
                     and Invalid (Ctx, F_Result_Cookie)
                     and Invalid (Ctx, F_Result_Index),
               when F_Result_Has_Parent =>
                  Ctx.Cursors (F_Result_Kind) = Ctx.Cursors (F_Result_Kind)'Old
                     and Ctx.Cursors (F_Result_Arity) = Ctx.Cursors (F_Result_Arity)'Old
                     and Ctx.Cursors (F_Result_Tag) = Ctx.Cursors (F_Result_Tag)'Old
                     and Ctx.Cursors (F_Result_Legacy_Flags) = Ctx.Cursors (F_Result_Legacy_Flags)'Old
                     and Invalid (Ctx, F_Result_Has_Parent)
                     and Invalid (Ctx, F_Result_Flags)
                     and Invalid (Ctx, F_Result_FD)
                     and Invalid (Ctx, F_Result_Num_FDs)
                     and Invalid (Ctx, F_Result_Padding)
                     and Invalid (Ctx, F_Result_Binder)
                     and Invalid (Ctx, F_Result_Handle)
                     and Invalid (Ctx, F_Result_Parent)
                     and Invalid (Ctx, F_Result_Buffer)
                     and Invalid (Ctx, F_Result_Unused_Padding)
                     and Invalid (Ctx, F_Result_Parent_Offset)
                     and Invalid (Ctx, F_Result_Length)
                     and Invalid (Ctx, F_Result_Cookie)
                     and Invalid (Ctx, F_Result_Index),
               when F_Result_Flags =>
                  Ctx.Cursors (F_Result_Kind) = Ctx.Cursors (F_Result_Kind)'Old
                     and Ctx.Cursors (F_Result_Arity) = Ctx.Cursors (F_Result_Arity)'Old
                     and Ctx.Cursors (F_Result_Tag) = Ctx.Cursors (F_Result_Tag)'Old
                     and Ctx.Cursors (F_Result_Legacy_Flags) = Ctx.Cursors (F_Result_Legacy_Flags)'Old
                     and Ctx.Cursors (F_Result_Has_Parent) = Ctx.Cursors (F_Result_Has_Parent)'Old
                     and Invalid (Ctx, F_Result_Flags)
                     and Invalid (Ctx, F_Result_FD)
                     and Invalid (Ctx, F_Result_Num_FDs)
                     and Invalid (Ctx, F_Result_Padding)
                     and Invalid (Ctx, F_Result_Binder)
                     and Invalid (Ctx, F_Result_Handle)
                     and Invalid (Ctx, F_Result_Parent)
                     and Invalid (Ctx, F_Result_Buffer)
                     and Invalid (Ctx, F_Result_Unused_Padding)
                     and Invalid (Ctx, F_Result_Parent_Offset)
                     and Invalid (Ctx, F_Result_Length)
                     and Invalid (Ctx, F_Result_Cookie)
                     and Invalid (Ctx, F_Result_Index),
               when F_Result_FD =>
                  Ctx.Cursors (F_Result_Kind) = Ctx.Cursors (F_Result_Kind)'Old
                     and Ctx.Cursors (F_Result_Arity) = Ctx.Cursors (F_Result_Arity)'Old
                     and Ctx.Cursors (F_Result_Tag) = Ctx.Cursors (F_Result_Tag)'Old
                     and Ctx.Cursors (F_Result_Legacy_Flags) = Ctx.Cursors (F_Result_Legacy_Flags)'Old
                     and Ctx.Cursors (F_Result_Has_Parent) = Ctx.Cursors (F_Result_Has_Parent)'Old
                     and Ctx.Cursors (F_Result_Flags) = Ctx.Cursors (F_Result_Flags)'Old
                     and Invalid (Ctx, F_Result_FD)
                     and Invalid (Ctx, F_Result_Num_FDs)
                     and Invalid (Ctx, F_Result_Padding)
                     and Invalid (Ctx, F_Result_Binder)
                     and Invalid (Ctx, F_Result_Handle)
                     and Invalid (Ctx, F_Result_Parent)
                     and Invalid (Ctx, F_Result_Buffer)
                     and Invalid (Ctx, F_Result_Unused_Padding)
                     and Invalid (Ctx, F_Result_Parent_Offset)
                     and Invalid (Ctx, F_Result_Length)
                     and Invalid (Ctx, F_Result_Cookie)
                     and Invalid (Ctx, F_Result_Index),
               when F_Result_Num_FDs =>
                  Ctx.Cursors (F_Result_Kind) = Ctx.Cursors (F_Result_Kind)'Old
                     and Ctx.Cursors (F_Result_Arity) = Ctx.Cursors (F_Result_Arity)'Old
                     and Ctx.Cursors (F_Result_Tag) = Ctx.Cursors (F_Result_Tag)'Old
                     and Ctx.Cursors (F_Result_Legacy_Flags) = Ctx.Cursors (F_Result_Legacy_Flags)'Old
                     and Ctx.Cursors (F_Result_Has_Parent) = Ctx.Cursors (F_Result_Has_Parent)'Old
                     and Ctx.Cursors (F_Result_Flags) = Ctx.Cursors (F_Result_Flags)'Old
                     and Ctx.Cursors (F_Result_FD) = Ctx.Cursors (F_Result_FD)'Old
                     and Invalid (Ctx, F_Result_Num_FDs)
                     and Invalid (Ctx, F_Result_Padding)
                     and Invalid (Ctx, F_Result_Binder)
                     and Invalid (Ctx, F_Result_Handle)
                     and Invalid (Ctx, F_Result_Parent)
                     and Invalid (Ctx, F_Result_Buffer)
                     and Invalid (Ctx, F_Result_Unused_Padding)
                     and Invalid (Ctx, F_Result_Parent_Offset)
                     and Invalid (Ctx, F_Result_Length)
                     and Invalid (Ctx, F_Result_Cookie)
                     and Invalid (Ctx, F_Result_Index),
               when F_Result_Padding =>
                  Ctx.Cursors (F_Result_Kind) = Ctx.Cursors (F_Result_Kind)'Old
                     and Ctx.Cursors (F_Result_Arity) = Ctx.Cursors (F_Result_Arity)'Old
                     and Ctx.Cursors (F_Result_Tag) = Ctx.Cursors (F_Result_Tag)'Old
                     and Ctx.Cursors (F_Result_Legacy_Flags) = Ctx.Cursors (F_Result_Legacy_Flags)'Old
                     and Ctx.Cursors (F_Result_Has_Parent) = Ctx.Cursors (F_Result_Has_Parent)'Old
                     and Ctx.Cursors (F_Result_Flags) = Ctx.Cursors (F_Result_Flags)'Old
                     and Ctx.Cursors (F_Result_FD) = Ctx.Cursors (F_Result_FD)'Old
                     and Ctx.Cursors (F_Result_Num_FDs) = Ctx.Cursors (F_Result_Num_FDs)'Old
                     and Invalid (Ctx, F_Result_Padding)
                     and Invalid (Ctx, F_Result_Binder)
                     and Invalid (Ctx, F_Result_Handle)
                     and Invalid (Ctx, F_Result_Parent)
                     and Invalid (Ctx, F_Result_Buffer)
                     and Invalid (Ctx, F_Result_Unused_Padding)
                     and Invalid (Ctx, F_Result_Parent_Offset)
                     and Invalid (Ctx, F_Result_Length)
                     and Invalid (Ctx, F_Result_Cookie)
                     and Invalid (Ctx, F_Result_Index),
               when F_Result_Binder =>
                  Ctx.Cursors (F_Result_Kind) = Ctx.Cursors (F_Result_Kind)'Old
                     and Ctx.Cursors (F_Result_Arity) = Ctx.Cursors (F_Result_Arity)'Old
                     and Ctx.Cursors (F_Result_Tag) = Ctx.Cursors (F_Result_Tag)'Old
                     and Ctx.Cursors (F_Result_Legacy_Flags) = Ctx.Cursors (F_Result_Legacy_Flags)'Old
                     and Ctx.Cursors (F_Result_Has_Parent) = Ctx.Cursors (F_Result_Has_Parent)'Old
                     and Ctx.Cursors (F_Result_Flags) = Ctx.Cursors (F_Result_Flags)'Old
                     and Ctx.Cursors (F_Result_FD) = Ctx.Cursors (F_Result_FD)'Old
                     and Ctx.Cursors (F_Result_Num_FDs) = Ctx.Cursors (F_Result_Num_FDs)'Old
                     and Ctx.Cursors (F_Result_Padding) = Ctx.Cursors (F_Result_Padding)'Old
                     and Invalid (Ctx, F_Result_Binder)
                     and Invalid (Ctx, F_Result_Handle)
                     and Invalid (Ctx, F_Result_Parent)
                     and Invalid (Ctx, F_Result_Buffer)
                     and Invalid (Ctx, F_Result_Unused_Padding)
                     and Invalid (Ctx, F_Result_Parent_Offset)
                     and Invalid (Ctx, F_Result_Length)
                     and Invalid (Ctx, F_Result_Cookie)
                     and Invalid (Ctx, F_Result_Index),
               when F_Result_Handle =>
                  Ctx.Cursors (F_Result_Kind) = Ctx.Cursors (F_Result_Kind)'Old
                     and Ctx.Cursors (F_Result_Arity) = Ctx.Cursors (F_Result_Arity)'Old
                     and Ctx.Cursors (F_Result_Tag) = Ctx.Cursors (F_Result_Tag)'Old
                     and Ctx.Cursors (F_Result_Legacy_Flags) = Ctx.Cursors (F_Result_Legacy_Flags)'Old
                     and Ctx.Cursors (F_Result_Has_Parent) = Ctx.Cursors (F_Result_Has_Parent)'Old
                     and Ctx.Cursors (F_Result_Flags) = Ctx.Cursors (F_Result_Flags)'Old
                     and Ctx.Cursors (F_Result_FD) = Ctx.Cursors (F_Result_FD)'Old
                     and Ctx.Cursors (F_Result_Num_FDs) = Ctx.Cursors (F_Result_Num_FDs)'Old
                     and Ctx.Cursors (F_Result_Padding) = Ctx.Cursors (F_Result_Padding)'Old
                     and Ctx.Cursors (F_Result_Binder) = Ctx.Cursors (F_Result_Binder)'Old
                     and Invalid (Ctx, F_Result_Handle)
                     and Invalid (Ctx, F_Result_Parent)
                     and Invalid (Ctx, F_Result_Buffer)
                     and Invalid (Ctx, F_Result_Unused_Padding)
                     and Invalid (Ctx, F_Result_Parent_Offset)
                     and Invalid (Ctx, F_Result_Length)
                     and Invalid (Ctx, F_Result_Cookie)
                     and Invalid (Ctx, F_Result_Index),
               when F_Result_Parent =>
                  Ctx.Cursors (F_Result_Kind) = Ctx.Cursors (F_Result_Kind)'Old
                     and Ctx.Cursors (F_Result_Arity) = Ctx.Cursors (F_Result_Arity)'Old
                     and Ctx.Cursors (F_Result_Tag) = Ctx.Cursors (F_Result_Tag)'Old
                     and Ctx.Cursors (F_Result_Legacy_Flags) = Ctx.Cursors (F_Result_Legacy_Flags)'Old
                     and Ctx.Cursors (F_Result_Has_Parent) = Ctx.Cursors (F_Result_Has_Parent)'Old
                     and Ctx.Cursors (F_Result_Flags) = Ctx.Cursors (F_Result_Flags)'Old
                     and Ctx.Cursors (F_Result_FD) = Ctx.Cursors (F_Result_FD)'Old
                     and Ctx.Cursors (F_Result_Num_FDs) = Ctx.Cursors (F_Result_Num_FDs)'Old
                     and Ctx.Cursors (F_Result_Padding) = Ctx.Cursors (F_Result_Padding)'Old
                     and Ctx.Cursors (F_Result_Binder) = Ctx.Cursors (F_Result_Binder)'Old
                     and Ctx.Cursors (F_Result_Handle) = Ctx.Cursors (F_Result_Handle)'Old
                     and Invalid (Ctx, F_Result_Parent)
                     and Invalid (Ctx, F_Result_Buffer)
                     and Invalid (Ctx, F_Result_Unused_Padding)
                     and Invalid (Ctx, F_Result_Parent_Offset)
                     and Invalid (Ctx, F_Result_Length)
                     and Invalid (Ctx, F_Result_Cookie)
                     and Invalid (Ctx, F_Result_Index),
               when F_Result_Buffer =>
                  Ctx.Cursors (F_Result_Kind) = Ctx.Cursors (F_Result_Kind)'Old
                     and Ctx.Cursors (F_Result_Arity) = Ctx.Cursors (F_Result_Arity)'Old
                     and Ctx.Cursors (F_Result_Tag) = Ctx.Cursors (F_Result_Tag)'Old
                     and Ctx.Cursors (F_Result_Legacy_Flags) = Ctx.Cursors (F_Result_Legacy_Flags)'Old
                     and Ctx.Cursors (F_Result_Has_Parent) = Ctx.Cursors (F_Result_Has_Parent)'Old
                     and Ctx.Cursors (F_Result_Flags) = Ctx.Cursors (F_Result_Flags)'Old
                     and Ctx.Cursors (F_Result_FD) = Ctx.Cursors (F_Result_FD)'Old
                     and Ctx.Cursors (F_Result_Num_FDs) = Ctx.Cursors (F_Result_Num_FDs)'Old
                     and Ctx.Cursors (F_Result_Padding) = Ctx.Cursors (F_Result_Padding)'Old
                     and Ctx.Cursors (F_Result_Binder) = Ctx.Cursors (F_Result_Binder)'Old
                     and Ctx.Cursors (F_Result_Handle) = Ctx.Cursors (F_Result_Handle)'Old
                     and Ctx.Cursors (F_Result_Parent) = Ctx.Cursors (F_Result_Parent)'Old
                     and Invalid (Ctx, F_Result_Buffer)
                     and Invalid (Ctx, F_Result_Unused_Padding)
                     and Invalid (Ctx, F_Result_Parent_Offset)
                     and Invalid (Ctx, F_Result_Length)
                     and Invalid (Ctx, F_Result_Cookie)
                     and Invalid (Ctx, F_Result_Index),
               when F_Result_Unused_Padding =>
                  Ctx.Cursors (F_Result_Kind) = Ctx.Cursors (F_Result_Kind)'Old
                     and Ctx.Cursors (F_Result_Arity) = Ctx.Cursors (F_Result_Arity)'Old
                     and Ctx.Cursors (F_Result_Tag) = Ctx.Cursors (F_Result_Tag)'Old
                     and Ctx.Cursors (F_Result_Legacy_Flags) = Ctx.Cursors (F_Result_Legacy_Flags)'Old
                     and Ctx.Cursors (F_Result_Has_Parent) = Ctx.Cursors (F_Result_Has_Parent)'Old
                     and Ctx.Cursors (F_Result_Flags) = Ctx.Cursors (F_Result_Flags)'Old
                     and Ctx.Cursors (F_Result_FD) = Ctx.Cursors (F_Result_FD)'Old
                     and Ctx.Cursors (F_Result_Num_FDs) = Ctx.Cursors (F_Result_Num_FDs)'Old
                     and Ctx.Cursors (F_Result_Padding) = Ctx.Cursors (F_Result_Padding)'Old
                     and Ctx.Cursors (F_Result_Binder) = Ctx.Cursors (F_Result_Binder)'Old
                     and Ctx.Cursors (F_Result_Handle) = Ctx.Cursors (F_Result_Handle)'Old
                     and Ctx.Cursors (F_Result_Parent) = Ctx.Cursors (F_Result_Parent)'Old
                     and Ctx.Cursors (F_Result_Buffer) = Ctx.Cursors (F_Result_Buffer)'Old
                     and Invalid (Ctx, F_Result_Unused_Padding)
                     and Invalid (Ctx, F_Result_Parent_Offset)
                     and Invalid (Ctx, F_Result_Length)
                     and Invalid (Ctx, F_Result_Cookie)
                     and Invalid (Ctx, F_Result_Index),
               when F_Result_Parent_Offset =>
                  Ctx.Cursors (F_Result_Kind) = Ctx.Cursors (F_Result_Kind)'Old
                     and Ctx.Cursors (F_Result_Arity) = Ctx.Cursors (F_Result_Arity)'Old
                     and Ctx.Cursors (F_Result_Tag) = Ctx.Cursors (F_Result_Tag)'Old
                     and Ctx.Cursors (F_Result_Legacy_Flags) = Ctx.Cursors (F_Result_Legacy_Flags)'Old
                     and Ctx.Cursors (F_Result_Has_Parent) = Ctx.Cursors (F_Result_Has_Parent)'Old
                     and Ctx.Cursors (F_Result_Flags) = Ctx.Cursors (F_Result_Flags)'Old
                     and Ctx.Cursors (F_Result_FD) = Ctx.Cursors (F_Result_FD)'Old
                     and Ctx.Cursors (F_Result_Num_FDs) = Ctx.Cursors (F_Result_Num_FDs)'Old
                     and Ctx.Cursors (F_Result_Padding) = Ctx.Cursors (F_Result_Padding)'Old
                     and Ctx.Cursors (F_Result_Binder) = Ctx.Cursors (F_Result_Binder)'Old
                     and Ctx.Cursors (F_Result_Handle) = Ctx.Cursors (F_Result_Handle)'Old
                     and Ctx.Cursors (F_Result_Parent) = Ctx.Cursors (F_Result_Parent)'Old
                     and Ctx.Cursors (F_Result_Buffer) = Ctx.Cursors (F_Result_Buffer)'Old
                     and Ctx.Cursors (F_Result_Unused_Padding) = Ctx.Cursors (F_Result_Unused_Padding)'Old
                     and Invalid (Ctx, F_Result_Parent_Offset)
                     and Invalid (Ctx, F_Result_Length)
                     and Invalid (Ctx, F_Result_Cookie)
                     and Invalid (Ctx, F_Result_Index),
               when F_Result_Length =>
                  Ctx.Cursors (F_Result_Kind) = Ctx.Cursors (F_Result_Kind)'Old
                     and Ctx.Cursors (F_Result_Arity) = Ctx.Cursors (F_Result_Arity)'Old
                     and Ctx.Cursors (F_Result_Tag) = Ctx.Cursors (F_Result_Tag)'Old
                     and Ctx.Cursors (F_Result_Legacy_Flags) = Ctx.Cursors (F_Result_Legacy_Flags)'Old
                     and Ctx.Cursors (F_Result_Has_Parent) = Ctx.Cursors (F_Result_Has_Parent)'Old
                     and Ctx.Cursors (F_Result_Flags) = Ctx.Cursors (F_Result_Flags)'Old
                     and Ctx.Cursors (F_Result_FD) = Ctx.Cursors (F_Result_FD)'Old
                     and Ctx.Cursors (F_Result_Num_FDs) = Ctx.Cursors (F_Result_Num_FDs)'Old
                     and Ctx.Cursors (F_Result_Padding) = Ctx.Cursors (F_Result_Padding)'Old
                     and Ctx.Cursors (F_Result_Binder) = Ctx.Cursors (F_Result_Binder)'Old
                     and Ctx.Cursors (F_Result_Handle) = Ctx.Cursors (F_Result_Handle)'Old
                     and Ctx.Cursors (F_Result_Parent) = Ctx.Cursors (F_Result_Parent)'Old
                     and Ctx.Cursors (F_Result_Buffer) = Ctx.Cursors (F_Result_Buffer)'Old
                     and Ctx.Cursors (F_Result_Unused_Padding) = Ctx.Cursors (F_Result_Unused_Padding)'Old
                     and Ctx.Cursors (F_Result_Parent_Offset) = Ctx.Cursors (F_Result_Parent_Offset)'Old
                     and Invalid (Ctx, F_Result_Length)
                     and Invalid (Ctx, F_Result_Cookie)
                     and Invalid (Ctx, F_Result_Index),
               when F_Result_Cookie =>
                  Ctx.Cursors (F_Result_Kind) = Ctx.Cursors (F_Result_Kind)'Old
                     and Ctx.Cursors (F_Result_Arity) = Ctx.Cursors (F_Result_Arity)'Old
                     and Ctx.Cursors (F_Result_Tag) = Ctx.Cursors (F_Result_Tag)'Old
                     and Ctx.Cursors (F_Result_Legacy_Flags) = Ctx.Cursors (F_Result_Legacy_Flags)'Old
                     and Ctx.Cursors (F_Result_Has_Parent) = Ctx.Cursors (F_Result_Has_Parent)'Old
                     and Ctx.Cursors (F_Result_Flags) = Ctx.Cursors (F_Result_Flags)'Old
                     and Ctx.Cursors (F_Result_FD) = Ctx.Cursors (F_Result_FD)'Old
                     and Ctx.Cursors (F_Result_Num_FDs) = Ctx.Cursors (F_Result_Num_FDs)'Old
                     and Ctx.Cursors (F_Result_Padding) = Ctx.Cursors (F_Result_Padding)'Old
                     and Ctx.Cursors (F_Result_Binder) = Ctx.Cursors (F_Result_Binder)'Old
                     and Ctx.Cursors (F_Result_Handle) = Ctx.Cursors (F_Result_Handle)'Old
                     and Ctx.Cursors (F_Result_Parent) = Ctx.Cursors (F_Result_Parent)'Old
                     and Ctx.Cursors (F_Result_Buffer) = Ctx.Cursors (F_Result_Buffer)'Old
                     and Ctx.Cursors (F_Result_Unused_Padding) = Ctx.Cursors (F_Result_Unused_Padding)'Old
                     and Ctx.Cursors (F_Result_Parent_Offset) = Ctx.Cursors (F_Result_Parent_Offset)'Old
                     and Ctx.Cursors (F_Result_Length) = Ctx.Cursors (F_Result_Length)'Old
                     and Invalid (Ctx, F_Result_Cookie)
                     and Invalid (Ctx, F_Result_Index),
               when F_Result_Index =>
                  Ctx.Cursors (F_Result_Kind) = Ctx.Cursors (F_Result_Kind)'Old
                     and Ctx.Cursors (F_Result_Arity) = Ctx.Cursors (F_Result_Arity)'Old
                     and Ctx.Cursors (F_Result_Tag) = Ctx.Cursors (F_Result_Tag)'Old
                     and Ctx.Cursors (F_Result_Legacy_Flags) = Ctx.Cursors (F_Result_Legacy_Flags)'Old
                     and Ctx.Cursors (F_Result_Has_Parent) = Ctx.Cursors (F_Result_Has_Parent)'Old
                     and Ctx.Cursors (F_Result_Flags) = Ctx.Cursors (F_Result_Flags)'Old
                     and Ctx.Cursors (F_Result_FD) = Ctx.Cursors (F_Result_FD)'Old
                     and Ctx.Cursors (F_Result_Num_FDs) = Ctx.Cursors (F_Result_Num_FDs)'Old
                     and Ctx.Cursors (F_Result_Padding) = Ctx.Cursors (F_Result_Padding)'Old
                     and Ctx.Cursors (F_Result_Binder) = Ctx.Cursors (F_Result_Binder)'Old
                     and Ctx.Cursors (F_Result_Handle) = Ctx.Cursors (F_Result_Handle)'Old
                     and Ctx.Cursors (F_Result_Parent) = Ctx.Cursors (F_Result_Parent)'Old
                     and Ctx.Cursors (F_Result_Buffer) = Ctx.Cursors (F_Result_Buffer)'Old
                     and Ctx.Cursors (F_Result_Unused_Padding) = Ctx.Cursors (F_Result_Unused_Padding)'Old
                     and Ctx.Cursors (F_Result_Parent_Offset) = Ctx.Cursors (F_Result_Parent_Offset)'Old
                     and Ctx.Cursors (F_Result_Length) = Ctx.Cursors (F_Result_Length)'Old
                     and Ctx.Cursors (F_Result_Cookie) = Ctx.Cursors (F_Result_Cookie)'Old
                     and Invalid (Ctx, F_Result_Index))
   is
      First : constant Types.Bit_Length := Field_First (Ctx, Fld) with
        Ghost;
      Length : constant Types.Bit_Length := Field_Length (Ctx, Fld) with
        Ghost;
   begin
      pragma Assert (Field_First (Ctx, Fld) = First
         and Field_Length (Ctx, Fld) = Length);
      case Fld is
         when F_Result_Kind =>
            Ctx.Cursors (F_Result_Index) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Result_Cookie) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Result_Length) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Result_Parent_Offset) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Result_Unused_Padding) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Result_Buffer) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Result_Parent) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Result_Handle) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Result_Binder) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Result_Padding) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Result_Num_FDs) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Result_FD) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Result_Flags) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Result_Has_Parent) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Result_Legacy_Flags) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Result_Tag) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Result_Arity) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Result_Kind) := (S_Invalid, Ctx.Cursors (F_Result_Kind).Predecessor);
            pragma Assert (Field_First (Ctx, Fld) = First
               and Field_Length (Ctx, Fld) = Length);
         when F_Result_Arity =>
            Ctx.Cursors (F_Result_Index) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Result_Cookie) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Result_Length) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Result_Parent_Offset) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Result_Unused_Padding) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Result_Buffer) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Result_Parent) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Result_Handle) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Result_Binder) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Result_Padding) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Result_Num_FDs) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Result_FD) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Result_Flags) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Result_Has_Parent) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Result_Legacy_Flags) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Result_Tag) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Result_Arity) := (S_Invalid, Ctx.Cursors (F_Result_Arity).Predecessor);
            pragma Assert (Field_First (Ctx, Fld) = First
               and Field_Length (Ctx, Fld) = Length);
         when F_Result_Tag =>
            Ctx.Cursors (F_Result_Index) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Result_Cookie) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Result_Length) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Result_Parent_Offset) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Result_Unused_Padding) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Result_Buffer) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Result_Parent) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Result_Handle) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Result_Binder) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Result_Padding) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Result_Num_FDs) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Result_FD) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Result_Flags) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Result_Has_Parent) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Result_Legacy_Flags) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Result_Tag) := (S_Invalid, Ctx.Cursors (F_Result_Tag).Predecessor);
            pragma Assert (Field_First (Ctx, Fld) = First
               and Field_Length (Ctx, Fld) = Length);
         when F_Result_Legacy_Flags =>
            Ctx.Cursors (F_Result_Index) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Result_Cookie) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Result_Length) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Result_Parent_Offset) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Result_Unused_Padding) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Result_Buffer) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Result_Parent) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Result_Handle) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Result_Binder) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Result_Padding) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Result_Num_FDs) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Result_FD) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Result_Flags) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Result_Has_Parent) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Result_Legacy_Flags) := (S_Invalid, Ctx.Cursors (F_Result_Legacy_Flags).Predecessor);
            pragma Assert (Field_First (Ctx, Fld) = First
               and Field_Length (Ctx, Fld) = Length);
         when F_Result_Has_Parent =>
            Ctx.Cursors (F_Result_Index) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Result_Cookie) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Result_Length) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Result_Parent_Offset) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Result_Unused_Padding) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Result_Buffer) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Result_Parent) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Result_Handle) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Result_Binder) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Result_Padding) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Result_Num_FDs) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Result_FD) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Result_Flags) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Result_Has_Parent) := (S_Invalid, Ctx.Cursors (F_Result_Has_Parent).Predecessor);
            pragma Assert (Field_First (Ctx, Fld) = First
               and Field_Length (Ctx, Fld) = Length);
         when F_Result_Flags =>
            Ctx.Cursors (F_Result_Index) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Result_Cookie) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Result_Length) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Result_Parent_Offset) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Result_Unused_Padding) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Result_Buffer) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Result_Parent) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Result_Handle) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Result_Binder) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Result_Padding) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Result_Num_FDs) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Result_FD) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Result_Flags) := (S_Invalid, Ctx.Cursors (F_Result_Flags).Predecessor);
            pragma Assert (Field_First (Ctx, Fld) = First
               and Field_Length (Ctx, Fld) = Length);
         when F_Result_FD =>
            Ctx.Cursors (F_Result_Index) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Result_Cookie) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Result_Length) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Result_Parent_Offset) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Result_Unused_Padding) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Result_Buffer) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Result_Parent) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Result_Handle) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Result_Binder) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Result_Padding) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Result_Num_FDs) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Result_FD) := (S_Invalid, Ctx.Cursors (F_Result_FD).Predecessor);
            pragma Assert (Field_First (Ctx, Fld) = First
               and Field_Length (Ctx, Fld) = Length);
         when F_Result_Num_FDs =>
            Ctx.Cursors (F_Result_Index) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Result_Cookie) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Result_Length) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Result_Parent_Offset) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Result_Unused_Padding) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Result_Buffer) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Result_Parent) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Result_Handle) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Result_Binder) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Result_Padding) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Result_Num_FDs) := (S_Invalid, Ctx.Cursors (F_Result_Num_FDs).Predecessor);
            pragma Assert (Field_First (Ctx, Fld) = First
               and Field_Length (Ctx, Fld) = Length);
         when F_Result_Padding =>
            Ctx.Cursors (F_Result_Index) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Result_Cookie) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Result_Length) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Result_Parent_Offset) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Result_Unused_Padding) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Result_Buffer) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Result_Parent) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Result_Handle) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Result_Binder) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Result_Padding) := (S_Invalid, Ctx.Cursors (F_Result_Padding).Predecessor);
            pragma Assert (Field_First (Ctx, Fld) = First
               and Field_Length (Ctx, Fld) = Length);
         when F_Result_Binder =>
            Ctx.Cursors (F_Result_Index) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Result_Cookie) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Result_Length) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Result_Parent_Offset) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Result_Unused_Padding) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Result_Buffer) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Result_Parent) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Result_Handle) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Result_Binder) := (S_Invalid, Ctx.Cursors (F_Result_Binder).Predecessor);
            pragma Assert (Field_First (Ctx, Fld) = First
               and Field_Length (Ctx, Fld) = Length);
         when F_Result_Handle =>
            Ctx.Cursors (F_Result_Index) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Result_Cookie) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Result_Length) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Result_Parent_Offset) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Result_Unused_Padding) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Result_Buffer) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Result_Parent) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Result_Handle) := (S_Invalid, Ctx.Cursors (F_Result_Handle).Predecessor);
            pragma Assert (Field_First (Ctx, Fld) = First
               and Field_Length (Ctx, Fld) = Length);
         when F_Result_Parent =>
            Ctx.Cursors (F_Result_Index) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Result_Cookie) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Result_Length) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Result_Parent_Offset) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Result_Unused_Padding) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Result_Buffer) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Result_Parent) := (S_Invalid, Ctx.Cursors (F_Result_Parent).Predecessor);
            pragma Assert (Field_First (Ctx, Fld) = First
               and Field_Length (Ctx, Fld) = Length);
         when F_Result_Buffer =>
            Ctx.Cursors (F_Result_Index) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Result_Cookie) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Result_Length) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Result_Parent_Offset) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Result_Unused_Padding) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Result_Buffer) := (S_Invalid, Ctx.Cursors (F_Result_Buffer).Predecessor);
            pragma Assert (Field_First (Ctx, Fld) = First
               and Field_Length (Ctx, Fld) = Length);
         when F_Result_Unused_Padding =>
            Ctx.Cursors (F_Result_Index) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Result_Cookie) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Result_Length) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Result_Parent_Offset) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Result_Unused_Padding) := (S_Invalid, Ctx.Cursors (F_Result_Unused_Padding).Predecessor);
            pragma Assert (Field_First (Ctx, Fld) = First
               and Field_Length (Ctx, Fld) = Length);
         when F_Result_Parent_Offset =>
            Ctx.Cursors (F_Result_Index) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Result_Cookie) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Result_Length) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Result_Parent_Offset) := (S_Invalid, Ctx.Cursors (F_Result_Parent_Offset).Predecessor);
            pragma Assert (Field_First (Ctx, Fld) = First
               and Field_Length (Ctx, Fld) = Length);
         when F_Result_Length =>
            Ctx.Cursors (F_Result_Index) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Result_Cookie) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Result_Length) := (S_Invalid, Ctx.Cursors (F_Result_Length).Predecessor);
            pragma Assert (Field_First (Ctx, Fld) = First
               and Field_Length (Ctx, Fld) = Length);
         when F_Result_Cookie =>
            Ctx.Cursors (F_Result_Index) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Result_Cookie) := (S_Invalid, Ctx.Cursors (F_Result_Cookie).Predecessor);
            pragma Assert (Field_First (Ctx, Fld) = First
               and Field_Length (Ctx, Fld) = Length);
         when F_Result_Index =>
            Ctx.Cursors (F_Result_Index) := (S_Invalid, Ctx.Cursors (F_Result_Index).Predecessor);
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
         when F_Result_Kind | F_Result_Arity | F_Result_Tag | F_Result_Legacy_Flags | F_Result_Has_Parent | F_Result_Flags | F_Result_FD | F_Result_Num_FDs | F_Result_Padding | F_Result_Binder | F_Result_Handle | F_Result_Parent | F_Result_Buffer | F_Result_Unused_Padding | F_Result_Parent_Offset | F_Result_Length | F_Result_Cookie | F_Result_Index =>
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
      function Extract is new Types.Extract (Binder.Binder_Kind_Base);
      function Extract is new Types.Extract (Binder.Binder_Arity_Base);
      function Extract is new Types.Extract (Binder.Binder_Tag_Base);
      function Extract is new Types.Extract (Binder.MBZ32_Base);
      function Extract is new Types.Extract (Builtin_Types.Boolean_Base);
      function Extract is new Types.Extract (Binder.Flat_Binder_Flags_Base);
      function Extract is new Types.Extract (Binder.Handle_Base);
      function Extract is new Types.Extract (Binder.Count);
      function Extract is new Types.Extract (Binder.MBZ31_Base);
      function Extract is new Types.Extract (Binder.Value);
      function Extract is new Types.Extract (Binder.Index);
      function Extract is new Types.Extract (Binder.Offset);
      function Extract is new Types.Extract (Binder.Length_Base);
      function Extract is new Types.Extract (Binder.Cookie);
   begin
      return ((case Fld is
            when F_Result_Kind =>
               (Fld => F_Result_Kind, Result_Kind_Value => Extract (Ctx.Buffer.all (Buffer_First .. Buffer_Last), Offset)),
            when F_Result_Arity =>
               (Fld => F_Result_Arity, Result_Arity_Value => Extract (Ctx.Buffer.all (Buffer_First .. Buffer_Last), Offset)),
            when F_Result_Tag =>
               (Fld => F_Result_Tag, Result_Tag_Value => Extract (Ctx.Buffer.all (Buffer_First .. Buffer_Last), Offset)),
            when F_Result_Legacy_Flags =>
               (Fld => F_Result_Legacy_Flags, Result_Legacy_Flags_Value => Extract (Ctx.Buffer.all (Buffer_First .. Buffer_Last), Offset)),
            when F_Result_Has_Parent =>
               (Fld => F_Result_Has_Parent, Result_Has_Parent_Value => Extract (Ctx.Buffer.all (Buffer_First .. Buffer_Last), Offset)),
            when F_Result_Flags =>
               (Fld => F_Result_Flags, Result_Flags_Value => Extract (Ctx.Buffer.all (Buffer_First .. Buffer_Last), Offset)),
            when F_Result_FD =>
               (Fld => F_Result_FD, Result_FD_Value => Extract (Ctx.Buffer.all (Buffer_First .. Buffer_Last), Offset)),
            when F_Result_Num_FDs =>
               (Fld => F_Result_Num_FDs, Result_Num_FDs_Value => Extract (Ctx.Buffer.all (Buffer_First .. Buffer_Last), Offset)),
            when F_Result_Padding =>
               (Fld => F_Result_Padding, Result_Padding_Value => Extract (Ctx.Buffer.all (Buffer_First .. Buffer_Last), Offset)),
            when F_Result_Binder =>
               (Fld => F_Result_Binder, Result_Binder_Value => Extract (Ctx.Buffer.all (Buffer_First .. Buffer_Last), Offset)),
            when F_Result_Handle =>
               (Fld => F_Result_Handle, Result_Handle_Value => Extract (Ctx.Buffer.all (Buffer_First .. Buffer_Last), Offset)),
            when F_Result_Parent =>
               (Fld => F_Result_Parent, Result_Parent_Value => Extract (Ctx.Buffer.all (Buffer_First .. Buffer_Last), Offset)),
            when F_Result_Buffer =>
               (Fld => F_Result_Buffer, Result_Buffer_Value => Extract (Ctx.Buffer.all (Buffer_First .. Buffer_Last), Offset)),
            when F_Result_Unused_Padding =>
               (Fld => F_Result_Unused_Padding, Result_Unused_Padding_Value => Extract (Ctx.Buffer.all (Buffer_First .. Buffer_Last), Offset)),
            when F_Result_Parent_Offset =>
               (Fld => F_Result_Parent_Offset, Result_Parent_Offset_Value => Extract (Ctx.Buffer.all (Buffer_First .. Buffer_Last), Offset)),
            when F_Result_Length =>
               (Fld => F_Result_Length, Result_Length_Value => Extract (Ctx.Buffer.all (Buffer_First .. Buffer_Last), Offset)),
            when F_Result_Cookie =>
               (Fld => F_Result_Cookie, Result_Cookie_Value => Extract (Ctx.Buffer.all (Buffer_First .. Buffer_Last), Offset)),
            when F_Result_Index =>
               (Fld => F_Result_Index, Result_Index_Value => Extract (Ctx.Buffer.all (Buffer_First .. Buffer_Last), Offset))));
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
               pragma Assert ((if Structural_Valid (Ctx.Cursors (F_Result_Kind)) then
                   (Ctx.Cursors (F_Result_Kind).Last - Ctx.Cursors (F_Result_Kind).First + 1) = Binder.Binder_Kind_Base'Size
                     and then Ctx.Cursors (F_Result_Kind).Predecessor = F_Initial
                     and then Ctx.Cursors (F_Result_Kind).First = Ctx.First
                     and then (if Structural_Valid (Ctx.Cursors (F_Result_Arity)) then
                        (Ctx.Cursors (F_Result_Arity).Last - Ctx.Cursors (F_Result_Arity).First + 1) = Binder.Binder_Arity_Base'Size
                          and then Ctx.Cursors (F_Result_Arity).Predecessor = F_Result_Kind
                          and then Ctx.Cursors (F_Result_Arity).First = (Ctx.Cursors (F_Result_Kind).Last + 1)
                          and then (if Structural_Valid (Ctx.Cursors (F_Result_Tag))
                               and then (Types.Bit_Length (Ctx.Cursors (F_Result_Arity).Value.Result_Arity_Value) = Types.Bit_Length (Convert (BA_SINGLE))
                                 or (Types.Bit_Length (Ctx.Cursors (F_Result_Kind).Value.Result_Kind_Value) = Types.Bit_Length (Convert (BK_FD))
                                   and Types.Bit_Length (Ctx.Cursors (F_Result_Arity).Value.Result_Arity_Value) = Types.Bit_Length (Convert (BA_ARRAY)))) then
                             (Ctx.Cursors (F_Result_Tag).Last - Ctx.Cursors (F_Result_Tag).First + 1) = Binder.Binder_Tag_Base'Size
                               and then Ctx.Cursors (F_Result_Tag).Predecessor = F_Result_Arity
                               and then Ctx.Cursors (F_Result_Tag).First = (Ctx.Cursors (F_Result_Arity).Last + 1)
                               and then (if Structural_Valid (Ctx.Cursors (F_Result_Legacy_Flags))
                                    and then Types.Bit_Length (Ctx.Cursors (F_Result_Kind).Value.Result_Kind_Value) = Types.Bit_Length (Convert (BK_FD)) then
                                  (Ctx.Cursors (F_Result_Legacy_Flags).Last - Ctx.Cursors (F_Result_Legacy_Flags).First + 1) = Binder.MBZ32_Base'Size
                                    and then Ctx.Cursors (F_Result_Legacy_Flags).Predecessor = F_Result_Tag
                                    and then Ctx.Cursors (F_Result_Legacy_Flags).First = (Ctx.Cursors (F_Result_Tag).Last + 1)
                                    and then (if Structural_Valid (Ctx.Cursors (F_Result_FD))
                                         and then Types.Bit_Length (Ctx.Cursors (F_Result_Arity).Value.Result_Arity_Value) = Types.Bit_Length (Convert (BA_SINGLE)) then
                                       (Ctx.Cursors (F_Result_FD).Last - Ctx.Cursors (F_Result_FD).First + 1) = Binder.Handle_Base'Size
                                         and then Ctx.Cursors (F_Result_FD).Predecessor = F_Result_Legacy_Flags
                                         and then Ctx.Cursors (F_Result_FD).First = (Ctx.Cursors (F_Result_Legacy_Flags).Last + 1)
                                         and then (if Structural_Valid (Ctx.Cursors (F_Result_Cookie)) then
                                            (Ctx.Cursors (F_Result_Cookie).Last - Ctx.Cursors (F_Result_Cookie).First + 1) = Binder.Cookie'Size
                                              and then Ctx.Cursors (F_Result_Cookie).Predecessor = F_Result_FD
                                              and then Ctx.Cursors (F_Result_Cookie).First = (Ctx.Cursors (F_Result_FD).Last + 1)))
                                    and then (if Structural_Valid (Ctx.Cursors (F_Result_Num_FDs))
                                         and then Types.Bit_Length (Ctx.Cursors (F_Result_Arity).Value.Result_Arity_Value) = Types.Bit_Length (Convert (BA_ARRAY)) then
                                       (Ctx.Cursors (F_Result_Num_FDs).Last - Ctx.Cursors (F_Result_Num_FDs).First + 1) = Binder.Count'Size
                                         and then Ctx.Cursors (F_Result_Num_FDs).Predecessor = F_Result_Legacy_Flags
                                         and then Ctx.Cursors (F_Result_Num_FDs).First = (Ctx.Cursors (F_Result_Legacy_Flags).Last + 1)
                                         and then (if Structural_Valid (Ctx.Cursors (F_Result_Parent)) then
                                            (Ctx.Cursors (F_Result_Parent).Last - Ctx.Cursors (F_Result_Parent).First + 1) = Binder.Index'Size
                                              and then Ctx.Cursors (F_Result_Parent).Predecessor = F_Result_Num_FDs
                                              and then Ctx.Cursors (F_Result_Parent).First = (Ctx.Cursors (F_Result_Num_FDs).Last + 1)
                                              and then (if Structural_Valid (Ctx.Cursors (F_Result_Parent_Offset)) then
                                                 (Ctx.Cursors (F_Result_Parent_Offset).Last - Ctx.Cursors (F_Result_Parent_Offset).First + 1) = Binder.Offset'Size
                                                   and then Ctx.Cursors (F_Result_Parent_Offset).Predecessor = F_Result_Parent
                                                   and then Ctx.Cursors (F_Result_Parent_Offset).First = (Ctx.Cursors (F_Result_Parent).Last + 1)))))
                               and then (if Structural_Valid (Ctx.Cursors (F_Result_Has_Parent))
                                    and then Types.Bit_Length (Ctx.Cursors (F_Result_Kind).Value.Result_Kind_Value) = Types.Bit_Length (Convert (BK_POINTER)) then
                                  (Ctx.Cursors (F_Result_Has_Parent).Last - Ctx.Cursors (F_Result_Has_Parent).First + 1) = Builtin_Types.Boolean_Base'Size
                                    and then Ctx.Cursors (F_Result_Has_Parent).Predecessor = F_Result_Tag
                                    and then Ctx.Cursors (F_Result_Has_Parent).First = (Ctx.Cursors (F_Result_Tag).Last + 1)
                                    and then (if Structural_Valid (Ctx.Cursors (F_Result_Padding)) then
                                       (Ctx.Cursors (F_Result_Padding).Last - Ctx.Cursors (F_Result_Padding).First + 1) = Binder.MBZ31_Base'Size
                                         and then Ctx.Cursors (F_Result_Padding).Predecessor = F_Result_Has_Parent
                                         and then Ctx.Cursors (F_Result_Padding).First = (Ctx.Cursors (F_Result_Has_Parent).Last + 1)
                                         and then (if Structural_Valid (Ctx.Cursors (F_Result_Buffer)) then
                                            (Ctx.Cursors (F_Result_Buffer).Last - Ctx.Cursors (F_Result_Buffer).First + 1) = Binder.Index'Size
                                              and then Ctx.Cursors (F_Result_Buffer).Predecessor = F_Result_Padding
                                              and then Ctx.Cursors (F_Result_Buffer).First = (Ctx.Cursors (F_Result_Padding).Last + 1)
                                              and then (if Structural_Valid (Ctx.Cursors (F_Result_Length)) then
                                                 (Ctx.Cursors (F_Result_Length).Last - Ctx.Cursors (F_Result_Length).First + 1) = Binder.Length_Base'Size
                                                   and then Ctx.Cursors (F_Result_Length).Predecessor = F_Result_Buffer
                                                   and then Ctx.Cursors (F_Result_Length).First = (Ctx.Cursors (F_Result_Buffer).Last + 1)
                                                   and then (if Structural_Valid (Ctx.Cursors (F_Result_Index))
                                                        and then Types.Bit_Length (Ctx.Cursors (F_Result_Has_Parent).Value.Result_Has_Parent_Value) = Types.Bit_Length (Convert (True)) then
                                                      (Ctx.Cursors (F_Result_Index).Last - Ctx.Cursors (F_Result_Index).First + 1) = Binder.Index'Size
                                                        and then Ctx.Cursors (F_Result_Index).Predecessor = F_Result_Length
                                                        and then Ctx.Cursors (F_Result_Index).First = (Ctx.Cursors (F_Result_Length).Last + 1))))))
                               and then (if Structural_Valid (Ctx.Cursors (F_Result_Flags))
                                    and then (Types.Bit_Length (Ctx.Cursors (F_Result_Kind).Value.Result_Kind_Value) /= Types.Bit_Length (Convert (BK_POINTER))
                                      and Types.Bit_Length (Ctx.Cursors (F_Result_Kind).Value.Result_Kind_Value) /= Types.Bit_Length (Convert (BK_FD))) then
                                  (Ctx.Cursors (F_Result_Flags).Last - Ctx.Cursors (F_Result_Flags).First + 1) = Binder.Flat_Binder_Flags_Base'Size
                                    and then Ctx.Cursors (F_Result_Flags).Predecessor = F_Result_Tag
                                    and then Ctx.Cursors (F_Result_Flags).First = (Ctx.Cursors (F_Result_Tag).Last + 1)
                                    and then (if Structural_Valid (Ctx.Cursors (F_Result_Binder))
                                         and then (Types.Bit_Length (Ctx.Cursors (F_Result_Kind).Value.Result_Kind_Value) = Types.Bit_Length (Convert (BK_STRONG_BINDER))
                                           or Types.Bit_Length (Ctx.Cursors (F_Result_Kind).Value.Result_Kind_Value) = Types.Bit_Length (Convert (BK_WEAK_BINDER))) then
                                       (Ctx.Cursors (F_Result_Binder).Last - Ctx.Cursors (F_Result_Binder).First + 1) = Binder.Value'Size
                                         and then Ctx.Cursors (F_Result_Binder).Predecessor = F_Result_Flags
                                         and then Ctx.Cursors (F_Result_Binder).First = (Ctx.Cursors (F_Result_Flags).Last + 1)
                                         and then (if Structural_Valid (Ctx.Cursors (F_Result_Cookie)) then
                                            (Ctx.Cursors (F_Result_Cookie).Last - Ctx.Cursors (F_Result_Cookie).First + 1) = Binder.Cookie'Size
                                              and then Ctx.Cursors (F_Result_Cookie).Predecessor = F_Result_Binder
                                              and then Ctx.Cursors (F_Result_Cookie).First = (Ctx.Cursors (F_Result_Binder).Last + 1)))
                                    and then (if Structural_Valid (Ctx.Cursors (F_Result_Handle))
                                         and then (Types.Bit_Length (Ctx.Cursors (F_Result_Kind).Value.Result_Kind_Value) = Types.Bit_Length (Convert (BK_STRONG_HANDLE))
                                           or Types.Bit_Length (Ctx.Cursors (F_Result_Kind).Value.Result_Kind_Value) = Types.Bit_Length (Convert (BK_WEAK_HANDLE))) then
                                       (Ctx.Cursors (F_Result_Handle).Last - Ctx.Cursors (F_Result_Handle).First + 1) = Binder.Handle_Base'Size
                                         and then Ctx.Cursors (F_Result_Handle).Predecessor = F_Result_Flags
                                         and then Ctx.Cursors (F_Result_Handle).First = (Ctx.Cursors (F_Result_Flags).Last + 1)
                                         and then (if Structural_Valid (Ctx.Cursors (F_Result_Unused_Padding)) then
                                            (Ctx.Cursors (F_Result_Unused_Padding).Last - Ctx.Cursors (F_Result_Unused_Padding).First + 1) = Binder.MBZ32_Base'Size
                                              and then Ctx.Cursors (F_Result_Unused_Padding).Predecessor = F_Result_Handle
                                              and then Ctx.Cursors (F_Result_Unused_Padding).First = (Ctx.Cursors (F_Result_Handle).Last + 1)
                                              and then (if Structural_Valid (Ctx.Cursors (F_Result_Cookie)) then
                                                 (Ctx.Cursors (F_Result_Cookie).Last - Ctx.Cursors (F_Result_Cookie).First + 1) = Binder.Cookie'Size
                                                   and then Ctx.Cursors (F_Result_Cookie).Predecessor = F_Result_Unused_Padding
                                                   and then Ctx.Cursors (F_Result_Cookie).First = (Ctx.Cursors (F_Result_Unused_Padding).Last + 1)))))))));
               if Fld = F_Result_Kind then
                  Ctx.Cursors (Successor (Ctx, Fld)) := (State => S_Invalid, Predecessor => Fld);
               elsif Fld = F_Result_Arity then
                  Ctx.Cursors (Successor (Ctx, Fld)) := (State => S_Invalid, Predecessor => Fld);
               elsif Fld = F_Result_Tag then
                  Ctx.Cursors (Successor (Ctx, Fld)) := (State => S_Invalid, Predecessor => Fld);
               elsif Fld = F_Result_Legacy_Flags then
                  Ctx.Cursors (Successor (Ctx, Fld)) := (State => S_Invalid, Predecessor => Fld);
               elsif Fld = F_Result_Has_Parent then
                  Ctx.Cursors (Successor (Ctx, Fld)) := (State => S_Invalid, Predecessor => Fld);
               elsif Fld = F_Result_Flags then
                  Ctx.Cursors (Successor (Ctx, Fld)) := (State => S_Invalid, Predecessor => Fld);
               elsif Fld = F_Result_FD then
                  Ctx.Cursors (Successor (Ctx, Fld)) := (State => S_Invalid, Predecessor => Fld);
               elsif Fld = F_Result_Num_FDs then
                  Ctx.Cursors (Successor (Ctx, Fld)) := (State => S_Invalid, Predecessor => Fld);
               elsif Fld = F_Result_Padding then
                  Ctx.Cursors (Successor (Ctx, Fld)) := (State => S_Invalid, Predecessor => Fld);
               elsif Fld = F_Result_Binder then
                  Ctx.Cursors (Successor (Ctx, Fld)) := (State => S_Invalid, Predecessor => Fld);
               elsif Fld = F_Result_Handle then
                  Ctx.Cursors (Successor (Ctx, Fld)) := (State => S_Invalid, Predecessor => Fld);
               elsif Fld = F_Result_Parent then
                  Ctx.Cursors (Successor (Ctx, Fld)) := (State => S_Invalid, Predecessor => Fld);
               elsif Fld = F_Result_Buffer then
                  Ctx.Cursors (Successor (Ctx, Fld)) := (State => S_Invalid, Predecessor => Fld);
               elsif Fld = F_Result_Unused_Padding then
                  Ctx.Cursors (Successor (Ctx, Fld)) := (State => S_Invalid, Predecessor => Fld);
               elsif Fld = F_Result_Parent_Offset then
                  Ctx.Cursors (Successor (Ctx, Fld)) := (State => S_Invalid, Predecessor => Fld);
               elsif Fld = F_Result_Length then
                  Ctx.Cursors (Successor (Ctx, Fld)) := (State => S_Invalid, Predecessor => Fld);
               elsif Fld = F_Result_Cookie then
                  Ctx.Cursors (Successor (Ctx, Fld)) := (State => S_Invalid, Predecessor => Fld);
               elsif Fld = F_Result_Index then
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
      Verify (Ctx, F_Result_Kind);
      Verify (Ctx, F_Result_Arity);
      Verify (Ctx, F_Result_Tag);
      Verify (Ctx, F_Result_Legacy_Flags);
      Verify (Ctx, F_Result_Has_Parent);
      Verify (Ctx, F_Result_Flags);
      Verify (Ctx, F_Result_FD);
      Verify (Ctx, F_Result_Num_FDs);
      Verify (Ctx, F_Result_Padding);
      Verify (Ctx, F_Result_Binder);
      Verify (Ctx, F_Result_Handle);
      Verify (Ctx, F_Result_Parent);
      Verify (Ctx, F_Result_Buffer);
      Verify (Ctx, F_Result_Unused_Padding);
      Verify (Ctx, F_Result_Parent_Offset);
      Verify (Ctx, F_Result_Length);
      Verify (Ctx, F_Result_Cookie);
      Verify (Ctx, F_Result_Index);
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
     (Valid (Ctx, F_Result_Kind)
      and then Valid (Ctx, F_Result_Arity)
      and then Valid (Ctx, F_Result_Tag)
      and then (Types.Bit_Length (Ctx.Cursors (F_Result_Arity).Value.Result_Arity_Value) = Types.Bit_Length (Convert (BA_SINGLE))
        or (Types.Bit_Length (Ctx.Cursors (F_Result_Kind).Value.Result_Kind_Value) = Types.Bit_Length (Convert (BK_FD))
          and Types.Bit_Length (Ctx.Cursors (F_Result_Arity).Value.Result_Arity_Value) = Types.Bit_Length (Convert (BA_ARRAY))))
      and then ((Valid (Ctx, F_Result_Legacy_Flags)
          and then Types.Bit_Length (Ctx.Cursors (F_Result_Kind).Value.Result_Kind_Value) = Types.Bit_Length (Convert (BK_FD))
          and then ((Valid (Ctx, F_Result_FD)
              and then Types.Bit_Length (Ctx.Cursors (F_Result_Arity).Value.Result_Arity_Value) = Types.Bit_Length (Convert (BA_SINGLE))
              and then Valid (Ctx, F_Result_Cookie))
            or (Valid (Ctx, F_Result_Num_FDs)
              and then Types.Bit_Length (Ctx.Cursors (F_Result_Arity).Value.Result_Arity_Value) = Types.Bit_Length (Convert (BA_ARRAY))
              and then Valid (Ctx, F_Result_Parent)
              and then Valid (Ctx, F_Result_Parent_Offset))))
        or (Valid (Ctx, F_Result_Has_Parent)
          and then Types.Bit_Length (Ctx.Cursors (F_Result_Kind).Value.Result_Kind_Value) = Types.Bit_Length (Convert (BK_POINTER))
          and then Valid (Ctx, F_Result_Padding)
          and then Valid (Ctx, F_Result_Buffer)
          and then Valid (Ctx, F_Result_Length)
          and then (Types.Bit_Length (Ctx.Cursors (F_Result_Has_Parent).Value.Result_Has_Parent_Value) = Types.Bit_Length (Convert (False))
            or (Valid (Ctx, F_Result_Index)
              and then Types.Bit_Length (Ctx.Cursors (F_Result_Has_Parent).Value.Result_Has_Parent_Value) = Types.Bit_Length (Convert (True)))))
        or (Valid (Ctx, F_Result_Flags)
          and then (Types.Bit_Length (Ctx.Cursors (F_Result_Kind).Value.Result_Kind_Value) /= Types.Bit_Length (Convert (BK_POINTER))
            and Types.Bit_Length (Ctx.Cursors (F_Result_Kind).Value.Result_Kind_Value) /= Types.Bit_Length (Convert (BK_FD)))
          and then ((Valid (Ctx, F_Result_Binder)
              and then (Types.Bit_Length (Ctx.Cursors (F_Result_Kind).Value.Result_Kind_Value) = Types.Bit_Length (Convert (BK_STRONG_BINDER))
                or Types.Bit_Length (Ctx.Cursors (F_Result_Kind).Value.Result_Kind_Value) = Types.Bit_Length (Convert (BK_WEAK_BINDER)))
              and then Valid (Ctx, F_Result_Cookie))
            or (Valid (Ctx, F_Result_Handle)
              and then (Types.Bit_Length (Ctx.Cursors (F_Result_Kind).Value.Result_Kind_Value) = Types.Bit_Length (Convert (BK_STRONG_HANDLE))
                or Types.Bit_Length (Ctx.Cursors (F_Result_Kind).Value.Result_Kind_Value) = Types.Bit_Length (Convert (BK_WEAK_HANDLE)))
              and then Valid (Ctx, F_Result_Unused_Padding)
              and then Valid (Ctx, F_Result_Cookie))))));

   function Valid_Message (Ctx : Context) return Boolean is
     (Valid (Ctx, F_Result_Kind)
      and then Valid (Ctx, F_Result_Arity)
      and then Valid (Ctx, F_Result_Tag)
      and then (Types.Bit_Length (Ctx.Cursors (F_Result_Arity).Value.Result_Arity_Value) = Types.Bit_Length (Convert (BA_SINGLE))
        or (Types.Bit_Length (Ctx.Cursors (F_Result_Kind).Value.Result_Kind_Value) = Types.Bit_Length (Convert (BK_FD))
          and Types.Bit_Length (Ctx.Cursors (F_Result_Arity).Value.Result_Arity_Value) = Types.Bit_Length (Convert (BA_ARRAY))))
      and then ((Valid (Ctx, F_Result_Legacy_Flags)
          and then Types.Bit_Length (Ctx.Cursors (F_Result_Kind).Value.Result_Kind_Value) = Types.Bit_Length (Convert (BK_FD))
          and then ((Valid (Ctx, F_Result_FD)
              and then Types.Bit_Length (Ctx.Cursors (F_Result_Arity).Value.Result_Arity_Value) = Types.Bit_Length (Convert (BA_SINGLE))
              and then Valid (Ctx, F_Result_Cookie))
            or (Valid (Ctx, F_Result_Num_FDs)
              and then Types.Bit_Length (Ctx.Cursors (F_Result_Arity).Value.Result_Arity_Value) = Types.Bit_Length (Convert (BA_ARRAY))
              and then Valid (Ctx, F_Result_Parent)
              and then Valid (Ctx, F_Result_Parent_Offset))))
        or (Valid (Ctx, F_Result_Has_Parent)
          and then Types.Bit_Length (Ctx.Cursors (F_Result_Kind).Value.Result_Kind_Value) = Types.Bit_Length (Convert (BK_POINTER))
          and then Valid (Ctx, F_Result_Padding)
          and then Valid (Ctx, F_Result_Buffer)
          and then Valid (Ctx, F_Result_Length)
          and then (Types.Bit_Length (Ctx.Cursors (F_Result_Has_Parent).Value.Result_Has_Parent_Value) = Types.Bit_Length (Convert (False))
            or (Valid (Ctx, F_Result_Index)
              and then Types.Bit_Length (Ctx.Cursors (F_Result_Has_Parent).Value.Result_Has_Parent_Value) = Types.Bit_Length (Convert (True)))))
        or (Valid (Ctx, F_Result_Flags)
          and then (Types.Bit_Length (Ctx.Cursors (F_Result_Kind).Value.Result_Kind_Value) /= Types.Bit_Length (Convert (BK_POINTER))
            and Types.Bit_Length (Ctx.Cursors (F_Result_Kind).Value.Result_Kind_Value) /= Types.Bit_Length (Convert (BK_FD)))
          and then ((Valid (Ctx, F_Result_Binder)
              and then (Types.Bit_Length (Ctx.Cursors (F_Result_Kind).Value.Result_Kind_Value) = Types.Bit_Length (Convert (BK_STRONG_BINDER))
                or Types.Bit_Length (Ctx.Cursors (F_Result_Kind).Value.Result_Kind_Value) = Types.Bit_Length (Convert (BK_WEAK_BINDER)))
              and then Valid (Ctx, F_Result_Cookie))
            or (Valid (Ctx, F_Result_Handle)
              and then (Types.Bit_Length (Ctx.Cursors (F_Result_Kind).Value.Result_Kind_Value) = Types.Bit_Length (Convert (BK_STRONG_HANDLE))
                or Types.Bit_Length (Ctx.Cursors (F_Result_Kind).Value.Result_Kind_Value) = Types.Bit_Length (Convert (BK_WEAK_HANDLE)))
              and then Valid (Ctx, F_Result_Unused_Padding)
              and then Valid (Ctx, F_Result_Cookie))))));

   function Incomplete_Message (Ctx : Context) return Boolean is
     (Incomplete (Ctx, F_Result_Kind)
      or Incomplete (Ctx, F_Result_Arity)
      or Incomplete (Ctx, F_Result_Tag)
      or Incomplete (Ctx, F_Result_Legacy_Flags)
      or Incomplete (Ctx, F_Result_Has_Parent)
      or Incomplete (Ctx, F_Result_Flags)
      or Incomplete (Ctx, F_Result_FD)
      or Incomplete (Ctx, F_Result_Num_FDs)
      or Incomplete (Ctx, F_Result_Padding)
      or Incomplete (Ctx, F_Result_Binder)
      or Incomplete (Ctx, F_Result_Handle)
      or Incomplete (Ctx, F_Result_Parent)
      or Incomplete (Ctx, F_Result_Buffer)
      or Incomplete (Ctx, F_Result_Unused_Padding)
      or Incomplete (Ctx, F_Result_Parent_Offset)
      or Incomplete (Ctx, F_Result_Length)
      or Incomplete (Ctx, F_Result_Cookie)
      or Incomplete (Ctx, F_Result_Index));

   function Get_Result_Kind (Ctx : Context) return Binder.Binder_Kind is
     (Convert (Ctx.Cursors (F_Result_Kind).Value.Result_Kind_Value));

   function Get_Result_Arity (Ctx : Context) return Binder.Binder_Arity is
     (Convert (Ctx.Cursors (F_Result_Arity).Value.Result_Arity_Value));

   function Get_Result_Tag (Ctx : Context) return Binder.Binder_Tag is
     (Ctx.Cursors (F_Result_Tag).Value.Result_Tag_Value);

   function Get_Result_Legacy_Flags (Ctx : Context) return Binder.MBZ32 is
     (Ctx.Cursors (F_Result_Legacy_Flags).Value.Result_Legacy_Flags_Value);

   function Get_Result_Has_Parent (Ctx : Context) return Boolean is
     (Convert (Ctx.Cursors (F_Result_Has_Parent).Value.Result_Has_Parent_Value));

   function Get_Result_Flags (Ctx : Context) return Binder.Flat_Binder_Flags is
     (Convert (Ctx.Cursors (F_Result_Flags).Value.Result_Flags_Value));

   function Get_Result_FD (Ctx : Context) return Binder.Handle is
     (Ctx.Cursors (F_Result_FD).Value.Result_FD_Value);

   function Get_Result_Num_FDs (Ctx : Context) return Binder.Count is
     (Ctx.Cursors (F_Result_Num_FDs).Value.Result_Num_FDs_Value);

   function Get_Result_Padding (Ctx : Context) return Binder.MBZ31 is
     (Ctx.Cursors (F_Result_Padding).Value.Result_Padding_Value);

   function Get_Result_Binder (Ctx : Context) return Binder.Value is
     (Ctx.Cursors (F_Result_Binder).Value.Result_Binder_Value);

   function Get_Result_Handle (Ctx : Context) return Binder.Handle is
     (Ctx.Cursors (F_Result_Handle).Value.Result_Handle_Value);

   function Get_Result_Parent (Ctx : Context) return Binder.Index is
     (Ctx.Cursors (F_Result_Parent).Value.Result_Parent_Value);

   function Get_Result_Buffer (Ctx : Context) return Binder.Index is
     (Ctx.Cursors (F_Result_Buffer).Value.Result_Buffer_Value);

   function Get_Result_Unused_Padding (Ctx : Context) return Binder.MBZ32 is
     (Ctx.Cursors (F_Result_Unused_Padding).Value.Result_Unused_Padding_Value);

   function Get_Result_Parent_Offset (Ctx : Context) return Binder.Offset is
     (Ctx.Cursors (F_Result_Parent_Offset).Value.Result_Parent_Offset_Value);

   function Get_Result_Length (Ctx : Context) return Binder.Length is
     (Ctx.Cursors (F_Result_Length).Value.Result_Length_Value);

   function Get_Result_Cookie (Ctx : Context) return Binder.Cookie is
     (Ctx.Cursors (F_Result_Cookie).Value.Result_Cookie_Value);

   function Get_Result_Index (Ctx : Context) return Binder.Index is
     (Ctx.Cursors (F_Result_Index).Value.Result_Index_Value);

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
      procedure Insert is new Types.Insert (Binder.Binder_Kind_Base);
      procedure Insert is new Types.Insert (Binder.Binder_Arity_Base);
      procedure Insert is new Types.Insert (Binder.Binder_Tag_Base);
      procedure Insert is new Types.Insert (Binder.MBZ32_Base);
      procedure Insert is new Types.Insert (Builtin_Types.Boolean_Base);
      procedure Insert is new Types.Insert (Binder.Flat_Binder_Flags_Base);
      procedure Insert is new Types.Insert (Binder.Handle_Base);
      procedure Insert is new Types.Insert (Binder.Count);
      procedure Insert is new Types.Insert (Binder.MBZ31_Base);
      procedure Insert is new Types.Insert (Binder.Value);
      procedure Insert is new Types.Insert (Binder.Index);
      procedure Insert is new Types.Insert (Binder.Offset);
      procedure Insert is new Types.Insert (Binder.Length_Base);
      procedure Insert is new Types.Insert (Binder.Cookie);
   begin
      Fst := First;
      Lst := Last;
      case Val.Fld is
         when F_Initial =>
            null;
         when F_Result_Kind =>
            Insert (Val.Result_Kind_Value, Ctx.Buffer.all (Buffer_First .. Buffer_Last), Offset);
         when F_Result_Arity =>
            Insert (Val.Result_Arity_Value, Ctx.Buffer.all (Buffer_First .. Buffer_Last), Offset);
         when F_Result_Tag =>
            Insert (Val.Result_Tag_Value, Ctx.Buffer.all (Buffer_First .. Buffer_Last), Offset);
         when F_Result_Legacy_Flags =>
            Insert (Val.Result_Legacy_Flags_Value, Ctx.Buffer.all (Buffer_First .. Buffer_Last), Offset);
         when F_Result_Has_Parent =>
            Insert (Val.Result_Has_Parent_Value, Ctx.Buffer.all (Buffer_First .. Buffer_Last), Offset);
         when F_Result_Flags =>
            Insert (Val.Result_Flags_Value, Ctx.Buffer.all (Buffer_First .. Buffer_Last), Offset);
         when F_Result_FD =>
            Insert (Val.Result_FD_Value, Ctx.Buffer.all (Buffer_First .. Buffer_Last), Offset);
         when F_Result_Num_FDs =>
            Insert (Val.Result_Num_FDs_Value, Ctx.Buffer.all (Buffer_First .. Buffer_Last), Offset);
         when F_Result_Padding =>
            Insert (Val.Result_Padding_Value, Ctx.Buffer.all (Buffer_First .. Buffer_Last), Offset);
         when F_Result_Binder =>
            Insert (Val.Result_Binder_Value, Ctx.Buffer.all (Buffer_First .. Buffer_Last), Offset);
         when F_Result_Handle =>
            Insert (Val.Result_Handle_Value, Ctx.Buffer.all (Buffer_First .. Buffer_Last), Offset);
         when F_Result_Parent =>
            Insert (Val.Result_Parent_Value, Ctx.Buffer.all (Buffer_First .. Buffer_Last), Offset);
         when F_Result_Buffer =>
            Insert (Val.Result_Buffer_Value, Ctx.Buffer.all (Buffer_First .. Buffer_Last), Offset);
         when F_Result_Unused_Padding =>
            Insert (Val.Result_Unused_Padding_Value, Ctx.Buffer.all (Buffer_First .. Buffer_Last), Offset);
         when F_Result_Parent_Offset =>
            Insert (Val.Result_Parent_Offset_Value, Ctx.Buffer.all (Buffer_First .. Buffer_Last), Offset);
         when F_Result_Length =>
            Insert (Val.Result_Length_Value, Ctx.Buffer.all (Buffer_First .. Buffer_Last), Offset);
         when F_Result_Cookie =>
            Insert (Val.Result_Cookie_Value, Ctx.Buffer.all (Buffer_First .. Buffer_Last), Offset);
         when F_Result_Index =>
            Insert (Val.Result_Index_Value, Ctx.Buffer.all (Buffer_First .. Buffer_Last), Offset);
         when F_Final =>
            null;
      end case;
   end Set_Field_Value;

   procedure Set_Result_Kind (Ctx : in out Context; Val : Binder.Binder_Kind) is
      Field_Value : constant Field_Dependent_Value := (F_Result_Kind, Convert (Val));
      First, Last : Types.Bit_Index;
   begin
      Reset_Dependent_Fields (Ctx, F_Result_Kind);
      Set_Field_Value (Ctx, Field_Value, First, Last);
      Ctx := (Ctx.Buffer_First, Ctx.Buffer_Last, Ctx.First, Last, Ctx.Buffer, Ctx.Cursors);
      Ctx.Cursors (F_Result_Kind) := (State => S_Valid, First => First, Last => Last, Value => Field_Value, Predecessor => Ctx.Cursors (F_Result_Kind).Predecessor);
      Ctx.Cursors (Successor (Ctx, F_Result_Kind)) := (State => S_Invalid, Predecessor => F_Result_Kind);
   end Set_Result_Kind;

   procedure Set_Result_Arity (Ctx : in out Context; Val : Binder.Binder_Arity) is
      Field_Value : constant Field_Dependent_Value := (F_Result_Arity, Convert (Val));
      First, Last : Types.Bit_Index;
   begin
      Reset_Dependent_Fields (Ctx, F_Result_Arity);
      Set_Field_Value (Ctx, Field_Value, First, Last);
      Ctx := (Ctx.Buffer_First, Ctx.Buffer_Last, Ctx.First, Last, Ctx.Buffer, Ctx.Cursors);
      Ctx.Cursors (F_Result_Arity) := (State => S_Valid, First => First, Last => Last, Value => Field_Value, Predecessor => Ctx.Cursors (F_Result_Arity).Predecessor);
      Ctx.Cursors (Successor (Ctx, F_Result_Arity)) := (State => S_Invalid, Predecessor => F_Result_Arity);
   end Set_Result_Arity;

   procedure Set_Result_Tag (Ctx : in out Context; Val : Binder.Binder_Tag) is
      Field_Value : constant Field_Dependent_Value := (F_Result_Tag, Val);
      First, Last : Types.Bit_Index;
   begin
      Reset_Dependent_Fields (Ctx, F_Result_Tag);
      Set_Field_Value (Ctx, Field_Value, First, Last);
      Ctx := (Ctx.Buffer_First, Ctx.Buffer_Last, Ctx.First, Last, Ctx.Buffer, Ctx.Cursors);
      Ctx.Cursors (F_Result_Tag) := (State => S_Valid, First => First, Last => Last, Value => Field_Value, Predecessor => Ctx.Cursors (F_Result_Tag).Predecessor);
      Ctx.Cursors (Successor (Ctx, F_Result_Tag)) := (State => S_Invalid, Predecessor => F_Result_Tag);
   end Set_Result_Tag;

   procedure Set_Result_Legacy_Flags (Ctx : in out Context; Val : Binder.MBZ32) is
      Field_Value : constant Field_Dependent_Value := (F_Result_Legacy_Flags, Val);
      First, Last : Types.Bit_Index;
   begin
      Reset_Dependent_Fields (Ctx, F_Result_Legacy_Flags);
      Set_Field_Value (Ctx, Field_Value, First, Last);
      Ctx := (Ctx.Buffer_First, Ctx.Buffer_Last, Ctx.First, Last, Ctx.Buffer, Ctx.Cursors);
      Ctx.Cursors (F_Result_Legacy_Flags) := (State => S_Valid, First => First, Last => Last, Value => Field_Value, Predecessor => Ctx.Cursors (F_Result_Legacy_Flags).Predecessor);
      Ctx.Cursors (Successor (Ctx, F_Result_Legacy_Flags)) := (State => S_Invalid, Predecessor => F_Result_Legacy_Flags);
   end Set_Result_Legacy_Flags;

   procedure Set_Result_Has_Parent (Ctx : in out Context; Val : Boolean) is
      Field_Value : constant Field_Dependent_Value := (F_Result_Has_Parent, Convert (Val));
      First, Last : Types.Bit_Index;
   begin
      Reset_Dependent_Fields (Ctx, F_Result_Has_Parent);
      Set_Field_Value (Ctx, Field_Value, First, Last);
      Ctx := (Ctx.Buffer_First, Ctx.Buffer_Last, Ctx.First, Last, Ctx.Buffer, Ctx.Cursors);
      Ctx.Cursors (F_Result_Has_Parent) := (State => S_Valid, First => First, Last => Last, Value => Field_Value, Predecessor => Ctx.Cursors (F_Result_Has_Parent).Predecessor);
      Ctx.Cursors (Successor (Ctx, F_Result_Has_Parent)) := (State => S_Invalid, Predecessor => F_Result_Has_Parent);
   end Set_Result_Has_Parent;

   procedure Set_Result_Flags (Ctx : in out Context; Val : Binder.Flat_Binder_Flags) is
      Field_Value : constant Field_Dependent_Value := (F_Result_Flags, Convert (Val));
      First, Last : Types.Bit_Index;
   begin
      Reset_Dependent_Fields (Ctx, F_Result_Flags);
      Set_Field_Value (Ctx, Field_Value, First, Last);
      Ctx := (Ctx.Buffer_First, Ctx.Buffer_Last, Ctx.First, Last, Ctx.Buffer, Ctx.Cursors);
      Ctx.Cursors (F_Result_Flags) := (State => S_Valid, First => First, Last => Last, Value => Field_Value, Predecessor => Ctx.Cursors (F_Result_Flags).Predecessor);
      Ctx.Cursors (Successor (Ctx, F_Result_Flags)) := (State => S_Invalid, Predecessor => F_Result_Flags);
   end Set_Result_Flags;

   procedure Set_Result_FD (Ctx : in out Context; Val : Binder.Handle) is
      Field_Value : constant Field_Dependent_Value := (F_Result_FD, Val);
      First, Last : Types.Bit_Index;
   begin
      Reset_Dependent_Fields (Ctx, F_Result_FD);
      Set_Field_Value (Ctx, Field_Value, First, Last);
      Ctx := (Ctx.Buffer_First, Ctx.Buffer_Last, Ctx.First, Last, Ctx.Buffer, Ctx.Cursors);
      Ctx.Cursors (F_Result_FD) := (State => S_Valid, First => First, Last => Last, Value => Field_Value, Predecessor => Ctx.Cursors (F_Result_FD).Predecessor);
      Ctx.Cursors (Successor (Ctx, F_Result_FD)) := (State => S_Invalid, Predecessor => F_Result_FD);
   end Set_Result_FD;

   procedure Set_Result_Num_FDs (Ctx : in out Context; Val : Binder.Count) is
      Field_Value : constant Field_Dependent_Value := (F_Result_Num_FDs, Val);
      First, Last : Types.Bit_Index;
   begin
      Reset_Dependent_Fields (Ctx, F_Result_Num_FDs);
      Set_Field_Value (Ctx, Field_Value, First, Last);
      Ctx := (Ctx.Buffer_First, Ctx.Buffer_Last, Ctx.First, Last, Ctx.Buffer, Ctx.Cursors);
      Ctx.Cursors (F_Result_Num_FDs) := (State => S_Valid, First => First, Last => Last, Value => Field_Value, Predecessor => Ctx.Cursors (F_Result_Num_FDs).Predecessor);
      Ctx.Cursors (Successor (Ctx, F_Result_Num_FDs)) := (State => S_Invalid, Predecessor => F_Result_Num_FDs);
   end Set_Result_Num_FDs;

   procedure Set_Result_Padding (Ctx : in out Context; Val : Binder.MBZ31) is
      Field_Value : constant Field_Dependent_Value := (F_Result_Padding, Val);
      First, Last : Types.Bit_Index;
   begin
      Reset_Dependent_Fields (Ctx, F_Result_Padding);
      Set_Field_Value (Ctx, Field_Value, First, Last);
      Ctx := (Ctx.Buffer_First, Ctx.Buffer_Last, Ctx.First, Last, Ctx.Buffer, Ctx.Cursors);
      Ctx.Cursors (F_Result_Padding) := (State => S_Valid, First => First, Last => Last, Value => Field_Value, Predecessor => Ctx.Cursors (F_Result_Padding).Predecessor);
      Ctx.Cursors (Successor (Ctx, F_Result_Padding)) := (State => S_Invalid, Predecessor => F_Result_Padding);
   end Set_Result_Padding;

   procedure Set_Result_Binder (Ctx : in out Context; Val : Binder.Value) is
      Field_Value : constant Field_Dependent_Value := (F_Result_Binder, Val);
      First, Last : Types.Bit_Index;
   begin
      Reset_Dependent_Fields (Ctx, F_Result_Binder);
      Set_Field_Value (Ctx, Field_Value, First, Last);
      Ctx := (Ctx.Buffer_First, Ctx.Buffer_Last, Ctx.First, Last, Ctx.Buffer, Ctx.Cursors);
      Ctx.Cursors (F_Result_Binder) := (State => S_Valid, First => First, Last => Last, Value => Field_Value, Predecessor => Ctx.Cursors (F_Result_Binder).Predecessor);
      Ctx.Cursors (Successor (Ctx, F_Result_Binder)) := (State => S_Invalid, Predecessor => F_Result_Binder);
   end Set_Result_Binder;

   procedure Set_Result_Handle (Ctx : in out Context; Val : Binder.Handle) is
      Field_Value : constant Field_Dependent_Value := (F_Result_Handle, Val);
      First, Last : Types.Bit_Index;
   begin
      Reset_Dependent_Fields (Ctx, F_Result_Handle);
      Set_Field_Value (Ctx, Field_Value, First, Last);
      Ctx := (Ctx.Buffer_First, Ctx.Buffer_Last, Ctx.First, Last, Ctx.Buffer, Ctx.Cursors);
      Ctx.Cursors (F_Result_Handle) := (State => S_Valid, First => First, Last => Last, Value => Field_Value, Predecessor => Ctx.Cursors (F_Result_Handle).Predecessor);
      Ctx.Cursors (Successor (Ctx, F_Result_Handle)) := (State => S_Invalid, Predecessor => F_Result_Handle);
   end Set_Result_Handle;

   procedure Set_Result_Parent (Ctx : in out Context; Val : Binder.Index) is
      Field_Value : constant Field_Dependent_Value := (F_Result_Parent, Val);
      First, Last : Types.Bit_Index;
   begin
      Reset_Dependent_Fields (Ctx, F_Result_Parent);
      Set_Field_Value (Ctx, Field_Value, First, Last);
      Ctx := (Ctx.Buffer_First, Ctx.Buffer_Last, Ctx.First, Last, Ctx.Buffer, Ctx.Cursors);
      Ctx.Cursors (F_Result_Parent) := (State => S_Valid, First => First, Last => Last, Value => Field_Value, Predecessor => Ctx.Cursors (F_Result_Parent).Predecessor);
      Ctx.Cursors (Successor (Ctx, F_Result_Parent)) := (State => S_Invalid, Predecessor => F_Result_Parent);
   end Set_Result_Parent;

   procedure Set_Result_Buffer (Ctx : in out Context; Val : Binder.Index) is
      Field_Value : constant Field_Dependent_Value := (F_Result_Buffer, Val);
      First, Last : Types.Bit_Index;
   begin
      Reset_Dependent_Fields (Ctx, F_Result_Buffer);
      Set_Field_Value (Ctx, Field_Value, First, Last);
      Ctx := (Ctx.Buffer_First, Ctx.Buffer_Last, Ctx.First, Last, Ctx.Buffer, Ctx.Cursors);
      Ctx.Cursors (F_Result_Buffer) := (State => S_Valid, First => First, Last => Last, Value => Field_Value, Predecessor => Ctx.Cursors (F_Result_Buffer).Predecessor);
      Ctx.Cursors (Successor (Ctx, F_Result_Buffer)) := (State => S_Invalid, Predecessor => F_Result_Buffer);
   end Set_Result_Buffer;

   procedure Set_Result_Unused_Padding (Ctx : in out Context; Val : Binder.MBZ32) is
      Field_Value : constant Field_Dependent_Value := (F_Result_Unused_Padding, Val);
      First, Last : Types.Bit_Index;
   begin
      Reset_Dependent_Fields (Ctx, F_Result_Unused_Padding);
      Set_Field_Value (Ctx, Field_Value, First, Last);
      Ctx := (Ctx.Buffer_First, Ctx.Buffer_Last, Ctx.First, Last, Ctx.Buffer, Ctx.Cursors);
      Ctx.Cursors (F_Result_Unused_Padding) := (State => S_Valid, First => First, Last => Last, Value => Field_Value, Predecessor => Ctx.Cursors (F_Result_Unused_Padding).Predecessor);
      Ctx.Cursors (Successor (Ctx, F_Result_Unused_Padding)) := (State => S_Invalid, Predecessor => F_Result_Unused_Padding);
   end Set_Result_Unused_Padding;

   procedure Set_Result_Parent_Offset (Ctx : in out Context; Val : Binder.Offset) is
      Field_Value : constant Field_Dependent_Value := (F_Result_Parent_Offset, Val);
      First, Last : Types.Bit_Index;
   begin
      Reset_Dependent_Fields (Ctx, F_Result_Parent_Offset);
      Set_Field_Value (Ctx, Field_Value, First, Last);
      Ctx := (Ctx.Buffer_First, Ctx.Buffer_Last, Ctx.First, Last, Ctx.Buffer, Ctx.Cursors);
      Ctx.Cursors (F_Result_Parent_Offset) := (State => S_Valid, First => First, Last => Last, Value => Field_Value, Predecessor => Ctx.Cursors (F_Result_Parent_Offset).Predecessor);
      Ctx.Cursors (Successor (Ctx, F_Result_Parent_Offset)) := (State => S_Invalid, Predecessor => F_Result_Parent_Offset);
   end Set_Result_Parent_Offset;

   procedure Set_Result_Length (Ctx : in out Context; Val : Binder.Length) is
      Field_Value : constant Field_Dependent_Value := (F_Result_Length, Val);
      First, Last : Types.Bit_Index;
   begin
      Reset_Dependent_Fields (Ctx, F_Result_Length);
      Set_Field_Value (Ctx, Field_Value, First, Last);
      Ctx := (Ctx.Buffer_First, Ctx.Buffer_Last, Ctx.First, Last, Ctx.Buffer, Ctx.Cursors);
      Ctx.Cursors (F_Result_Length) := (State => S_Valid, First => First, Last => Last, Value => Field_Value, Predecessor => Ctx.Cursors (F_Result_Length).Predecessor);
      Ctx.Cursors (Successor (Ctx, F_Result_Length)) := (State => S_Invalid, Predecessor => F_Result_Length);
   end Set_Result_Length;

   procedure Set_Result_Cookie (Ctx : in out Context; Val : Binder.Cookie) is
      Field_Value : constant Field_Dependent_Value := (F_Result_Cookie, Val);
      First, Last : Types.Bit_Index;
   begin
      Reset_Dependent_Fields (Ctx, F_Result_Cookie);
      Set_Field_Value (Ctx, Field_Value, First, Last);
      Ctx := (Ctx.Buffer_First, Ctx.Buffer_Last, Ctx.First, Last, Ctx.Buffer, Ctx.Cursors);
      Ctx.Cursors (F_Result_Cookie) := (State => S_Valid, First => First, Last => Last, Value => Field_Value, Predecessor => Ctx.Cursors (F_Result_Cookie).Predecessor);
      Ctx.Cursors (Successor (Ctx, F_Result_Cookie)) := (State => S_Invalid, Predecessor => F_Result_Cookie);
   end Set_Result_Cookie;

   procedure Set_Result_Index (Ctx : in out Context; Val : Binder.Index) is
      Field_Value : constant Field_Dependent_Value := (F_Result_Index, Val);
      First, Last : Types.Bit_Index;
   begin
      Reset_Dependent_Fields (Ctx, F_Result_Index);
      Set_Field_Value (Ctx, Field_Value, First, Last);
      Ctx := (Ctx.Buffer_First, Ctx.Buffer_Last, Ctx.First, Last, Ctx.Buffer, Ctx.Cursors);
      Ctx.Cursors (F_Result_Index) := (State => S_Valid, First => First, Last => Last, Value => Field_Value, Predecessor => Ctx.Cursors (F_Result_Index).Predecessor);
      Ctx.Cursors (Successor (Ctx, F_Result_Index)) := (State => S_Invalid, Predecessor => F_Result_Index);
   end Set_Result_Index;

end Parpen.Service_Manager.Generic_Reply_Get_Service;
