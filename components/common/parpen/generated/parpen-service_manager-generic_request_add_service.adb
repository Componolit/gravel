package body Parpen.Service_Manager.Generic_Request_Add_Service with
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
      and then Invalid (Ctx, F_Name)
      and then Invalid (Ctx, F_Server_Kind)
      and then Invalid (Ctx, F_Server_Arity)
      and then Invalid (Ctx, F_Server_Tag)
      and then Invalid (Ctx, F_Server_Legacy_Flags)
      and then Invalid (Ctx, F_Server_Has_Parent)
      and then Invalid (Ctx, F_Server_Flags)
      and then Invalid (Ctx, F_Server_FD)
      and then Invalid (Ctx, F_Server_Num_FDs)
      and then Invalid (Ctx, F_Server_Padding)
      and then Invalid (Ctx, F_Server_Binder)
      and then Invalid (Ctx, F_Server_Handle)
      and then Invalid (Ctx, F_Server_Parent)
      and then Invalid (Ctx, F_Server_Buffer)
      and then Invalid (Ctx, F_Server_Unused_Padding)
      and then Invalid (Ctx, F_Server_Parent_Offset)
      and then Invalid (Ctx, F_Server_Length)
      and then Invalid (Ctx, F_Server_Cookie)
      and then Invalid (Ctx, F_Server_Index)
      and then Invalid (Ctx, F_Padding)
      and then Invalid (Ctx, F_Allow_Isolated)
      and then Invalid (Ctx, F_Dump_Flags));

   procedure Take_Buffer (Ctx : in out Context; Buffer : out Types.Bytes_Ptr) is
   begin
      Buffer := Ctx.Buffer;
      Ctx.Buffer := null;
   end Take_Buffer;

   function Has_Buffer (Ctx : Context) return Boolean is
     (Ctx.Buffer /= null);

   function Message_Last (Ctx : Context) return Types.Bit_Index is
     ((if Structural_Valid (Ctx.Cursors (F_Dump_Flags)) then
       Ctx.Cursors (F_Dump_Flags).Last
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
         when F_Name =>
            (case Fld is
                  when F_Server_Kind =>
                     True,
                  when others =>
                     False),
         when F_Server_Kind =>
            (case Fld is
                  when F_Server_Arity =>
                     True,
                  when others =>
                     False),
         when F_Server_Arity =>
            (case Fld is
                  when F_Server_Tag =>
                     Types.Bit_Length (Ctx.Cursors (F_Server_Arity).Value.Server_Arity_Value) = Types.Bit_Length (Convert (BA_SINGLE))
                        or (Types.Bit_Length (Ctx.Cursors (F_Server_Kind).Value.Server_Kind_Value) = Types.Bit_Length (Convert (BK_FD))
                          and Types.Bit_Length (Ctx.Cursors (F_Server_Arity).Value.Server_Arity_Value) = Types.Bit_Length (Convert (BA_ARRAY))),
                  when others =>
                     False),
         when F_Server_Tag =>
            (case Fld is
                  when F_Server_Legacy_Flags =>
                     Types.Bit_Length (Ctx.Cursors (F_Server_Kind).Value.Server_Kind_Value) = Types.Bit_Length (Convert (BK_FD)),
                  when F_Server_Has_Parent =>
                     Types.Bit_Length (Ctx.Cursors (F_Server_Kind).Value.Server_Kind_Value) = Types.Bit_Length (Convert (BK_POINTER)),
                  when F_Server_Flags =>
                     Types.Bit_Length (Ctx.Cursors (F_Server_Kind).Value.Server_Kind_Value) /= Types.Bit_Length (Convert (BK_POINTER))
                        and Types.Bit_Length (Ctx.Cursors (F_Server_Kind).Value.Server_Kind_Value) /= Types.Bit_Length (Convert (BK_FD)),
                  when others =>
                     False),
         when F_Server_Legacy_Flags =>
            (case Fld is
                  when F_Server_FD =>
                     Types.Bit_Length (Ctx.Cursors (F_Server_Arity).Value.Server_Arity_Value) = Types.Bit_Length (Convert (BA_SINGLE)),
                  when F_Server_Num_FDs =>
                     Types.Bit_Length (Ctx.Cursors (F_Server_Arity).Value.Server_Arity_Value) = Types.Bit_Length (Convert (BA_ARRAY)),
                  when others =>
                     False),
         when F_Server_Has_Parent =>
            (case Fld is
                  when F_Server_Padding =>
                     True,
                  when others =>
                     False),
         when F_Server_Flags =>
            (case Fld is
                  when F_Server_Binder =>
                     Types.Bit_Length (Ctx.Cursors (F_Server_Kind).Value.Server_Kind_Value) = Types.Bit_Length (Convert (BK_STRONG_BINDER))
                        or Types.Bit_Length (Ctx.Cursors (F_Server_Kind).Value.Server_Kind_Value) = Types.Bit_Length (Convert (BK_WEAK_BINDER)),
                  when F_Server_Handle =>
                     Types.Bit_Length (Ctx.Cursors (F_Server_Kind).Value.Server_Kind_Value) = Types.Bit_Length (Convert (BK_STRONG_HANDLE))
                        or Types.Bit_Length (Ctx.Cursors (F_Server_Kind).Value.Server_Kind_Value) = Types.Bit_Length (Convert (BK_WEAK_HANDLE)),
                  when others =>
                     False),
         when F_Server_FD =>
            (case Fld is
                  when F_Server_Cookie =>
                     True,
                  when others =>
                     False),
         when F_Server_Num_FDs =>
            (case Fld is
                  when F_Server_Parent =>
                     True,
                  when others =>
                     False),
         when F_Server_Padding =>
            (case Fld is
                  when F_Server_Buffer =>
                     True,
                  when others =>
                     False),
         when F_Server_Binder =>
            (case Fld is
                  when F_Server_Cookie =>
                     True,
                  when others =>
                     False),
         when F_Server_Handle =>
            (case Fld is
                  when F_Server_Unused_Padding =>
                     True,
                  when others =>
                     False),
         when F_Server_Parent =>
            (case Fld is
                  when F_Server_Parent_Offset =>
                     True,
                  when others =>
                     False),
         when F_Server_Buffer =>
            (case Fld is
                  when F_Server_Length =>
                     True,
                  when others =>
                     False),
         when F_Server_Unused_Padding =>
            (case Fld is
                  when F_Server_Cookie =>
                     True,
                  when others =>
                     False),
         when F_Server_Parent_Offset =>
            (case Fld is
                  when F_Padding =>
                     True,
                  when others =>
                     False),
         when F_Server_Length =>
            (case Fld is
                  when F_Padding =>
                     Types.Bit_Length (Ctx.Cursors (F_Server_Has_Parent).Value.Server_Has_Parent_Value) = Types.Bit_Length (Convert (False)),
                  when F_Server_Index =>
                     Types.Bit_Length (Ctx.Cursors (F_Server_Has_Parent).Value.Server_Has_Parent_Value) = Types.Bit_Length (Convert (True)),
                  when others =>
                     False),
         when F_Server_Cookie | F_Server_Index =>
            (case Fld is
                  when F_Padding =>
                     True,
                  when others =>
                     False),
         when F_Padding =>
            (case Fld is
                  when F_Allow_Isolated =>
                     True,
                  when others =>
                     False),
         when F_Allow_Isolated =>
            (case Fld is
                  when F_Dump_Flags =>
                     True,
                  when others =>
                     False),
         when F_Dump_Flags | F_Final =>
            False));

   function Field_Condition (Ctx : Context; Val : Field_Dependent_Value) return Boolean is
     ((case Val.Fld is
         when F_Initial | F_Len | F_Name | F_Server_Kind =>
            True,
         when F_Server_Arity =>
            Types.Bit_Length (Val.Server_Arity_Value) = Types.Bit_Length (Convert (BA_SINGLE))
               or (Types.Bit_Length (Ctx.Cursors (F_Server_Kind).Value.Server_Kind_Value) = Types.Bit_Length (Convert (BK_FD))
                 and Types.Bit_Length (Val.Server_Arity_Value) = Types.Bit_Length (Convert (BA_ARRAY))),
         when F_Server_Tag =>
            Types.Bit_Length (Ctx.Cursors (F_Server_Kind).Value.Server_Kind_Value) = Types.Bit_Length (Convert (BK_FD))
               or Types.Bit_Length (Ctx.Cursors (F_Server_Kind).Value.Server_Kind_Value) = Types.Bit_Length (Convert (BK_POINTER))
               or (Types.Bit_Length (Ctx.Cursors (F_Server_Kind).Value.Server_Kind_Value) /= Types.Bit_Length (Convert (BK_POINTER))
                 and Types.Bit_Length (Ctx.Cursors (F_Server_Kind).Value.Server_Kind_Value) /= Types.Bit_Length (Convert (BK_FD))),
         when F_Server_Legacy_Flags =>
            Types.Bit_Length (Ctx.Cursors (F_Server_Arity).Value.Server_Arity_Value) = Types.Bit_Length (Convert (BA_SINGLE))
               or Types.Bit_Length (Ctx.Cursors (F_Server_Arity).Value.Server_Arity_Value) = Types.Bit_Length (Convert (BA_ARRAY)),
         when F_Server_Has_Parent =>
            True,
         when F_Server_Flags =>
            Types.Bit_Length (Ctx.Cursors (F_Server_Kind).Value.Server_Kind_Value) = Types.Bit_Length (Convert (BK_STRONG_BINDER))
               or Types.Bit_Length (Ctx.Cursors (F_Server_Kind).Value.Server_Kind_Value) = Types.Bit_Length (Convert (BK_WEAK_BINDER))
               or Types.Bit_Length (Ctx.Cursors (F_Server_Kind).Value.Server_Kind_Value) = Types.Bit_Length (Convert (BK_STRONG_HANDLE))
               or Types.Bit_Length (Ctx.Cursors (F_Server_Kind).Value.Server_Kind_Value) = Types.Bit_Length (Convert (BK_WEAK_HANDLE)),
         when F_Server_FD | F_Server_Num_FDs | F_Server_Padding | F_Server_Binder | F_Server_Handle | F_Server_Parent | F_Server_Buffer | F_Server_Unused_Padding | F_Server_Parent_Offset =>
            True,
         when F_Server_Length =>
            Types.Bit_Length (Ctx.Cursors (F_Server_Has_Parent).Value.Server_Has_Parent_Value) = Types.Bit_Length (Convert (False))
               or Types.Bit_Length (Ctx.Cursors (F_Server_Has_Parent).Value.Server_Has_Parent_Value) = Types.Bit_Length (Convert (True)),
         when F_Server_Cookie | F_Server_Index | F_Padding | F_Allow_Isolated | F_Dump_Flags =>
            True,
         when F_Final =>
            False));

   function Field_Length (Ctx : Context; Fld : Field) return Types.Bit_Length is
     ((case Ctx.Cursors (Fld).Predecessor is
         when F_Initial =>
            (case Fld is
                  when F_Len =>
                     Service_Manager.Len_Base'Size,
                  when others =>
                     Types.Unreachable_Bit_Length),
         when F_Len =>
            (case Fld is
                  when F_Name =>
                     Types.Bit_Length (Ctx.Cursors (F_Len).Value.Len_Value),
                  when others =>
                     Types.Unreachable_Bit_Length),
         when F_Name =>
            (case Fld is
                  when F_Server_Kind =>
                     Binder.Binder_Kind_Base'Size,
                  when others =>
                     Types.Unreachable_Bit_Length),
         when F_Server_Kind =>
            (case Fld is
                  when F_Server_Arity =>
                     Binder.Binder_Arity_Base'Size,
                  when others =>
                     Types.Unreachable_Bit_Length),
         when F_Server_Arity =>
            (case Fld is
                  when F_Server_Tag =>
                     Binder.Binder_Tag_Base'Size,
                  when others =>
                     Types.Unreachable_Bit_Length),
         when F_Server_Tag =>
            (case Fld is
                  when F_Server_Legacy_Flags =>
                     Binder.MBZ32_Base'Size,
                  when F_Server_Has_Parent =>
                     Builtin_Types.Boolean_Base'Size,
                  when F_Server_Flags =>
                     Binder.Flat_Binder_Flags_Base'Size,
                  when others =>
                     Types.Unreachable_Bit_Length),
         when F_Server_Legacy_Flags =>
            (case Fld is
                  when F_Server_FD =>
                     Binder.Handle_Base'Size,
                  when F_Server_Num_FDs =>
                     Binder.Count'Size,
                  when others =>
                     Types.Unreachable_Bit_Length),
         when F_Server_Has_Parent =>
            (case Fld is
                  when F_Server_Padding =>
                     Binder.MBZ31_Base'Size,
                  when others =>
                     Types.Unreachable_Bit_Length),
         when F_Server_Flags =>
            (case Fld is
                  when F_Server_Binder =>
                     Binder.Value'Size,
                  when F_Server_Handle =>
                     Binder.Handle_Base'Size,
                  when others =>
                     Types.Unreachable_Bit_Length),
         when F_Server_FD =>
            (case Fld is
                  when F_Server_Cookie =>
                     Binder.Cookie'Size,
                  when others =>
                     Types.Unreachable_Bit_Length),
         when F_Server_Num_FDs =>
            (case Fld is
                  when F_Server_Parent =>
                     Binder.Index'Size,
                  when others =>
                     Types.Unreachable_Bit_Length),
         when F_Server_Padding =>
            (case Fld is
                  when F_Server_Buffer =>
                     Binder.Index'Size,
                  when others =>
                     Types.Unreachable_Bit_Length),
         when F_Server_Binder =>
            (case Fld is
                  when F_Server_Cookie =>
                     Binder.Cookie'Size,
                  when others =>
                     Types.Unreachable_Bit_Length),
         when F_Server_Handle =>
            (case Fld is
                  when F_Server_Unused_Padding =>
                     Binder.MBZ32_Base'Size,
                  when others =>
                     Types.Unreachable_Bit_Length),
         when F_Server_Parent =>
            (case Fld is
                  when F_Server_Parent_Offset =>
                     Binder.Offset'Size,
                  when others =>
                     Types.Unreachable_Bit_Length),
         when F_Server_Buffer =>
            (case Fld is
                  when F_Server_Length =>
                     Binder.Length_Base'Size,
                  when others =>
                     Types.Unreachable_Bit_Length),
         when F_Server_Unused_Padding =>
            (case Fld is
                  when F_Server_Cookie =>
                     Binder.Cookie'Size,
                  when others =>
                     Types.Unreachable_Bit_Length),
         when F_Server_Parent_Offset =>
            (case Fld is
                  when F_Padding =>
                     Service_Manager.MBZ_7_Base'Size,
                  when others =>
                     Types.Unreachable_Bit_Length),
         when F_Server_Length =>
            (case Fld is
                  when F_Padding =>
                     Service_Manager.MBZ_7_Base'Size,
                  when F_Server_Index =>
                     Binder.Index'Size,
                  when others =>
                     Types.Unreachable_Bit_Length),
         when F_Server_Cookie | F_Server_Index =>
            (case Fld is
                  when F_Padding =>
                     Service_Manager.MBZ_7_Base'Size,
                  when others =>
                     Types.Unreachable_Bit_Length),
         when F_Padding =>
            (case Fld is
                  when F_Allow_Isolated =>
                     Builtin_Types.Boolean_Base'Size,
                  when others =>
                     Types.Unreachable_Bit_Length),
         when F_Allow_Isolated =>
            (case Fld is
                  when F_Dump_Flags =>
                     Service_Manager.Integer_Base'Size,
                  when others =>
                     Types.Unreachable_Bit_Length),
         when F_Dump_Flags | F_Final =>
            0));

   function Field_First (Ctx : Context; Fld : Field) return Types.Bit_Index is
     ((case Fld is
         when F_Len =>
            Ctx.First,
         when F_Name =>
            (if Ctx.Cursors (Fld).Predecessor = F_Len then
                (Ctx.Cursors (Ctx.Cursors (Fld).Predecessor).Last + 1)
             else
                Types.Unreachable_Bit_Length),
         when F_Server_Kind =>
            (if Ctx.Cursors (Fld).Predecessor = F_Name then
                (Ctx.Cursors (Ctx.Cursors (Fld).Predecessor).Last + 1)
             else
                Types.Unreachable_Bit_Length),
         when F_Server_Arity =>
            (if Ctx.Cursors (Fld).Predecessor = F_Server_Kind then
                (Ctx.Cursors (Ctx.Cursors (Fld).Predecessor).Last + 1)
             else
                Types.Unreachable_Bit_Length),
         when F_Server_Tag =>
            (if Ctx.Cursors (Fld).Predecessor = F_Server_Arity
                  and (Types.Bit_Length (Ctx.Cursors (F_Server_Arity).Value.Server_Arity_Value) = Types.Bit_Length (Convert (BA_SINGLE))
                    or (Types.Bit_Length (Ctx.Cursors (F_Server_Kind).Value.Server_Kind_Value) = Types.Bit_Length (Convert (BK_FD))
                      and Types.Bit_Length (Ctx.Cursors (F_Server_Arity).Value.Server_Arity_Value) = Types.Bit_Length (Convert (BA_ARRAY)))) then
                (Ctx.Cursors (Ctx.Cursors (Fld).Predecessor).Last + 1)
             else
                Types.Unreachable_Bit_Length),
         when F_Server_Legacy_Flags =>
            (if Ctx.Cursors (Fld).Predecessor = F_Server_Tag
                  and Types.Bit_Length (Ctx.Cursors (F_Server_Kind).Value.Server_Kind_Value) = Types.Bit_Length (Convert (BK_FD)) then
                (Ctx.Cursors (Ctx.Cursors (Fld).Predecessor).Last + 1)
             else
                Types.Unreachable_Bit_Length),
         when F_Server_Has_Parent =>
            (if Ctx.Cursors (Fld).Predecessor = F_Server_Tag
                  and Types.Bit_Length (Ctx.Cursors (F_Server_Kind).Value.Server_Kind_Value) = Types.Bit_Length (Convert (BK_POINTER)) then
                (Ctx.Cursors (Ctx.Cursors (Fld).Predecessor).Last + 1)
             else
                Types.Unreachable_Bit_Length),
         when F_Server_Flags =>
            (if Ctx.Cursors (Fld).Predecessor = F_Server_Tag
                  and Types.Bit_Length (Ctx.Cursors (F_Server_Kind).Value.Server_Kind_Value) /= Types.Bit_Length (Convert (BK_POINTER))
                  and Types.Bit_Length (Ctx.Cursors (F_Server_Kind).Value.Server_Kind_Value) /= Types.Bit_Length (Convert (BK_FD)) then
                (Ctx.Cursors (Ctx.Cursors (Fld).Predecessor).Last + 1)
             else
                Types.Unreachable_Bit_Length),
         when F_Server_FD =>
            (if Ctx.Cursors (Fld).Predecessor = F_Server_Legacy_Flags
                  and Types.Bit_Length (Ctx.Cursors (F_Server_Arity).Value.Server_Arity_Value) = Types.Bit_Length (Convert (BA_SINGLE)) then
                (Ctx.Cursors (Ctx.Cursors (Fld).Predecessor).Last + 1)
             else
                Types.Unreachable_Bit_Length),
         when F_Server_Num_FDs =>
            (if Ctx.Cursors (Fld).Predecessor = F_Server_Legacy_Flags
                  and Types.Bit_Length (Ctx.Cursors (F_Server_Arity).Value.Server_Arity_Value) = Types.Bit_Length (Convert (BA_ARRAY)) then
                (Ctx.Cursors (Ctx.Cursors (Fld).Predecessor).Last + 1)
             else
                Types.Unreachable_Bit_Length),
         when F_Server_Padding =>
            (if Ctx.Cursors (Fld).Predecessor = F_Server_Has_Parent then
                (Ctx.Cursors (Ctx.Cursors (Fld).Predecessor).Last + 1)
             else
                Types.Unreachable_Bit_Length),
         when F_Server_Binder =>
            (if Ctx.Cursors (Fld).Predecessor = F_Server_Flags
                  and (Types.Bit_Length (Ctx.Cursors (F_Server_Kind).Value.Server_Kind_Value) = Types.Bit_Length (Convert (BK_STRONG_BINDER))
                    or Types.Bit_Length (Ctx.Cursors (F_Server_Kind).Value.Server_Kind_Value) = Types.Bit_Length (Convert (BK_WEAK_BINDER))) then
                (Ctx.Cursors (Ctx.Cursors (Fld).Predecessor).Last + 1)
             else
                Types.Unreachable_Bit_Length),
         when F_Server_Handle =>
            (if Ctx.Cursors (Fld).Predecessor = F_Server_Flags
                  and (Types.Bit_Length (Ctx.Cursors (F_Server_Kind).Value.Server_Kind_Value) = Types.Bit_Length (Convert (BK_STRONG_HANDLE))
                    or Types.Bit_Length (Ctx.Cursors (F_Server_Kind).Value.Server_Kind_Value) = Types.Bit_Length (Convert (BK_WEAK_HANDLE))) then
                (Ctx.Cursors (Ctx.Cursors (Fld).Predecessor).Last + 1)
             else
                Types.Unreachable_Bit_Length),
         when F_Server_Parent =>
            (if Ctx.Cursors (Fld).Predecessor = F_Server_Num_FDs then
                (Ctx.Cursors (Ctx.Cursors (Fld).Predecessor).Last + 1)
             else
                Types.Unreachable_Bit_Length),
         when F_Server_Buffer =>
            (if Ctx.Cursors (Fld).Predecessor = F_Server_Padding then
                (Ctx.Cursors (Ctx.Cursors (Fld).Predecessor).Last + 1)
             else
                Types.Unreachable_Bit_Length),
         when F_Server_Unused_Padding =>
            (if Ctx.Cursors (Fld).Predecessor = F_Server_Handle then
                (Ctx.Cursors (Ctx.Cursors (Fld).Predecessor).Last + 1)
             else
                Types.Unreachable_Bit_Length),
         when F_Server_Parent_Offset =>
            (if Ctx.Cursors (Fld).Predecessor = F_Server_Parent then
                (Ctx.Cursors (Ctx.Cursors (Fld).Predecessor).Last + 1)
             else
                Types.Unreachable_Bit_Length),
         when F_Server_Length =>
            (if Ctx.Cursors (Fld).Predecessor = F_Server_Buffer then
                (Ctx.Cursors (Ctx.Cursors (Fld).Predecessor).Last + 1)
             else
                Types.Unreachable_Bit_Length),
         when F_Server_Cookie =>
            (if Ctx.Cursors (Fld).Predecessor = F_Server_FD then
                (Ctx.Cursors (Ctx.Cursors (Fld).Predecessor).Last + 1)
             elsif Ctx.Cursors (Fld).Predecessor = F_Server_Binder then
                (Ctx.Cursors (Ctx.Cursors (Fld).Predecessor).Last + 1)
             elsif Ctx.Cursors (Fld).Predecessor = F_Server_Unused_Padding then
                (Ctx.Cursors (Ctx.Cursors (Fld).Predecessor).Last + 1)
             else
                Types.Unreachable_Bit_Length),
         when F_Server_Index =>
            (if Ctx.Cursors (Fld).Predecessor = F_Server_Length
                  and Types.Bit_Length (Ctx.Cursors (F_Server_Has_Parent).Value.Server_Has_Parent_Value) = Types.Bit_Length (Convert (True)) then
                (Ctx.Cursors (Ctx.Cursors (Fld).Predecessor).Last + 1)
             else
                Types.Unreachable_Bit_Length),
         when F_Padding =>
            (if Ctx.Cursors (Fld).Predecessor = F_Server_Length
                  and Types.Bit_Length (Ctx.Cursors (F_Server_Has_Parent).Value.Server_Has_Parent_Value) = Types.Bit_Length (Convert (False)) then
                (Ctx.Cursors (Ctx.Cursors (Fld).Predecessor).Last + 1)
             elsif Ctx.Cursors (Fld).Predecessor = F_Server_Index then
                (Ctx.Cursors (Ctx.Cursors (Fld).Predecessor).Last + 1)
             elsif Ctx.Cursors (Fld).Predecessor = F_Server_Parent_Offset then
                (Ctx.Cursors (Ctx.Cursors (Fld).Predecessor).Last + 1)
             elsif Ctx.Cursors (Fld).Predecessor = F_Server_Cookie then
                (Ctx.Cursors (Ctx.Cursors (Fld).Predecessor).Last + 1)
             else
                Types.Unreachable_Bit_Length),
         when F_Allow_Isolated =>
            (if Ctx.Cursors (Fld).Predecessor = F_Padding then
                (Ctx.Cursors (Ctx.Cursors (Fld).Predecessor).Last + 1)
             else
                Types.Unreachable_Bit_Length),
         when F_Dump_Flags =>
            (if Ctx.Cursors (Fld).Predecessor = F_Allow_Isolated then
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
            F_Server_Kind,
         when F_Server_Kind =>
            F_Server_Arity,
         when F_Server_Arity =>
            (if Types.Bit_Length (Ctx.Cursors (F_Server_Arity).Value.Server_Arity_Value) = Types.Bit_Length (Convert (BA_SINGLE))
                  or (Types.Bit_Length (Ctx.Cursors (F_Server_Kind).Value.Server_Kind_Value) = Types.Bit_Length (Convert (BK_FD))
                    and Types.Bit_Length (Ctx.Cursors (F_Server_Arity).Value.Server_Arity_Value) = Types.Bit_Length (Convert (BA_ARRAY))) then
                F_Server_Tag
             else
                F_Initial),
         when F_Server_Tag =>
            (if Types.Bit_Length (Ctx.Cursors (F_Server_Kind).Value.Server_Kind_Value) = Types.Bit_Length (Convert (BK_FD)) then
                F_Server_Legacy_Flags
             elsif Types.Bit_Length (Ctx.Cursors (F_Server_Kind).Value.Server_Kind_Value) = Types.Bit_Length (Convert (BK_POINTER)) then
                F_Server_Has_Parent
             elsif Types.Bit_Length (Ctx.Cursors (F_Server_Kind).Value.Server_Kind_Value) /= Types.Bit_Length (Convert (BK_POINTER))
                     and Types.Bit_Length (Ctx.Cursors (F_Server_Kind).Value.Server_Kind_Value) /= Types.Bit_Length (Convert (BK_FD)) then
                F_Server_Flags
             else
                F_Initial),
         when F_Server_Legacy_Flags =>
            (if Types.Bit_Length (Ctx.Cursors (F_Server_Arity).Value.Server_Arity_Value) = Types.Bit_Length (Convert (BA_SINGLE)) then
                F_Server_FD
             elsif Types.Bit_Length (Ctx.Cursors (F_Server_Arity).Value.Server_Arity_Value) = Types.Bit_Length (Convert (BA_ARRAY)) then
                F_Server_Num_FDs
             else
                F_Initial),
         when F_Server_Has_Parent =>
            F_Server_Padding,
         when F_Server_Flags =>
            (if Types.Bit_Length (Ctx.Cursors (F_Server_Kind).Value.Server_Kind_Value) = Types.Bit_Length (Convert (BK_STRONG_BINDER))
                  or Types.Bit_Length (Ctx.Cursors (F_Server_Kind).Value.Server_Kind_Value) = Types.Bit_Length (Convert (BK_WEAK_BINDER)) then
                F_Server_Binder
             elsif Types.Bit_Length (Ctx.Cursors (F_Server_Kind).Value.Server_Kind_Value) = Types.Bit_Length (Convert (BK_STRONG_HANDLE))
                     or Types.Bit_Length (Ctx.Cursors (F_Server_Kind).Value.Server_Kind_Value) = Types.Bit_Length (Convert (BK_WEAK_HANDLE)) then
                F_Server_Handle
             else
                F_Initial),
         when F_Server_FD =>
            F_Server_Cookie,
         when F_Server_Num_FDs =>
            F_Server_Parent,
         when F_Server_Padding =>
            F_Server_Buffer,
         when F_Server_Binder =>
            F_Server_Cookie,
         when F_Server_Handle =>
            F_Server_Unused_Padding,
         when F_Server_Parent =>
            F_Server_Parent_Offset,
         when F_Server_Buffer =>
            F_Server_Length,
         when F_Server_Unused_Padding =>
            F_Server_Cookie,
         when F_Server_Parent_Offset =>
            F_Padding,
         when F_Server_Length =>
            (if Types.Bit_Length (Ctx.Cursors (F_Server_Has_Parent).Value.Server_Has_Parent_Value) = Types.Bit_Length (Convert (False)) then
                F_Padding
             elsif Types.Bit_Length (Ctx.Cursors (F_Server_Has_Parent).Value.Server_Has_Parent_Value) = Types.Bit_Length (Convert (True)) then
                F_Server_Index
             else
                F_Initial),
         when F_Server_Cookie | F_Server_Index =>
            F_Padding,
         when F_Padding =>
            F_Allow_Isolated,
         when F_Allow_Isolated =>
            F_Dump_Flags,
         when F_Dump_Flags =>
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
         when F_Server_Kind =>
            (Structural_Valid (Ctx.Cursors (F_Name))
                 and Ctx.Cursors (Fld).Predecessor = F_Name),
         when F_Server_Arity =>
            (Valid (Ctx.Cursors (F_Server_Kind))
                 and Ctx.Cursors (Fld).Predecessor = F_Server_Kind),
         when F_Server_Tag =>
            (Valid (Ctx.Cursors (F_Server_Arity))
                 and Ctx.Cursors (Fld).Predecessor = F_Server_Arity),
         when F_Server_Legacy_Flags | F_Server_Has_Parent | F_Server_Flags =>
            (Valid (Ctx.Cursors (F_Server_Tag))
                 and Ctx.Cursors (Fld).Predecessor = F_Server_Tag),
         when F_Server_FD | F_Server_Num_FDs =>
            (Valid (Ctx.Cursors (F_Server_Legacy_Flags))
                 and Ctx.Cursors (Fld).Predecessor = F_Server_Legacy_Flags),
         when F_Server_Padding =>
            (Valid (Ctx.Cursors (F_Server_Has_Parent))
                 and Ctx.Cursors (Fld).Predecessor = F_Server_Has_Parent),
         when F_Server_Binder | F_Server_Handle =>
            (Valid (Ctx.Cursors (F_Server_Flags))
                 and Ctx.Cursors (Fld).Predecessor = F_Server_Flags),
         when F_Server_Parent =>
            (Valid (Ctx.Cursors (F_Server_Num_FDs))
                 and Ctx.Cursors (Fld).Predecessor = F_Server_Num_FDs),
         when F_Server_Buffer =>
            (Valid (Ctx.Cursors (F_Server_Padding))
                 and Ctx.Cursors (Fld).Predecessor = F_Server_Padding),
         when F_Server_Unused_Padding =>
            (Valid (Ctx.Cursors (F_Server_Handle))
                 and Ctx.Cursors (Fld).Predecessor = F_Server_Handle),
         when F_Server_Parent_Offset =>
            (Valid (Ctx.Cursors (F_Server_Parent))
                 and Ctx.Cursors (Fld).Predecessor = F_Server_Parent),
         when F_Server_Length =>
            (Valid (Ctx.Cursors (F_Server_Buffer))
                 and Ctx.Cursors (Fld).Predecessor = F_Server_Buffer),
         when F_Server_Cookie =>
            (Valid (Ctx.Cursors (F_Server_FD))
                 and Ctx.Cursors (Fld).Predecessor = F_Server_FD)
               or (Valid (Ctx.Cursors (F_Server_Binder))
                 and Ctx.Cursors (Fld).Predecessor = F_Server_Binder)
               or (Valid (Ctx.Cursors (F_Server_Unused_Padding))
                 and Ctx.Cursors (Fld).Predecessor = F_Server_Unused_Padding),
         when F_Server_Index =>
            (Valid (Ctx.Cursors (F_Server_Length))
                 and Ctx.Cursors (Fld).Predecessor = F_Server_Length),
         when F_Padding =>
            (Valid (Ctx.Cursors (F_Server_Length))
                 and Ctx.Cursors (Fld).Predecessor = F_Server_Length)
               or (Valid (Ctx.Cursors (F_Server_Index))
                 and Ctx.Cursors (Fld).Predecessor = F_Server_Index)
               or (Valid (Ctx.Cursors (F_Server_Parent_Offset))
                 and Ctx.Cursors (Fld).Predecessor = F_Server_Parent_Offset)
               or (Valid (Ctx.Cursors (F_Server_Cookie))
                 and Ctx.Cursors (Fld).Predecessor = F_Server_Cookie),
         when F_Allow_Isolated =>
            (Valid (Ctx.Cursors (F_Padding))
                 and Ctx.Cursors (Fld).Predecessor = F_Padding),
         when F_Dump_Flags =>
            (Valid (Ctx.Cursors (F_Allow_Isolated))
                 and Ctx.Cursors (Fld).Predecessor = F_Allow_Isolated),
         when F_Final =>
            (Valid (Ctx.Cursors (F_Dump_Flags))
                 and Ctx.Cursors (Fld).Predecessor = F_Dump_Flags)));

   function Invalid_Successor (Ctx : Context; Fld : Field) return Boolean is
     ((case Fld is
         when F_Len =>
            Invalid (Ctx.Cursors (F_Name)),
         when F_Name =>
            Invalid (Ctx.Cursors (F_Server_Kind)),
         when F_Server_Kind =>
            Invalid (Ctx.Cursors (F_Server_Arity)),
         when F_Server_Arity =>
            Invalid (Ctx.Cursors (F_Server_Tag)),
         when F_Server_Tag =>
            Invalid (Ctx.Cursors (F_Server_Legacy_Flags))
               and Invalid (Ctx.Cursors (F_Server_Has_Parent))
               and Invalid (Ctx.Cursors (F_Server_Flags)),
         when F_Server_Legacy_Flags =>
            Invalid (Ctx.Cursors (F_Server_FD))
               and Invalid (Ctx.Cursors (F_Server_Num_FDs)),
         when F_Server_Has_Parent =>
            Invalid (Ctx.Cursors (F_Server_Padding)),
         when F_Server_Flags =>
            Invalid (Ctx.Cursors (F_Server_Binder))
               and Invalid (Ctx.Cursors (F_Server_Handle)),
         when F_Server_FD =>
            Invalid (Ctx.Cursors (F_Server_Cookie)),
         when F_Server_Num_FDs =>
            Invalid (Ctx.Cursors (F_Server_Parent)),
         when F_Server_Padding =>
            Invalid (Ctx.Cursors (F_Server_Buffer)),
         when F_Server_Binder =>
            Invalid (Ctx.Cursors (F_Server_Cookie)),
         when F_Server_Handle =>
            Invalid (Ctx.Cursors (F_Server_Unused_Padding)),
         when F_Server_Parent =>
            Invalid (Ctx.Cursors (F_Server_Parent_Offset)),
         when F_Server_Buffer =>
            Invalid (Ctx.Cursors (F_Server_Length)),
         when F_Server_Unused_Padding =>
            Invalid (Ctx.Cursors (F_Server_Cookie)),
         when F_Server_Parent_Offset =>
            Invalid (Ctx.Cursors (F_Padding)),
         when F_Server_Length =>
            Invalid (Ctx.Cursors (F_Padding))
               and Invalid (Ctx.Cursors (F_Server_Index)),
         when F_Server_Cookie | F_Server_Index =>
            Invalid (Ctx.Cursors (F_Padding)),
         when F_Padding =>
            Invalid (Ctx.Cursors (F_Allow_Isolated)),
         when F_Allow_Isolated =>
            Invalid (Ctx.Cursors (F_Dump_Flags)),
         when F_Dump_Flags =>
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
                     and Invalid (Ctx, F_Name)
                     and Invalid (Ctx, F_Server_Kind)
                     and Invalid (Ctx, F_Server_Arity)
                     and Invalid (Ctx, F_Server_Tag)
                     and Invalid (Ctx, F_Server_Legacy_Flags)
                     and Invalid (Ctx, F_Server_Has_Parent)
                     and Invalid (Ctx, F_Server_Flags)
                     and Invalid (Ctx, F_Server_FD)
                     and Invalid (Ctx, F_Server_Num_FDs)
                     and Invalid (Ctx, F_Server_Padding)
                     and Invalid (Ctx, F_Server_Binder)
                     and Invalid (Ctx, F_Server_Handle)
                     and Invalid (Ctx, F_Server_Parent)
                     and Invalid (Ctx, F_Server_Buffer)
                     and Invalid (Ctx, F_Server_Unused_Padding)
                     and Invalid (Ctx, F_Server_Parent_Offset)
                     and Invalid (Ctx, F_Server_Length)
                     and Invalid (Ctx, F_Server_Cookie)
                     and Invalid (Ctx, F_Server_Index)
                     and Invalid (Ctx, F_Padding)
                     and Invalid (Ctx, F_Allow_Isolated)
                     and Invalid (Ctx, F_Dump_Flags),
               when F_Name =>
                  Ctx.Cursors (F_Len) = Ctx.Cursors (F_Len)'Old
                     and Invalid (Ctx, F_Name)
                     and Invalid (Ctx, F_Server_Kind)
                     and Invalid (Ctx, F_Server_Arity)
                     and Invalid (Ctx, F_Server_Tag)
                     and Invalid (Ctx, F_Server_Legacy_Flags)
                     and Invalid (Ctx, F_Server_Has_Parent)
                     and Invalid (Ctx, F_Server_Flags)
                     and Invalid (Ctx, F_Server_FD)
                     and Invalid (Ctx, F_Server_Num_FDs)
                     and Invalid (Ctx, F_Server_Padding)
                     and Invalid (Ctx, F_Server_Binder)
                     and Invalid (Ctx, F_Server_Handle)
                     and Invalid (Ctx, F_Server_Parent)
                     and Invalid (Ctx, F_Server_Buffer)
                     and Invalid (Ctx, F_Server_Unused_Padding)
                     and Invalid (Ctx, F_Server_Parent_Offset)
                     and Invalid (Ctx, F_Server_Length)
                     and Invalid (Ctx, F_Server_Cookie)
                     and Invalid (Ctx, F_Server_Index)
                     and Invalid (Ctx, F_Padding)
                     and Invalid (Ctx, F_Allow_Isolated)
                     and Invalid (Ctx, F_Dump_Flags),
               when F_Server_Kind =>
                  Ctx.Cursors (F_Len) = Ctx.Cursors (F_Len)'Old
                     and Ctx.Cursors (F_Name) = Ctx.Cursors (F_Name)'Old
                     and Invalid (Ctx, F_Server_Kind)
                     and Invalid (Ctx, F_Server_Arity)
                     and Invalid (Ctx, F_Server_Tag)
                     and Invalid (Ctx, F_Server_Legacy_Flags)
                     and Invalid (Ctx, F_Server_Has_Parent)
                     and Invalid (Ctx, F_Server_Flags)
                     and Invalid (Ctx, F_Server_FD)
                     and Invalid (Ctx, F_Server_Num_FDs)
                     and Invalid (Ctx, F_Server_Padding)
                     and Invalid (Ctx, F_Server_Binder)
                     and Invalid (Ctx, F_Server_Handle)
                     and Invalid (Ctx, F_Server_Parent)
                     and Invalid (Ctx, F_Server_Buffer)
                     and Invalid (Ctx, F_Server_Unused_Padding)
                     and Invalid (Ctx, F_Server_Parent_Offset)
                     and Invalid (Ctx, F_Server_Length)
                     and Invalid (Ctx, F_Server_Cookie)
                     and Invalid (Ctx, F_Server_Index)
                     and Invalid (Ctx, F_Padding)
                     and Invalid (Ctx, F_Allow_Isolated)
                     and Invalid (Ctx, F_Dump_Flags),
               when F_Server_Arity =>
                  Ctx.Cursors (F_Len) = Ctx.Cursors (F_Len)'Old
                     and Ctx.Cursors (F_Name) = Ctx.Cursors (F_Name)'Old
                     and Ctx.Cursors (F_Server_Kind) = Ctx.Cursors (F_Server_Kind)'Old
                     and Invalid (Ctx, F_Server_Arity)
                     and Invalid (Ctx, F_Server_Tag)
                     and Invalid (Ctx, F_Server_Legacy_Flags)
                     and Invalid (Ctx, F_Server_Has_Parent)
                     and Invalid (Ctx, F_Server_Flags)
                     and Invalid (Ctx, F_Server_FD)
                     and Invalid (Ctx, F_Server_Num_FDs)
                     and Invalid (Ctx, F_Server_Padding)
                     and Invalid (Ctx, F_Server_Binder)
                     and Invalid (Ctx, F_Server_Handle)
                     and Invalid (Ctx, F_Server_Parent)
                     and Invalid (Ctx, F_Server_Buffer)
                     and Invalid (Ctx, F_Server_Unused_Padding)
                     and Invalid (Ctx, F_Server_Parent_Offset)
                     and Invalid (Ctx, F_Server_Length)
                     and Invalid (Ctx, F_Server_Cookie)
                     and Invalid (Ctx, F_Server_Index)
                     and Invalid (Ctx, F_Padding)
                     and Invalid (Ctx, F_Allow_Isolated)
                     and Invalid (Ctx, F_Dump_Flags),
               when F_Server_Tag =>
                  Ctx.Cursors (F_Len) = Ctx.Cursors (F_Len)'Old
                     and Ctx.Cursors (F_Name) = Ctx.Cursors (F_Name)'Old
                     and Ctx.Cursors (F_Server_Kind) = Ctx.Cursors (F_Server_Kind)'Old
                     and Ctx.Cursors (F_Server_Arity) = Ctx.Cursors (F_Server_Arity)'Old
                     and Invalid (Ctx, F_Server_Tag)
                     and Invalid (Ctx, F_Server_Legacy_Flags)
                     and Invalid (Ctx, F_Server_Has_Parent)
                     and Invalid (Ctx, F_Server_Flags)
                     and Invalid (Ctx, F_Server_FD)
                     and Invalid (Ctx, F_Server_Num_FDs)
                     and Invalid (Ctx, F_Server_Padding)
                     and Invalid (Ctx, F_Server_Binder)
                     and Invalid (Ctx, F_Server_Handle)
                     and Invalid (Ctx, F_Server_Parent)
                     and Invalid (Ctx, F_Server_Buffer)
                     and Invalid (Ctx, F_Server_Unused_Padding)
                     and Invalid (Ctx, F_Server_Parent_Offset)
                     and Invalid (Ctx, F_Server_Length)
                     and Invalid (Ctx, F_Server_Cookie)
                     and Invalid (Ctx, F_Server_Index)
                     and Invalid (Ctx, F_Padding)
                     and Invalid (Ctx, F_Allow_Isolated)
                     and Invalid (Ctx, F_Dump_Flags),
               when F_Server_Legacy_Flags =>
                  Ctx.Cursors (F_Len) = Ctx.Cursors (F_Len)'Old
                     and Ctx.Cursors (F_Name) = Ctx.Cursors (F_Name)'Old
                     and Ctx.Cursors (F_Server_Kind) = Ctx.Cursors (F_Server_Kind)'Old
                     and Ctx.Cursors (F_Server_Arity) = Ctx.Cursors (F_Server_Arity)'Old
                     and Ctx.Cursors (F_Server_Tag) = Ctx.Cursors (F_Server_Tag)'Old
                     and Invalid (Ctx, F_Server_Legacy_Flags)
                     and Invalid (Ctx, F_Server_Has_Parent)
                     and Invalid (Ctx, F_Server_Flags)
                     and Invalid (Ctx, F_Server_FD)
                     and Invalid (Ctx, F_Server_Num_FDs)
                     and Invalid (Ctx, F_Server_Padding)
                     and Invalid (Ctx, F_Server_Binder)
                     and Invalid (Ctx, F_Server_Handle)
                     and Invalid (Ctx, F_Server_Parent)
                     and Invalid (Ctx, F_Server_Buffer)
                     and Invalid (Ctx, F_Server_Unused_Padding)
                     and Invalid (Ctx, F_Server_Parent_Offset)
                     and Invalid (Ctx, F_Server_Length)
                     and Invalid (Ctx, F_Server_Cookie)
                     and Invalid (Ctx, F_Server_Index)
                     and Invalid (Ctx, F_Padding)
                     and Invalid (Ctx, F_Allow_Isolated)
                     and Invalid (Ctx, F_Dump_Flags),
               when F_Server_Has_Parent =>
                  Ctx.Cursors (F_Len) = Ctx.Cursors (F_Len)'Old
                     and Ctx.Cursors (F_Name) = Ctx.Cursors (F_Name)'Old
                     and Ctx.Cursors (F_Server_Kind) = Ctx.Cursors (F_Server_Kind)'Old
                     and Ctx.Cursors (F_Server_Arity) = Ctx.Cursors (F_Server_Arity)'Old
                     and Ctx.Cursors (F_Server_Tag) = Ctx.Cursors (F_Server_Tag)'Old
                     and Ctx.Cursors (F_Server_Legacy_Flags) = Ctx.Cursors (F_Server_Legacy_Flags)'Old
                     and Invalid (Ctx, F_Server_Has_Parent)
                     and Invalid (Ctx, F_Server_Flags)
                     and Invalid (Ctx, F_Server_FD)
                     and Invalid (Ctx, F_Server_Num_FDs)
                     and Invalid (Ctx, F_Server_Padding)
                     and Invalid (Ctx, F_Server_Binder)
                     and Invalid (Ctx, F_Server_Handle)
                     and Invalid (Ctx, F_Server_Parent)
                     and Invalid (Ctx, F_Server_Buffer)
                     and Invalid (Ctx, F_Server_Unused_Padding)
                     and Invalid (Ctx, F_Server_Parent_Offset)
                     and Invalid (Ctx, F_Server_Length)
                     and Invalid (Ctx, F_Server_Cookie)
                     and Invalid (Ctx, F_Server_Index)
                     and Invalid (Ctx, F_Padding)
                     and Invalid (Ctx, F_Allow_Isolated)
                     and Invalid (Ctx, F_Dump_Flags),
               when F_Server_Flags =>
                  Ctx.Cursors (F_Len) = Ctx.Cursors (F_Len)'Old
                     and Ctx.Cursors (F_Name) = Ctx.Cursors (F_Name)'Old
                     and Ctx.Cursors (F_Server_Kind) = Ctx.Cursors (F_Server_Kind)'Old
                     and Ctx.Cursors (F_Server_Arity) = Ctx.Cursors (F_Server_Arity)'Old
                     and Ctx.Cursors (F_Server_Tag) = Ctx.Cursors (F_Server_Tag)'Old
                     and Ctx.Cursors (F_Server_Legacy_Flags) = Ctx.Cursors (F_Server_Legacy_Flags)'Old
                     and Ctx.Cursors (F_Server_Has_Parent) = Ctx.Cursors (F_Server_Has_Parent)'Old
                     and Invalid (Ctx, F_Server_Flags)
                     and Invalid (Ctx, F_Server_FD)
                     and Invalid (Ctx, F_Server_Num_FDs)
                     and Invalid (Ctx, F_Server_Padding)
                     and Invalid (Ctx, F_Server_Binder)
                     and Invalid (Ctx, F_Server_Handle)
                     and Invalid (Ctx, F_Server_Parent)
                     and Invalid (Ctx, F_Server_Buffer)
                     and Invalid (Ctx, F_Server_Unused_Padding)
                     and Invalid (Ctx, F_Server_Parent_Offset)
                     and Invalid (Ctx, F_Server_Length)
                     and Invalid (Ctx, F_Server_Cookie)
                     and Invalid (Ctx, F_Server_Index)
                     and Invalid (Ctx, F_Padding)
                     and Invalid (Ctx, F_Allow_Isolated)
                     and Invalid (Ctx, F_Dump_Flags),
               when F_Server_FD =>
                  Ctx.Cursors (F_Len) = Ctx.Cursors (F_Len)'Old
                     and Ctx.Cursors (F_Name) = Ctx.Cursors (F_Name)'Old
                     and Ctx.Cursors (F_Server_Kind) = Ctx.Cursors (F_Server_Kind)'Old
                     and Ctx.Cursors (F_Server_Arity) = Ctx.Cursors (F_Server_Arity)'Old
                     and Ctx.Cursors (F_Server_Tag) = Ctx.Cursors (F_Server_Tag)'Old
                     and Ctx.Cursors (F_Server_Legacy_Flags) = Ctx.Cursors (F_Server_Legacy_Flags)'Old
                     and Ctx.Cursors (F_Server_Has_Parent) = Ctx.Cursors (F_Server_Has_Parent)'Old
                     and Ctx.Cursors (F_Server_Flags) = Ctx.Cursors (F_Server_Flags)'Old
                     and Invalid (Ctx, F_Server_FD)
                     and Invalid (Ctx, F_Server_Num_FDs)
                     and Invalid (Ctx, F_Server_Padding)
                     and Invalid (Ctx, F_Server_Binder)
                     and Invalid (Ctx, F_Server_Handle)
                     and Invalid (Ctx, F_Server_Parent)
                     and Invalid (Ctx, F_Server_Buffer)
                     and Invalid (Ctx, F_Server_Unused_Padding)
                     and Invalid (Ctx, F_Server_Parent_Offset)
                     and Invalid (Ctx, F_Server_Length)
                     and Invalid (Ctx, F_Server_Cookie)
                     and Invalid (Ctx, F_Server_Index)
                     and Invalid (Ctx, F_Padding)
                     and Invalid (Ctx, F_Allow_Isolated)
                     and Invalid (Ctx, F_Dump_Flags),
               when F_Server_Num_FDs =>
                  Ctx.Cursors (F_Len) = Ctx.Cursors (F_Len)'Old
                     and Ctx.Cursors (F_Name) = Ctx.Cursors (F_Name)'Old
                     and Ctx.Cursors (F_Server_Kind) = Ctx.Cursors (F_Server_Kind)'Old
                     and Ctx.Cursors (F_Server_Arity) = Ctx.Cursors (F_Server_Arity)'Old
                     and Ctx.Cursors (F_Server_Tag) = Ctx.Cursors (F_Server_Tag)'Old
                     and Ctx.Cursors (F_Server_Legacy_Flags) = Ctx.Cursors (F_Server_Legacy_Flags)'Old
                     and Ctx.Cursors (F_Server_Has_Parent) = Ctx.Cursors (F_Server_Has_Parent)'Old
                     and Ctx.Cursors (F_Server_Flags) = Ctx.Cursors (F_Server_Flags)'Old
                     and Ctx.Cursors (F_Server_FD) = Ctx.Cursors (F_Server_FD)'Old
                     and Invalid (Ctx, F_Server_Num_FDs)
                     and Invalid (Ctx, F_Server_Padding)
                     and Invalid (Ctx, F_Server_Binder)
                     and Invalid (Ctx, F_Server_Handle)
                     and Invalid (Ctx, F_Server_Parent)
                     and Invalid (Ctx, F_Server_Buffer)
                     and Invalid (Ctx, F_Server_Unused_Padding)
                     and Invalid (Ctx, F_Server_Parent_Offset)
                     and Invalid (Ctx, F_Server_Length)
                     and Invalid (Ctx, F_Server_Cookie)
                     and Invalid (Ctx, F_Server_Index)
                     and Invalid (Ctx, F_Padding)
                     and Invalid (Ctx, F_Allow_Isolated)
                     and Invalid (Ctx, F_Dump_Flags),
               when F_Server_Padding =>
                  Ctx.Cursors (F_Len) = Ctx.Cursors (F_Len)'Old
                     and Ctx.Cursors (F_Name) = Ctx.Cursors (F_Name)'Old
                     and Ctx.Cursors (F_Server_Kind) = Ctx.Cursors (F_Server_Kind)'Old
                     and Ctx.Cursors (F_Server_Arity) = Ctx.Cursors (F_Server_Arity)'Old
                     and Ctx.Cursors (F_Server_Tag) = Ctx.Cursors (F_Server_Tag)'Old
                     and Ctx.Cursors (F_Server_Legacy_Flags) = Ctx.Cursors (F_Server_Legacy_Flags)'Old
                     and Ctx.Cursors (F_Server_Has_Parent) = Ctx.Cursors (F_Server_Has_Parent)'Old
                     and Ctx.Cursors (F_Server_Flags) = Ctx.Cursors (F_Server_Flags)'Old
                     and Ctx.Cursors (F_Server_FD) = Ctx.Cursors (F_Server_FD)'Old
                     and Ctx.Cursors (F_Server_Num_FDs) = Ctx.Cursors (F_Server_Num_FDs)'Old
                     and Invalid (Ctx, F_Server_Padding)
                     and Invalid (Ctx, F_Server_Binder)
                     and Invalid (Ctx, F_Server_Handle)
                     and Invalid (Ctx, F_Server_Parent)
                     and Invalid (Ctx, F_Server_Buffer)
                     and Invalid (Ctx, F_Server_Unused_Padding)
                     and Invalid (Ctx, F_Server_Parent_Offset)
                     and Invalid (Ctx, F_Server_Length)
                     and Invalid (Ctx, F_Server_Cookie)
                     and Invalid (Ctx, F_Server_Index)
                     and Invalid (Ctx, F_Padding)
                     and Invalid (Ctx, F_Allow_Isolated)
                     and Invalid (Ctx, F_Dump_Flags),
               when F_Server_Binder =>
                  Ctx.Cursors (F_Len) = Ctx.Cursors (F_Len)'Old
                     and Ctx.Cursors (F_Name) = Ctx.Cursors (F_Name)'Old
                     and Ctx.Cursors (F_Server_Kind) = Ctx.Cursors (F_Server_Kind)'Old
                     and Ctx.Cursors (F_Server_Arity) = Ctx.Cursors (F_Server_Arity)'Old
                     and Ctx.Cursors (F_Server_Tag) = Ctx.Cursors (F_Server_Tag)'Old
                     and Ctx.Cursors (F_Server_Legacy_Flags) = Ctx.Cursors (F_Server_Legacy_Flags)'Old
                     and Ctx.Cursors (F_Server_Has_Parent) = Ctx.Cursors (F_Server_Has_Parent)'Old
                     and Ctx.Cursors (F_Server_Flags) = Ctx.Cursors (F_Server_Flags)'Old
                     and Ctx.Cursors (F_Server_FD) = Ctx.Cursors (F_Server_FD)'Old
                     and Ctx.Cursors (F_Server_Num_FDs) = Ctx.Cursors (F_Server_Num_FDs)'Old
                     and Ctx.Cursors (F_Server_Padding) = Ctx.Cursors (F_Server_Padding)'Old
                     and Invalid (Ctx, F_Server_Binder)
                     and Invalid (Ctx, F_Server_Handle)
                     and Invalid (Ctx, F_Server_Parent)
                     and Invalid (Ctx, F_Server_Buffer)
                     and Invalid (Ctx, F_Server_Unused_Padding)
                     and Invalid (Ctx, F_Server_Parent_Offset)
                     and Invalid (Ctx, F_Server_Length)
                     and Invalid (Ctx, F_Server_Cookie)
                     and Invalid (Ctx, F_Server_Index)
                     and Invalid (Ctx, F_Padding)
                     and Invalid (Ctx, F_Allow_Isolated)
                     and Invalid (Ctx, F_Dump_Flags),
               when F_Server_Handle =>
                  Ctx.Cursors (F_Len) = Ctx.Cursors (F_Len)'Old
                     and Ctx.Cursors (F_Name) = Ctx.Cursors (F_Name)'Old
                     and Ctx.Cursors (F_Server_Kind) = Ctx.Cursors (F_Server_Kind)'Old
                     and Ctx.Cursors (F_Server_Arity) = Ctx.Cursors (F_Server_Arity)'Old
                     and Ctx.Cursors (F_Server_Tag) = Ctx.Cursors (F_Server_Tag)'Old
                     and Ctx.Cursors (F_Server_Legacy_Flags) = Ctx.Cursors (F_Server_Legacy_Flags)'Old
                     and Ctx.Cursors (F_Server_Has_Parent) = Ctx.Cursors (F_Server_Has_Parent)'Old
                     and Ctx.Cursors (F_Server_Flags) = Ctx.Cursors (F_Server_Flags)'Old
                     and Ctx.Cursors (F_Server_FD) = Ctx.Cursors (F_Server_FD)'Old
                     and Ctx.Cursors (F_Server_Num_FDs) = Ctx.Cursors (F_Server_Num_FDs)'Old
                     and Ctx.Cursors (F_Server_Padding) = Ctx.Cursors (F_Server_Padding)'Old
                     and Ctx.Cursors (F_Server_Binder) = Ctx.Cursors (F_Server_Binder)'Old
                     and Invalid (Ctx, F_Server_Handle)
                     and Invalid (Ctx, F_Server_Parent)
                     and Invalid (Ctx, F_Server_Buffer)
                     and Invalid (Ctx, F_Server_Unused_Padding)
                     and Invalid (Ctx, F_Server_Parent_Offset)
                     and Invalid (Ctx, F_Server_Length)
                     and Invalid (Ctx, F_Server_Cookie)
                     and Invalid (Ctx, F_Server_Index)
                     and Invalid (Ctx, F_Padding)
                     and Invalid (Ctx, F_Allow_Isolated)
                     and Invalid (Ctx, F_Dump_Flags),
               when F_Server_Parent =>
                  Ctx.Cursors (F_Len) = Ctx.Cursors (F_Len)'Old
                     and Ctx.Cursors (F_Name) = Ctx.Cursors (F_Name)'Old
                     and Ctx.Cursors (F_Server_Kind) = Ctx.Cursors (F_Server_Kind)'Old
                     and Ctx.Cursors (F_Server_Arity) = Ctx.Cursors (F_Server_Arity)'Old
                     and Ctx.Cursors (F_Server_Tag) = Ctx.Cursors (F_Server_Tag)'Old
                     and Ctx.Cursors (F_Server_Legacy_Flags) = Ctx.Cursors (F_Server_Legacy_Flags)'Old
                     and Ctx.Cursors (F_Server_Has_Parent) = Ctx.Cursors (F_Server_Has_Parent)'Old
                     and Ctx.Cursors (F_Server_Flags) = Ctx.Cursors (F_Server_Flags)'Old
                     and Ctx.Cursors (F_Server_FD) = Ctx.Cursors (F_Server_FD)'Old
                     and Ctx.Cursors (F_Server_Num_FDs) = Ctx.Cursors (F_Server_Num_FDs)'Old
                     and Ctx.Cursors (F_Server_Padding) = Ctx.Cursors (F_Server_Padding)'Old
                     and Ctx.Cursors (F_Server_Binder) = Ctx.Cursors (F_Server_Binder)'Old
                     and Ctx.Cursors (F_Server_Handle) = Ctx.Cursors (F_Server_Handle)'Old
                     and Invalid (Ctx, F_Server_Parent)
                     and Invalid (Ctx, F_Server_Buffer)
                     and Invalid (Ctx, F_Server_Unused_Padding)
                     and Invalid (Ctx, F_Server_Parent_Offset)
                     and Invalid (Ctx, F_Server_Length)
                     and Invalid (Ctx, F_Server_Cookie)
                     and Invalid (Ctx, F_Server_Index)
                     and Invalid (Ctx, F_Padding)
                     and Invalid (Ctx, F_Allow_Isolated)
                     and Invalid (Ctx, F_Dump_Flags),
               when F_Server_Buffer =>
                  Ctx.Cursors (F_Len) = Ctx.Cursors (F_Len)'Old
                     and Ctx.Cursors (F_Name) = Ctx.Cursors (F_Name)'Old
                     and Ctx.Cursors (F_Server_Kind) = Ctx.Cursors (F_Server_Kind)'Old
                     and Ctx.Cursors (F_Server_Arity) = Ctx.Cursors (F_Server_Arity)'Old
                     and Ctx.Cursors (F_Server_Tag) = Ctx.Cursors (F_Server_Tag)'Old
                     and Ctx.Cursors (F_Server_Legacy_Flags) = Ctx.Cursors (F_Server_Legacy_Flags)'Old
                     and Ctx.Cursors (F_Server_Has_Parent) = Ctx.Cursors (F_Server_Has_Parent)'Old
                     and Ctx.Cursors (F_Server_Flags) = Ctx.Cursors (F_Server_Flags)'Old
                     and Ctx.Cursors (F_Server_FD) = Ctx.Cursors (F_Server_FD)'Old
                     and Ctx.Cursors (F_Server_Num_FDs) = Ctx.Cursors (F_Server_Num_FDs)'Old
                     and Ctx.Cursors (F_Server_Padding) = Ctx.Cursors (F_Server_Padding)'Old
                     and Ctx.Cursors (F_Server_Binder) = Ctx.Cursors (F_Server_Binder)'Old
                     and Ctx.Cursors (F_Server_Handle) = Ctx.Cursors (F_Server_Handle)'Old
                     and Ctx.Cursors (F_Server_Parent) = Ctx.Cursors (F_Server_Parent)'Old
                     and Invalid (Ctx, F_Server_Buffer)
                     and Invalid (Ctx, F_Server_Unused_Padding)
                     and Invalid (Ctx, F_Server_Parent_Offset)
                     and Invalid (Ctx, F_Server_Length)
                     and Invalid (Ctx, F_Server_Cookie)
                     and Invalid (Ctx, F_Server_Index)
                     and Invalid (Ctx, F_Padding)
                     and Invalid (Ctx, F_Allow_Isolated)
                     and Invalid (Ctx, F_Dump_Flags),
               when F_Server_Unused_Padding =>
                  Ctx.Cursors (F_Len) = Ctx.Cursors (F_Len)'Old
                     and Ctx.Cursors (F_Name) = Ctx.Cursors (F_Name)'Old
                     and Ctx.Cursors (F_Server_Kind) = Ctx.Cursors (F_Server_Kind)'Old
                     and Ctx.Cursors (F_Server_Arity) = Ctx.Cursors (F_Server_Arity)'Old
                     and Ctx.Cursors (F_Server_Tag) = Ctx.Cursors (F_Server_Tag)'Old
                     and Ctx.Cursors (F_Server_Legacy_Flags) = Ctx.Cursors (F_Server_Legacy_Flags)'Old
                     and Ctx.Cursors (F_Server_Has_Parent) = Ctx.Cursors (F_Server_Has_Parent)'Old
                     and Ctx.Cursors (F_Server_Flags) = Ctx.Cursors (F_Server_Flags)'Old
                     and Ctx.Cursors (F_Server_FD) = Ctx.Cursors (F_Server_FD)'Old
                     and Ctx.Cursors (F_Server_Num_FDs) = Ctx.Cursors (F_Server_Num_FDs)'Old
                     and Ctx.Cursors (F_Server_Padding) = Ctx.Cursors (F_Server_Padding)'Old
                     and Ctx.Cursors (F_Server_Binder) = Ctx.Cursors (F_Server_Binder)'Old
                     and Ctx.Cursors (F_Server_Handle) = Ctx.Cursors (F_Server_Handle)'Old
                     and Ctx.Cursors (F_Server_Parent) = Ctx.Cursors (F_Server_Parent)'Old
                     and Ctx.Cursors (F_Server_Buffer) = Ctx.Cursors (F_Server_Buffer)'Old
                     and Invalid (Ctx, F_Server_Unused_Padding)
                     and Invalid (Ctx, F_Server_Parent_Offset)
                     and Invalid (Ctx, F_Server_Length)
                     and Invalid (Ctx, F_Server_Cookie)
                     and Invalid (Ctx, F_Server_Index)
                     and Invalid (Ctx, F_Padding)
                     and Invalid (Ctx, F_Allow_Isolated)
                     and Invalid (Ctx, F_Dump_Flags),
               when F_Server_Parent_Offset =>
                  Ctx.Cursors (F_Len) = Ctx.Cursors (F_Len)'Old
                     and Ctx.Cursors (F_Name) = Ctx.Cursors (F_Name)'Old
                     and Ctx.Cursors (F_Server_Kind) = Ctx.Cursors (F_Server_Kind)'Old
                     and Ctx.Cursors (F_Server_Arity) = Ctx.Cursors (F_Server_Arity)'Old
                     and Ctx.Cursors (F_Server_Tag) = Ctx.Cursors (F_Server_Tag)'Old
                     and Ctx.Cursors (F_Server_Legacy_Flags) = Ctx.Cursors (F_Server_Legacy_Flags)'Old
                     and Ctx.Cursors (F_Server_Has_Parent) = Ctx.Cursors (F_Server_Has_Parent)'Old
                     and Ctx.Cursors (F_Server_Flags) = Ctx.Cursors (F_Server_Flags)'Old
                     and Ctx.Cursors (F_Server_FD) = Ctx.Cursors (F_Server_FD)'Old
                     and Ctx.Cursors (F_Server_Num_FDs) = Ctx.Cursors (F_Server_Num_FDs)'Old
                     and Ctx.Cursors (F_Server_Padding) = Ctx.Cursors (F_Server_Padding)'Old
                     and Ctx.Cursors (F_Server_Binder) = Ctx.Cursors (F_Server_Binder)'Old
                     and Ctx.Cursors (F_Server_Handle) = Ctx.Cursors (F_Server_Handle)'Old
                     and Ctx.Cursors (F_Server_Parent) = Ctx.Cursors (F_Server_Parent)'Old
                     and Ctx.Cursors (F_Server_Buffer) = Ctx.Cursors (F_Server_Buffer)'Old
                     and Ctx.Cursors (F_Server_Unused_Padding) = Ctx.Cursors (F_Server_Unused_Padding)'Old
                     and Invalid (Ctx, F_Server_Parent_Offset)
                     and Invalid (Ctx, F_Server_Length)
                     and Invalid (Ctx, F_Server_Cookie)
                     and Invalid (Ctx, F_Server_Index)
                     and Invalid (Ctx, F_Padding)
                     and Invalid (Ctx, F_Allow_Isolated)
                     and Invalid (Ctx, F_Dump_Flags),
               when F_Server_Length =>
                  Ctx.Cursors (F_Len) = Ctx.Cursors (F_Len)'Old
                     and Ctx.Cursors (F_Name) = Ctx.Cursors (F_Name)'Old
                     and Ctx.Cursors (F_Server_Kind) = Ctx.Cursors (F_Server_Kind)'Old
                     and Ctx.Cursors (F_Server_Arity) = Ctx.Cursors (F_Server_Arity)'Old
                     and Ctx.Cursors (F_Server_Tag) = Ctx.Cursors (F_Server_Tag)'Old
                     and Ctx.Cursors (F_Server_Legacy_Flags) = Ctx.Cursors (F_Server_Legacy_Flags)'Old
                     and Ctx.Cursors (F_Server_Has_Parent) = Ctx.Cursors (F_Server_Has_Parent)'Old
                     and Ctx.Cursors (F_Server_Flags) = Ctx.Cursors (F_Server_Flags)'Old
                     and Ctx.Cursors (F_Server_FD) = Ctx.Cursors (F_Server_FD)'Old
                     and Ctx.Cursors (F_Server_Num_FDs) = Ctx.Cursors (F_Server_Num_FDs)'Old
                     and Ctx.Cursors (F_Server_Padding) = Ctx.Cursors (F_Server_Padding)'Old
                     and Ctx.Cursors (F_Server_Binder) = Ctx.Cursors (F_Server_Binder)'Old
                     and Ctx.Cursors (F_Server_Handle) = Ctx.Cursors (F_Server_Handle)'Old
                     and Ctx.Cursors (F_Server_Parent) = Ctx.Cursors (F_Server_Parent)'Old
                     and Ctx.Cursors (F_Server_Buffer) = Ctx.Cursors (F_Server_Buffer)'Old
                     and Ctx.Cursors (F_Server_Unused_Padding) = Ctx.Cursors (F_Server_Unused_Padding)'Old
                     and Ctx.Cursors (F_Server_Parent_Offset) = Ctx.Cursors (F_Server_Parent_Offset)'Old
                     and Invalid (Ctx, F_Server_Length)
                     and Invalid (Ctx, F_Server_Cookie)
                     and Invalid (Ctx, F_Server_Index)
                     and Invalid (Ctx, F_Padding)
                     and Invalid (Ctx, F_Allow_Isolated)
                     and Invalid (Ctx, F_Dump_Flags),
               when F_Server_Cookie =>
                  Ctx.Cursors (F_Len) = Ctx.Cursors (F_Len)'Old
                     and Ctx.Cursors (F_Name) = Ctx.Cursors (F_Name)'Old
                     and Ctx.Cursors (F_Server_Kind) = Ctx.Cursors (F_Server_Kind)'Old
                     and Ctx.Cursors (F_Server_Arity) = Ctx.Cursors (F_Server_Arity)'Old
                     and Ctx.Cursors (F_Server_Tag) = Ctx.Cursors (F_Server_Tag)'Old
                     and Ctx.Cursors (F_Server_Legacy_Flags) = Ctx.Cursors (F_Server_Legacy_Flags)'Old
                     and Ctx.Cursors (F_Server_Has_Parent) = Ctx.Cursors (F_Server_Has_Parent)'Old
                     and Ctx.Cursors (F_Server_Flags) = Ctx.Cursors (F_Server_Flags)'Old
                     and Ctx.Cursors (F_Server_FD) = Ctx.Cursors (F_Server_FD)'Old
                     and Ctx.Cursors (F_Server_Num_FDs) = Ctx.Cursors (F_Server_Num_FDs)'Old
                     and Ctx.Cursors (F_Server_Padding) = Ctx.Cursors (F_Server_Padding)'Old
                     and Ctx.Cursors (F_Server_Binder) = Ctx.Cursors (F_Server_Binder)'Old
                     and Ctx.Cursors (F_Server_Handle) = Ctx.Cursors (F_Server_Handle)'Old
                     and Ctx.Cursors (F_Server_Parent) = Ctx.Cursors (F_Server_Parent)'Old
                     and Ctx.Cursors (F_Server_Buffer) = Ctx.Cursors (F_Server_Buffer)'Old
                     and Ctx.Cursors (F_Server_Unused_Padding) = Ctx.Cursors (F_Server_Unused_Padding)'Old
                     and Ctx.Cursors (F_Server_Parent_Offset) = Ctx.Cursors (F_Server_Parent_Offset)'Old
                     and Ctx.Cursors (F_Server_Length) = Ctx.Cursors (F_Server_Length)'Old
                     and Invalid (Ctx, F_Server_Cookie)
                     and Invalid (Ctx, F_Server_Index)
                     and Invalid (Ctx, F_Padding)
                     and Invalid (Ctx, F_Allow_Isolated)
                     and Invalid (Ctx, F_Dump_Flags),
               when F_Server_Index =>
                  Ctx.Cursors (F_Len) = Ctx.Cursors (F_Len)'Old
                     and Ctx.Cursors (F_Name) = Ctx.Cursors (F_Name)'Old
                     and Ctx.Cursors (F_Server_Kind) = Ctx.Cursors (F_Server_Kind)'Old
                     and Ctx.Cursors (F_Server_Arity) = Ctx.Cursors (F_Server_Arity)'Old
                     and Ctx.Cursors (F_Server_Tag) = Ctx.Cursors (F_Server_Tag)'Old
                     and Ctx.Cursors (F_Server_Legacy_Flags) = Ctx.Cursors (F_Server_Legacy_Flags)'Old
                     and Ctx.Cursors (F_Server_Has_Parent) = Ctx.Cursors (F_Server_Has_Parent)'Old
                     and Ctx.Cursors (F_Server_Flags) = Ctx.Cursors (F_Server_Flags)'Old
                     and Ctx.Cursors (F_Server_FD) = Ctx.Cursors (F_Server_FD)'Old
                     and Ctx.Cursors (F_Server_Num_FDs) = Ctx.Cursors (F_Server_Num_FDs)'Old
                     and Ctx.Cursors (F_Server_Padding) = Ctx.Cursors (F_Server_Padding)'Old
                     and Ctx.Cursors (F_Server_Binder) = Ctx.Cursors (F_Server_Binder)'Old
                     and Ctx.Cursors (F_Server_Handle) = Ctx.Cursors (F_Server_Handle)'Old
                     and Ctx.Cursors (F_Server_Parent) = Ctx.Cursors (F_Server_Parent)'Old
                     and Ctx.Cursors (F_Server_Buffer) = Ctx.Cursors (F_Server_Buffer)'Old
                     and Ctx.Cursors (F_Server_Unused_Padding) = Ctx.Cursors (F_Server_Unused_Padding)'Old
                     and Ctx.Cursors (F_Server_Parent_Offset) = Ctx.Cursors (F_Server_Parent_Offset)'Old
                     and Ctx.Cursors (F_Server_Length) = Ctx.Cursors (F_Server_Length)'Old
                     and Ctx.Cursors (F_Server_Cookie) = Ctx.Cursors (F_Server_Cookie)'Old
                     and Invalid (Ctx, F_Server_Index)
                     and Invalid (Ctx, F_Padding)
                     and Invalid (Ctx, F_Allow_Isolated)
                     and Invalid (Ctx, F_Dump_Flags),
               when F_Padding =>
                  Ctx.Cursors (F_Len) = Ctx.Cursors (F_Len)'Old
                     and Ctx.Cursors (F_Name) = Ctx.Cursors (F_Name)'Old
                     and Ctx.Cursors (F_Server_Kind) = Ctx.Cursors (F_Server_Kind)'Old
                     and Ctx.Cursors (F_Server_Arity) = Ctx.Cursors (F_Server_Arity)'Old
                     and Ctx.Cursors (F_Server_Tag) = Ctx.Cursors (F_Server_Tag)'Old
                     and Ctx.Cursors (F_Server_Legacy_Flags) = Ctx.Cursors (F_Server_Legacy_Flags)'Old
                     and Ctx.Cursors (F_Server_Has_Parent) = Ctx.Cursors (F_Server_Has_Parent)'Old
                     and Ctx.Cursors (F_Server_Flags) = Ctx.Cursors (F_Server_Flags)'Old
                     and Ctx.Cursors (F_Server_FD) = Ctx.Cursors (F_Server_FD)'Old
                     and Ctx.Cursors (F_Server_Num_FDs) = Ctx.Cursors (F_Server_Num_FDs)'Old
                     and Ctx.Cursors (F_Server_Padding) = Ctx.Cursors (F_Server_Padding)'Old
                     and Ctx.Cursors (F_Server_Binder) = Ctx.Cursors (F_Server_Binder)'Old
                     and Ctx.Cursors (F_Server_Handle) = Ctx.Cursors (F_Server_Handle)'Old
                     and Ctx.Cursors (F_Server_Parent) = Ctx.Cursors (F_Server_Parent)'Old
                     and Ctx.Cursors (F_Server_Buffer) = Ctx.Cursors (F_Server_Buffer)'Old
                     and Ctx.Cursors (F_Server_Unused_Padding) = Ctx.Cursors (F_Server_Unused_Padding)'Old
                     and Ctx.Cursors (F_Server_Parent_Offset) = Ctx.Cursors (F_Server_Parent_Offset)'Old
                     and Ctx.Cursors (F_Server_Length) = Ctx.Cursors (F_Server_Length)'Old
                     and Ctx.Cursors (F_Server_Cookie) = Ctx.Cursors (F_Server_Cookie)'Old
                     and Ctx.Cursors (F_Server_Index) = Ctx.Cursors (F_Server_Index)'Old
                     and Invalid (Ctx, F_Padding)
                     and Invalid (Ctx, F_Allow_Isolated)
                     and Invalid (Ctx, F_Dump_Flags),
               when F_Allow_Isolated =>
                  Ctx.Cursors (F_Len) = Ctx.Cursors (F_Len)'Old
                     and Ctx.Cursors (F_Name) = Ctx.Cursors (F_Name)'Old
                     and Ctx.Cursors (F_Server_Kind) = Ctx.Cursors (F_Server_Kind)'Old
                     and Ctx.Cursors (F_Server_Arity) = Ctx.Cursors (F_Server_Arity)'Old
                     and Ctx.Cursors (F_Server_Tag) = Ctx.Cursors (F_Server_Tag)'Old
                     and Ctx.Cursors (F_Server_Legacy_Flags) = Ctx.Cursors (F_Server_Legacy_Flags)'Old
                     and Ctx.Cursors (F_Server_Has_Parent) = Ctx.Cursors (F_Server_Has_Parent)'Old
                     and Ctx.Cursors (F_Server_Flags) = Ctx.Cursors (F_Server_Flags)'Old
                     and Ctx.Cursors (F_Server_FD) = Ctx.Cursors (F_Server_FD)'Old
                     and Ctx.Cursors (F_Server_Num_FDs) = Ctx.Cursors (F_Server_Num_FDs)'Old
                     and Ctx.Cursors (F_Server_Padding) = Ctx.Cursors (F_Server_Padding)'Old
                     and Ctx.Cursors (F_Server_Binder) = Ctx.Cursors (F_Server_Binder)'Old
                     and Ctx.Cursors (F_Server_Handle) = Ctx.Cursors (F_Server_Handle)'Old
                     and Ctx.Cursors (F_Server_Parent) = Ctx.Cursors (F_Server_Parent)'Old
                     and Ctx.Cursors (F_Server_Buffer) = Ctx.Cursors (F_Server_Buffer)'Old
                     and Ctx.Cursors (F_Server_Unused_Padding) = Ctx.Cursors (F_Server_Unused_Padding)'Old
                     and Ctx.Cursors (F_Server_Parent_Offset) = Ctx.Cursors (F_Server_Parent_Offset)'Old
                     and Ctx.Cursors (F_Server_Length) = Ctx.Cursors (F_Server_Length)'Old
                     and Ctx.Cursors (F_Server_Cookie) = Ctx.Cursors (F_Server_Cookie)'Old
                     and Ctx.Cursors (F_Server_Index) = Ctx.Cursors (F_Server_Index)'Old
                     and Ctx.Cursors (F_Padding) = Ctx.Cursors (F_Padding)'Old
                     and Invalid (Ctx, F_Allow_Isolated)
                     and Invalid (Ctx, F_Dump_Flags),
               when F_Dump_Flags =>
                  Ctx.Cursors (F_Len) = Ctx.Cursors (F_Len)'Old
                     and Ctx.Cursors (F_Name) = Ctx.Cursors (F_Name)'Old
                     and Ctx.Cursors (F_Server_Kind) = Ctx.Cursors (F_Server_Kind)'Old
                     and Ctx.Cursors (F_Server_Arity) = Ctx.Cursors (F_Server_Arity)'Old
                     and Ctx.Cursors (F_Server_Tag) = Ctx.Cursors (F_Server_Tag)'Old
                     and Ctx.Cursors (F_Server_Legacy_Flags) = Ctx.Cursors (F_Server_Legacy_Flags)'Old
                     and Ctx.Cursors (F_Server_Has_Parent) = Ctx.Cursors (F_Server_Has_Parent)'Old
                     and Ctx.Cursors (F_Server_Flags) = Ctx.Cursors (F_Server_Flags)'Old
                     and Ctx.Cursors (F_Server_FD) = Ctx.Cursors (F_Server_FD)'Old
                     and Ctx.Cursors (F_Server_Num_FDs) = Ctx.Cursors (F_Server_Num_FDs)'Old
                     and Ctx.Cursors (F_Server_Padding) = Ctx.Cursors (F_Server_Padding)'Old
                     and Ctx.Cursors (F_Server_Binder) = Ctx.Cursors (F_Server_Binder)'Old
                     and Ctx.Cursors (F_Server_Handle) = Ctx.Cursors (F_Server_Handle)'Old
                     and Ctx.Cursors (F_Server_Parent) = Ctx.Cursors (F_Server_Parent)'Old
                     and Ctx.Cursors (F_Server_Buffer) = Ctx.Cursors (F_Server_Buffer)'Old
                     and Ctx.Cursors (F_Server_Unused_Padding) = Ctx.Cursors (F_Server_Unused_Padding)'Old
                     and Ctx.Cursors (F_Server_Parent_Offset) = Ctx.Cursors (F_Server_Parent_Offset)'Old
                     and Ctx.Cursors (F_Server_Length) = Ctx.Cursors (F_Server_Length)'Old
                     and Ctx.Cursors (F_Server_Cookie) = Ctx.Cursors (F_Server_Cookie)'Old
                     and Ctx.Cursors (F_Server_Index) = Ctx.Cursors (F_Server_Index)'Old
                     and Ctx.Cursors (F_Padding) = Ctx.Cursors (F_Padding)'Old
                     and Ctx.Cursors (F_Allow_Isolated) = Ctx.Cursors (F_Allow_Isolated)'Old
                     and Invalid (Ctx, F_Dump_Flags))
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
            Ctx.Cursors (F_Dump_Flags) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Allow_Isolated) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Padding) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Server_Index) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Server_Cookie) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Server_Length) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Server_Parent_Offset) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Server_Unused_Padding) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Server_Buffer) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Server_Parent) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Server_Handle) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Server_Binder) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Server_Padding) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Server_Num_FDs) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Server_FD) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Server_Flags) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Server_Has_Parent) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Server_Legacy_Flags) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Server_Tag) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Server_Arity) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Server_Kind) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Name) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Len) := (S_Invalid, Ctx.Cursors (F_Len).Predecessor);
            pragma Assert (Field_First (Ctx, Fld) = First
               and Field_Length (Ctx, Fld) = Length);
         when F_Name =>
            Ctx.Cursors (F_Dump_Flags) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Allow_Isolated) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Padding) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Server_Index) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Server_Cookie) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Server_Length) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Server_Parent_Offset) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Server_Unused_Padding) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Server_Buffer) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Server_Parent) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Server_Handle) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Server_Binder) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Server_Padding) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Server_Num_FDs) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Server_FD) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Server_Flags) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Server_Has_Parent) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Server_Legacy_Flags) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Server_Tag) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Server_Arity) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Server_Kind) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Name) := (S_Invalid, Ctx.Cursors (F_Name).Predecessor);
            pragma Assert (Field_First (Ctx, Fld) = First
               and Field_Length (Ctx, Fld) = Length);
         when F_Server_Kind =>
            Ctx.Cursors (F_Dump_Flags) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Allow_Isolated) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Padding) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Server_Index) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Server_Cookie) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Server_Length) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Server_Parent_Offset) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Server_Unused_Padding) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Server_Buffer) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Server_Parent) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Server_Handle) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Server_Binder) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Server_Padding) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Server_Num_FDs) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Server_FD) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Server_Flags) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Server_Has_Parent) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Server_Legacy_Flags) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Server_Tag) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Server_Arity) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Server_Kind) := (S_Invalid, Ctx.Cursors (F_Server_Kind).Predecessor);
            pragma Assert (Field_First (Ctx, Fld) = First
               and Field_Length (Ctx, Fld) = Length);
         when F_Server_Arity =>
            Ctx.Cursors (F_Dump_Flags) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Allow_Isolated) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Padding) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Server_Index) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Server_Cookie) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Server_Length) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Server_Parent_Offset) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Server_Unused_Padding) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Server_Buffer) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Server_Parent) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Server_Handle) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Server_Binder) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Server_Padding) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Server_Num_FDs) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Server_FD) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Server_Flags) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Server_Has_Parent) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Server_Legacy_Flags) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Server_Tag) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Server_Arity) := (S_Invalid, Ctx.Cursors (F_Server_Arity).Predecessor);
            pragma Assert (Field_First (Ctx, Fld) = First
               and Field_Length (Ctx, Fld) = Length);
         when F_Server_Tag =>
            Ctx.Cursors (F_Dump_Flags) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Allow_Isolated) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Padding) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Server_Index) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Server_Cookie) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Server_Length) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Server_Parent_Offset) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Server_Unused_Padding) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Server_Buffer) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Server_Parent) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Server_Handle) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Server_Binder) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Server_Padding) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Server_Num_FDs) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Server_FD) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Server_Flags) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Server_Has_Parent) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Server_Legacy_Flags) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Server_Tag) := (S_Invalid, Ctx.Cursors (F_Server_Tag).Predecessor);
            pragma Assert (Field_First (Ctx, Fld) = First
               and Field_Length (Ctx, Fld) = Length);
         when F_Server_Legacy_Flags =>
            Ctx.Cursors (F_Dump_Flags) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Allow_Isolated) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Padding) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Server_Index) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Server_Cookie) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Server_Length) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Server_Parent_Offset) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Server_Unused_Padding) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Server_Buffer) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Server_Parent) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Server_Handle) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Server_Binder) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Server_Padding) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Server_Num_FDs) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Server_FD) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Server_Flags) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Server_Has_Parent) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Server_Legacy_Flags) := (S_Invalid, Ctx.Cursors (F_Server_Legacy_Flags).Predecessor);
            pragma Assert (Field_First (Ctx, Fld) = First
               and Field_Length (Ctx, Fld) = Length);
         when F_Server_Has_Parent =>
            Ctx.Cursors (F_Dump_Flags) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Allow_Isolated) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Padding) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Server_Index) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Server_Cookie) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Server_Length) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Server_Parent_Offset) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Server_Unused_Padding) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Server_Buffer) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Server_Parent) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Server_Handle) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Server_Binder) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Server_Padding) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Server_Num_FDs) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Server_FD) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Server_Flags) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Server_Has_Parent) := (S_Invalid, Ctx.Cursors (F_Server_Has_Parent).Predecessor);
            pragma Assert (Field_First (Ctx, Fld) = First
               and Field_Length (Ctx, Fld) = Length);
         when F_Server_Flags =>
            Ctx.Cursors (F_Dump_Flags) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Allow_Isolated) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Padding) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Server_Index) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Server_Cookie) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Server_Length) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Server_Parent_Offset) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Server_Unused_Padding) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Server_Buffer) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Server_Parent) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Server_Handle) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Server_Binder) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Server_Padding) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Server_Num_FDs) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Server_FD) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Server_Flags) := (S_Invalid, Ctx.Cursors (F_Server_Flags).Predecessor);
            pragma Assert (Field_First (Ctx, Fld) = First
               and Field_Length (Ctx, Fld) = Length);
         when F_Server_FD =>
            Ctx.Cursors (F_Dump_Flags) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Allow_Isolated) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Padding) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Server_Index) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Server_Cookie) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Server_Length) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Server_Parent_Offset) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Server_Unused_Padding) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Server_Buffer) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Server_Parent) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Server_Handle) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Server_Binder) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Server_Padding) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Server_Num_FDs) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Server_FD) := (S_Invalid, Ctx.Cursors (F_Server_FD).Predecessor);
            pragma Assert (Field_First (Ctx, Fld) = First
               and Field_Length (Ctx, Fld) = Length);
         when F_Server_Num_FDs =>
            Ctx.Cursors (F_Dump_Flags) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Allow_Isolated) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Padding) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Server_Index) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Server_Cookie) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Server_Length) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Server_Parent_Offset) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Server_Unused_Padding) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Server_Buffer) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Server_Parent) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Server_Handle) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Server_Binder) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Server_Padding) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Server_Num_FDs) := (S_Invalid, Ctx.Cursors (F_Server_Num_FDs).Predecessor);
            pragma Assert (Field_First (Ctx, Fld) = First
               and Field_Length (Ctx, Fld) = Length);
         when F_Server_Padding =>
            Ctx.Cursors (F_Dump_Flags) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Allow_Isolated) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Padding) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Server_Index) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Server_Cookie) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Server_Length) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Server_Parent_Offset) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Server_Unused_Padding) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Server_Buffer) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Server_Parent) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Server_Handle) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Server_Binder) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Server_Padding) := (S_Invalid, Ctx.Cursors (F_Server_Padding).Predecessor);
            pragma Assert (Field_First (Ctx, Fld) = First
               and Field_Length (Ctx, Fld) = Length);
         when F_Server_Binder =>
            Ctx.Cursors (F_Dump_Flags) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Allow_Isolated) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Padding) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Server_Index) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Server_Cookie) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Server_Length) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Server_Parent_Offset) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Server_Unused_Padding) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Server_Buffer) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Server_Parent) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Server_Handle) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Server_Binder) := (S_Invalid, Ctx.Cursors (F_Server_Binder).Predecessor);
            pragma Assert (Field_First (Ctx, Fld) = First
               and Field_Length (Ctx, Fld) = Length);
         when F_Server_Handle =>
            Ctx.Cursors (F_Dump_Flags) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Allow_Isolated) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Padding) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Server_Index) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Server_Cookie) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Server_Length) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Server_Parent_Offset) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Server_Unused_Padding) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Server_Buffer) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Server_Parent) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Server_Handle) := (S_Invalid, Ctx.Cursors (F_Server_Handle).Predecessor);
            pragma Assert (Field_First (Ctx, Fld) = First
               and Field_Length (Ctx, Fld) = Length);
         when F_Server_Parent =>
            Ctx.Cursors (F_Dump_Flags) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Allow_Isolated) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Padding) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Server_Index) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Server_Cookie) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Server_Length) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Server_Parent_Offset) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Server_Unused_Padding) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Server_Buffer) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Server_Parent) := (S_Invalid, Ctx.Cursors (F_Server_Parent).Predecessor);
            pragma Assert (Field_First (Ctx, Fld) = First
               and Field_Length (Ctx, Fld) = Length);
         when F_Server_Buffer =>
            Ctx.Cursors (F_Dump_Flags) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Allow_Isolated) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Padding) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Server_Index) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Server_Cookie) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Server_Length) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Server_Parent_Offset) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Server_Unused_Padding) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Server_Buffer) := (S_Invalid, Ctx.Cursors (F_Server_Buffer).Predecessor);
            pragma Assert (Field_First (Ctx, Fld) = First
               and Field_Length (Ctx, Fld) = Length);
         when F_Server_Unused_Padding =>
            Ctx.Cursors (F_Dump_Flags) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Allow_Isolated) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Padding) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Server_Index) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Server_Cookie) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Server_Length) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Server_Parent_Offset) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Server_Unused_Padding) := (S_Invalid, Ctx.Cursors (F_Server_Unused_Padding).Predecessor);
            pragma Assert (Field_First (Ctx, Fld) = First
               and Field_Length (Ctx, Fld) = Length);
         when F_Server_Parent_Offset =>
            Ctx.Cursors (F_Dump_Flags) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Allow_Isolated) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Padding) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Server_Index) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Server_Cookie) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Server_Length) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Server_Parent_Offset) := (S_Invalid, Ctx.Cursors (F_Server_Parent_Offset).Predecessor);
            pragma Assert (Field_First (Ctx, Fld) = First
               and Field_Length (Ctx, Fld) = Length);
         when F_Server_Length =>
            Ctx.Cursors (F_Dump_Flags) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Allow_Isolated) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Padding) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Server_Index) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Server_Cookie) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Server_Length) := (S_Invalid, Ctx.Cursors (F_Server_Length).Predecessor);
            pragma Assert (Field_First (Ctx, Fld) = First
               and Field_Length (Ctx, Fld) = Length);
         when F_Server_Cookie =>
            Ctx.Cursors (F_Dump_Flags) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Allow_Isolated) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Padding) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Server_Index) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Server_Cookie) := (S_Invalid, Ctx.Cursors (F_Server_Cookie).Predecessor);
            pragma Assert (Field_First (Ctx, Fld) = First
               and Field_Length (Ctx, Fld) = Length);
         when F_Server_Index =>
            Ctx.Cursors (F_Dump_Flags) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Allow_Isolated) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Padding) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Server_Index) := (S_Invalid, Ctx.Cursors (F_Server_Index).Predecessor);
            pragma Assert (Field_First (Ctx, Fld) = First
               and Field_Length (Ctx, Fld) = Length);
         when F_Padding =>
            Ctx.Cursors (F_Dump_Flags) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Allow_Isolated) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Padding) := (S_Invalid, Ctx.Cursors (F_Padding).Predecessor);
            pragma Assert (Field_First (Ctx, Fld) = First
               and Field_Length (Ctx, Fld) = Length);
         when F_Allow_Isolated =>
            Ctx.Cursors (F_Dump_Flags) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Allow_Isolated) := (S_Invalid, Ctx.Cursors (F_Allow_Isolated).Predecessor);
            pragma Assert (Field_First (Ctx, Fld) = First
               and Field_Length (Ctx, Fld) = Length);
         when F_Dump_Flags =>
            Ctx.Cursors (F_Dump_Flags) := (S_Invalid, Ctx.Cursors (F_Dump_Flags).Predecessor);
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
            True,
         when F_Server_Kind | F_Server_Arity | F_Server_Tag | F_Server_Legacy_Flags | F_Server_Has_Parent | F_Server_Flags | F_Server_FD | F_Server_Num_FDs | F_Server_Padding | F_Server_Binder | F_Server_Handle | F_Server_Parent | F_Server_Buffer | F_Server_Unused_Padding | F_Server_Parent_Offset | F_Server_Length | F_Server_Cookie | F_Server_Index | F_Padding | F_Allow_Isolated | F_Dump_Flags =>
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
      function Extract is new Types.Extract (Service_Manager.Len_Base);
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
      function Extract is new Types.Extract (Service_Manager.MBZ_7_Base);
      function Extract is new Types.Extract (Service_Manager.Integer_Base);
   begin
      return ((case Fld is
            when F_Len =>
               (Fld => F_Len, Len_Value => Extract (Ctx.Buffer.all (Buffer_First .. Buffer_Last), Offset)),
            when F_Name =>
               (Fld => F_Name),
            when F_Server_Kind =>
               (Fld => F_Server_Kind, Server_Kind_Value => Extract (Ctx.Buffer.all (Buffer_First .. Buffer_Last), Offset)),
            when F_Server_Arity =>
               (Fld => F_Server_Arity, Server_Arity_Value => Extract (Ctx.Buffer.all (Buffer_First .. Buffer_Last), Offset)),
            when F_Server_Tag =>
               (Fld => F_Server_Tag, Server_Tag_Value => Extract (Ctx.Buffer.all (Buffer_First .. Buffer_Last), Offset)),
            when F_Server_Legacy_Flags =>
               (Fld => F_Server_Legacy_Flags, Server_Legacy_Flags_Value => Extract (Ctx.Buffer.all (Buffer_First .. Buffer_Last), Offset)),
            when F_Server_Has_Parent =>
               (Fld => F_Server_Has_Parent, Server_Has_Parent_Value => Extract (Ctx.Buffer.all (Buffer_First .. Buffer_Last), Offset)),
            when F_Server_Flags =>
               (Fld => F_Server_Flags, Server_Flags_Value => Extract (Ctx.Buffer.all (Buffer_First .. Buffer_Last), Offset)),
            when F_Server_FD =>
               (Fld => F_Server_FD, Server_FD_Value => Extract (Ctx.Buffer.all (Buffer_First .. Buffer_Last), Offset)),
            when F_Server_Num_FDs =>
               (Fld => F_Server_Num_FDs, Server_Num_FDs_Value => Extract (Ctx.Buffer.all (Buffer_First .. Buffer_Last), Offset)),
            when F_Server_Padding =>
               (Fld => F_Server_Padding, Server_Padding_Value => Extract (Ctx.Buffer.all (Buffer_First .. Buffer_Last), Offset)),
            when F_Server_Binder =>
               (Fld => F_Server_Binder, Server_Binder_Value => Extract (Ctx.Buffer.all (Buffer_First .. Buffer_Last), Offset)),
            when F_Server_Handle =>
               (Fld => F_Server_Handle, Server_Handle_Value => Extract (Ctx.Buffer.all (Buffer_First .. Buffer_Last), Offset)),
            when F_Server_Parent =>
               (Fld => F_Server_Parent, Server_Parent_Value => Extract (Ctx.Buffer.all (Buffer_First .. Buffer_Last), Offset)),
            when F_Server_Buffer =>
               (Fld => F_Server_Buffer, Server_Buffer_Value => Extract (Ctx.Buffer.all (Buffer_First .. Buffer_Last), Offset)),
            when F_Server_Unused_Padding =>
               (Fld => F_Server_Unused_Padding, Server_Unused_Padding_Value => Extract (Ctx.Buffer.all (Buffer_First .. Buffer_Last), Offset)),
            when F_Server_Parent_Offset =>
               (Fld => F_Server_Parent_Offset, Server_Parent_Offset_Value => Extract (Ctx.Buffer.all (Buffer_First .. Buffer_Last), Offset)),
            when F_Server_Length =>
               (Fld => F_Server_Length, Server_Length_Value => Extract (Ctx.Buffer.all (Buffer_First .. Buffer_Last), Offset)),
            when F_Server_Cookie =>
               (Fld => F_Server_Cookie, Server_Cookie_Value => Extract (Ctx.Buffer.all (Buffer_First .. Buffer_Last), Offset)),
            when F_Server_Index =>
               (Fld => F_Server_Index, Server_Index_Value => Extract (Ctx.Buffer.all (Buffer_First .. Buffer_Last), Offset)),
            when F_Padding =>
               (Fld => F_Padding, Padding_Value => Extract (Ctx.Buffer.all (Buffer_First .. Buffer_Last), Offset)),
            when F_Allow_Isolated =>
               (Fld => F_Allow_Isolated, Allow_Isolated_Value => Extract (Ctx.Buffer.all (Buffer_First .. Buffer_Last), Offset)),
            when F_Dump_Flags =>
               (Fld => F_Dump_Flags, Dump_Flags_Value => Extract (Ctx.Buffer.all (Buffer_First .. Buffer_Last), Offset))));
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
                   (Ctx.Cursors (F_Len).Last - Ctx.Cursors (F_Len).First + 1) = Service_Manager.Len_Base'Size
                     and then Ctx.Cursors (F_Len).Predecessor = F_Initial
                     and then Ctx.Cursors (F_Len).First = Ctx.First
                     and then (if Structural_Valid (Ctx.Cursors (F_Name)) then
                        (Ctx.Cursors (F_Name).Last - Ctx.Cursors (F_Name).First + 1) = Types.Bit_Length (Ctx.Cursors (F_Len).Value.Len_Value)
                          and then Ctx.Cursors (F_Name).Predecessor = F_Len
                          and then Ctx.Cursors (F_Name).First = (Ctx.Cursors (F_Len).Last + 1)
                          and then (if Structural_Valid (Ctx.Cursors (F_Server_Kind)) then
                             (Ctx.Cursors (F_Server_Kind).Last - Ctx.Cursors (F_Server_Kind).First + 1) = Binder.Binder_Kind_Base'Size
                               and then Ctx.Cursors (F_Server_Kind).Predecessor = F_Name
                               and then Ctx.Cursors (F_Server_Kind).First = (Ctx.Cursors (F_Name).Last + 1)
                               and then (if Structural_Valid (Ctx.Cursors (F_Server_Arity)) then
                                  (Ctx.Cursors (F_Server_Arity).Last - Ctx.Cursors (F_Server_Arity).First + 1) = Binder.Binder_Arity_Base'Size
                                    and then Ctx.Cursors (F_Server_Arity).Predecessor = F_Server_Kind
                                    and then Ctx.Cursors (F_Server_Arity).First = (Ctx.Cursors (F_Server_Kind).Last + 1)
                                    and then (if Structural_Valid (Ctx.Cursors (F_Server_Tag))
                                         and then (Types.Bit_Length (Ctx.Cursors (F_Server_Arity).Value.Server_Arity_Value) = Types.Bit_Length (Convert (BA_SINGLE))
                                           or (Types.Bit_Length (Ctx.Cursors (F_Server_Kind).Value.Server_Kind_Value) = Types.Bit_Length (Convert (BK_FD))
                                             and Types.Bit_Length (Ctx.Cursors (F_Server_Arity).Value.Server_Arity_Value) = Types.Bit_Length (Convert (BA_ARRAY)))) then
                                       (Ctx.Cursors (F_Server_Tag).Last - Ctx.Cursors (F_Server_Tag).First + 1) = Binder.Binder_Tag_Base'Size
                                         and then Ctx.Cursors (F_Server_Tag).Predecessor = F_Server_Arity
                                         and then Ctx.Cursors (F_Server_Tag).First = (Ctx.Cursors (F_Server_Arity).Last + 1)
                                         and then (if Structural_Valid (Ctx.Cursors (F_Server_Legacy_Flags))
                                              and then Types.Bit_Length (Ctx.Cursors (F_Server_Kind).Value.Server_Kind_Value) = Types.Bit_Length (Convert (BK_FD)) then
                                            (Ctx.Cursors (F_Server_Legacy_Flags).Last - Ctx.Cursors (F_Server_Legacy_Flags).First + 1) = Binder.MBZ32_Base'Size
                                              and then Ctx.Cursors (F_Server_Legacy_Flags).Predecessor = F_Server_Tag
                                              and then Ctx.Cursors (F_Server_Legacy_Flags).First = (Ctx.Cursors (F_Server_Tag).Last + 1)
                                              and then (if Structural_Valid (Ctx.Cursors (F_Server_FD))
                                                   and then Types.Bit_Length (Ctx.Cursors (F_Server_Arity).Value.Server_Arity_Value) = Types.Bit_Length (Convert (BA_SINGLE)) then
                                                 (Ctx.Cursors (F_Server_FD).Last - Ctx.Cursors (F_Server_FD).First + 1) = Binder.Handle_Base'Size
                                                   and then Ctx.Cursors (F_Server_FD).Predecessor = F_Server_Legacy_Flags
                                                   and then Ctx.Cursors (F_Server_FD).First = (Ctx.Cursors (F_Server_Legacy_Flags).Last + 1)
                                                   and then (if Structural_Valid (Ctx.Cursors (F_Server_Cookie)) then
                                                      (Ctx.Cursors (F_Server_Cookie).Last - Ctx.Cursors (F_Server_Cookie).First + 1) = Binder.Cookie'Size
                                                        and then Ctx.Cursors (F_Server_Cookie).Predecessor = F_Server_FD
                                                        and then Ctx.Cursors (F_Server_Cookie).First = (Ctx.Cursors (F_Server_FD).Last + 1)
                                                        and then (if Structural_Valid (Ctx.Cursors (F_Padding)) then
                                                           (Ctx.Cursors (F_Padding).Last - Ctx.Cursors (F_Padding).First + 1) = Service_Manager.MBZ_7_Base'Size
                                                             and then Ctx.Cursors (F_Padding).Predecessor = F_Server_Cookie
                                                             and then Ctx.Cursors (F_Padding).First = (Ctx.Cursors (F_Server_Cookie).Last + 1)
                                                             and then (if Structural_Valid (Ctx.Cursors (F_Allow_Isolated)) then
                                                                (Ctx.Cursors (F_Allow_Isolated).Last - Ctx.Cursors (F_Allow_Isolated).First + 1) = Builtin_Types.Boolean_Base'Size
                                                                  and then Ctx.Cursors (F_Allow_Isolated).Predecessor = F_Padding
                                                                  and then Ctx.Cursors (F_Allow_Isolated).First = (Ctx.Cursors (F_Padding).Last + 1)
                                                                  and then (if Structural_Valid (Ctx.Cursors (F_Dump_Flags)) then
                                                                     (Ctx.Cursors (F_Dump_Flags).Last - Ctx.Cursors (F_Dump_Flags).First + 1) = Service_Manager.Integer_Base'Size
                                                                       and then Ctx.Cursors (F_Dump_Flags).Predecessor = F_Allow_Isolated
                                                                       and then Ctx.Cursors (F_Dump_Flags).First = (Ctx.Cursors (F_Allow_Isolated).Last + 1))))))
                                              and then (if Structural_Valid (Ctx.Cursors (F_Server_Num_FDs))
                                                   and then Types.Bit_Length (Ctx.Cursors (F_Server_Arity).Value.Server_Arity_Value) = Types.Bit_Length (Convert (BA_ARRAY)) then
                                                 (Ctx.Cursors (F_Server_Num_FDs).Last - Ctx.Cursors (F_Server_Num_FDs).First + 1) = Binder.Count'Size
                                                   and then Ctx.Cursors (F_Server_Num_FDs).Predecessor = F_Server_Legacy_Flags
                                                   and then Ctx.Cursors (F_Server_Num_FDs).First = (Ctx.Cursors (F_Server_Legacy_Flags).Last + 1)
                                                   and then (if Structural_Valid (Ctx.Cursors (F_Server_Parent)) then
                                                      (Ctx.Cursors (F_Server_Parent).Last - Ctx.Cursors (F_Server_Parent).First + 1) = Binder.Index'Size
                                                        and then Ctx.Cursors (F_Server_Parent).Predecessor = F_Server_Num_FDs
                                                        and then Ctx.Cursors (F_Server_Parent).First = (Ctx.Cursors (F_Server_Num_FDs).Last + 1)
                                                        and then (if Structural_Valid (Ctx.Cursors (F_Server_Parent_Offset)) then
                                                           (Ctx.Cursors (F_Server_Parent_Offset).Last - Ctx.Cursors (F_Server_Parent_Offset).First + 1) = Binder.Offset'Size
                                                             and then Ctx.Cursors (F_Server_Parent_Offset).Predecessor = F_Server_Parent
                                                             and then Ctx.Cursors (F_Server_Parent_Offset).First = (Ctx.Cursors (F_Server_Parent).Last + 1)
                                                             and then (if Structural_Valid (Ctx.Cursors (F_Padding)) then
                                                                (Ctx.Cursors (F_Padding).Last - Ctx.Cursors (F_Padding).First + 1) = Service_Manager.MBZ_7_Base'Size
                                                                  and then Ctx.Cursors (F_Padding).Predecessor = F_Server_Parent_Offset
                                                                  and then Ctx.Cursors (F_Padding).First = (Ctx.Cursors (F_Server_Parent_Offset).Last + 1)
                                                                  and then (if Structural_Valid (Ctx.Cursors (F_Allow_Isolated)) then
                                                                     (Ctx.Cursors (F_Allow_Isolated).Last - Ctx.Cursors (F_Allow_Isolated).First + 1) = Builtin_Types.Boolean_Base'Size
                                                                       and then Ctx.Cursors (F_Allow_Isolated).Predecessor = F_Padding
                                                                       and then Ctx.Cursors (F_Allow_Isolated).First = (Ctx.Cursors (F_Padding).Last + 1)
                                                                       and then (if Structural_Valid (Ctx.Cursors (F_Dump_Flags)) then
                                                                          (Ctx.Cursors (F_Dump_Flags).Last - Ctx.Cursors (F_Dump_Flags).First + 1) = Service_Manager.Integer_Base'Size
                                                                            and then Ctx.Cursors (F_Dump_Flags).Predecessor = F_Allow_Isolated
                                                                            and then Ctx.Cursors (F_Dump_Flags).First = (Ctx.Cursors (F_Allow_Isolated).Last + 1))))))))
                                         and then (if Structural_Valid (Ctx.Cursors (F_Server_Has_Parent))
                                              and then Types.Bit_Length (Ctx.Cursors (F_Server_Kind).Value.Server_Kind_Value) = Types.Bit_Length (Convert (BK_POINTER)) then
                                            (Ctx.Cursors (F_Server_Has_Parent).Last - Ctx.Cursors (F_Server_Has_Parent).First + 1) = Builtin_Types.Boolean_Base'Size
                                              and then Ctx.Cursors (F_Server_Has_Parent).Predecessor = F_Server_Tag
                                              and then Ctx.Cursors (F_Server_Has_Parent).First = (Ctx.Cursors (F_Server_Tag).Last + 1)
                                              and then (if Structural_Valid (Ctx.Cursors (F_Server_Padding)) then
                                                 (Ctx.Cursors (F_Server_Padding).Last - Ctx.Cursors (F_Server_Padding).First + 1) = Binder.MBZ31_Base'Size
                                                   and then Ctx.Cursors (F_Server_Padding).Predecessor = F_Server_Has_Parent
                                                   and then Ctx.Cursors (F_Server_Padding).First = (Ctx.Cursors (F_Server_Has_Parent).Last + 1)
                                                   and then (if Structural_Valid (Ctx.Cursors (F_Server_Buffer)) then
                                                      (Ctx.Cursors (F_Server_Buffer).Last - Ctx.Cursors (F_Server_Buffer).First + 1) = Binder.Index'Size
                                                        and then Ctx.Cursors (F_Server_Buffer).Predecessor = F_Server_Padding
                                                        and then Ctx.Cursors (F_Server_Buffer).First = (Ctx.Cursors (F_Server_Padding).Last + 1)
                                                        and then (if Structural_Valid (Ctx.Cursors (F_Server_Length)) then
                                                           (Ctx.Cursors (F_Server_Length).Last - Ctx.Cursors (F_Server_Length).First + 1) = Binder.Length_Base'Size
                                                             and then Ctx.Cursors (F_Server_Length).Predecessor = F_Server_Buffer
                                                             and then Ctx.Cursors (F_Server_Length).First = (Ctx.Cursors (F_Server_Buffer).Last + 1)
                                                             and then (if Structural_Valid (Ctx.Cursors (F_Padding))
                                                                  and then Types.Bit_Length (Ctx.Cursors (F_Server_Has_Parent).Value.Server_Has_Parent_Value) = Types.Bit_Length (Convert (False)) then
                                                                (Ctx.Cursors (F_Padding).Last - Ctx.Cursors (F_Padding).First + 1) = Service_Manager.MBZ_7_Base'Size
                                                                  and then Ctx.Cursors (F_Padding).Predecessor = F_Server_Length
                                                                  and then Ctx.Cursors (F_Padding).First = (Ctx.Cursors (F_Server_Length).Last + 1)
                                                                  and then (if Structural_Valid (Ctx.Cursors (F_Allow_Isolated)) then
                                                                     (Ctx.Cursors (F_Allow_Isolated).Last - Ctx.Cursors (F_Allow_Isolated).First + 1) = Builtin_Types.Boolean_Base'Size
                                                                       and then Ctx.Cursors (F_Allow_Isolated).Predecessor = F_Padding
                                                                       and then Ctx.Cursors (F_Allow_Isolated).First = (Ctx.Cursors (F_Padding).Last + 1)
                                                                       and then (if Structural_Valid (Ctx.Cursors (F_Dump_Flags)) then
                                                                          (Ctx.Cursors (F_Dump_Flags).Last - Ctx.Cursors (F_Dump_Flags).First + 1) = Service_Manager.Integer_Base'Size
                                                                            and then Ctx.Cursors (F_Dump_Flags).Predecessor = F_Allow_Isolated
                                                                            and then Ctx.Cursors (F_Dump_Flags).First = (Ctx.Cursors (F_Allow_Isolated).Last + 1))))
                                                             and then (if Structural_Valid (Ctx.Cursors (F_Server_Index))
                                                                  and then Types.Bit_Length (Ctx.Cursors (F_Server_Has_Parent).Value.Server_Has_Parent_Value) = Types.Bit_Length (Convert (True)) then
                                                                (Ctx.Cursors (F_Server_Index).Last - Ctx.Cursors (F_Server_Index).First + 1) = Binder.Index'Size
                                                                  and then Ctx.Cursors (F_Server_Index).Predecessor = F_Server_Length
                                                                  and then Ctx.Cursors (F_Server_Index).First = (Ctx.Cursors (F_Server_Length).Last + 1)
                                                                  and then (if Structural_Valid (Ctx.Cursors (F_Padding)) then
                                                                     (Ctx.Cursors (F_Padding).Last - Ctx.Cursors (F_Padding).First + 1) = Service_Manager.MBZ_7_Base'Size
                                                                       and then Ctx.Cursors (F_Padding).Predecessor = F_Server_Index
                                                                       and then Ctx.Cursors (F_Padding).First = (Ctx.Cursors (F_Server_Index).Last + 1)
                                                                       and then (if Structural_Valid (Ctx.Cursors (F_Allow_Isolated)) then
                                                                          (Ctx.Cursors (F_Allow_Isolated).Last - Ctx.Cursors (F_Allow_Isolated).First + 1) = Builtin_Types.Boolean_Base'Size
                                                                            and then Ctx.Cursors (F_Allow_Isolated).Predecessor = F_Padding
                                                                            and then Ctx.Cursors (F_Allow_Isolated).First = (Ctx.Cursors (F_Padding).Last + 1)
                                                                            and then (if Structural_Valid (Ctx.Cursors (F_Dump_Flags)) then
                                                                               (Ctx.Cursors (F_Dump_Flags).Last - Ctx.Cursors (F_Dump_Flags).First + 1) = Service_Manager.Integer_Base'Size
                                                                                 and then Ctx.Cursors (F_Dump_Flags).Predecessor = F_Allow_Isolated
                                                                                 and then Ctx.Cursors (F_Dump_Flags).First = (Ctx.Cursors (F_Allow_Isolated).Last + 1)))))))))
                                         and then (if Structural_Valid (Ctx.Cursors (F_Server_Flags))
                                              and then (Types.Bit_Length (Ctx.Cursors (F_Server_Kind).Value.Server_Kind_Value) /= Types.Bit_Length (Convert (BK_POINTER))
                                                and Types.Bit_Length (Ctx.Cursors (F_Server_Kind).Value.Server_Kind_Value) /= Types.Bit_Length (Convert (BK_FD))) then
                                            (Ctx.Cursors (F_Server_Flags).Last - Ctx.Cursors (F_Server_Flags).First + 1) = Binder.Flat_Binder_Flags_Base'Size
                                              and then Ctx.Cursors (F_Server_Flags).Predecessor = F_Server_Tag
                                              and then Ctx.Cursors (F_Server_Flags).First = (Ctx.Cursors (F_Server_Tag).Last + 1)
                                              and then (if Structural_Valid (Ctx.Cursors (F_Server_Binder))
                                                   and then (Types.Bit_Length (Ctx.Cursors (F_Server_Kind).Value.Server_Kind_Value) = Types.Bit_Length (Convert (BK_STRONG_BINDER))
                                                     or Types.Bit_Length (Ctx.Cursors (F_Server_Kind).Value.Server_Kind_Value) = Types.Bit_Length (Convert (BK_WEAK_BINDER))) then
                                                 (Ctx.Cursors (F_Server_Binder).Last - Ctx.Cursors (F_Server_Binder).First + 1) = Binder.Value'Size
                                                   and then Ctx.Cursors (F_Server_Binder).Predecessor = F_Server_Flags
                                                   and then Ctx.Cursors (F_Server_Binder).First = (Ctx.Cursors (F_Server_Flags).Last + 1)
                                                   and then (if Structural_Valid (Ctx.Cursors (F_Server_Cookie)) then
                                                      (Ctx.Cursors (F_Server_Cookie).Last - Ctx.Cursors (F_Server_Cookie).First + 1) = Binder.Cookie'Size
                                                        and then Ctx.Cursors (F_Server_Cookie).Predecessor = F_Server_Binder
                                                        and then Ctx.Cursors (F_Server_Cookie).First = (Ctx.Cursors (F_Server_Binder).Last + 1)
                                                        and then (if Structural_Valid (Ctx.Cursors (F_Padding)) then
                                                           (Ctx.Cursors (F_Padding).Last - Ctx.Cursors (F_Padding).First + 1) = Service_Manager.MBZ_7_Base'Size
                                                             and then Ctx.Cursors (F_Padding).Predecessor = F_Server_Cookie
                                                             and then Ctx.Cursors (F_Padding).First = (Ctx.Cursors (F_Server_Cookie).Last + 1)
                                                             and then (if Structural_Valid (Ctx.Cursors (F_Allow_Isolated)) then
                                                                (Ctx.Cursors (F_Allow_Isolated).Last - Ctx.Cursors (F_Allow_Isolated).First + 1) = Builtin_Types.Boolean_Base'Size
                                                                  and then Ctx.Cursors (F_Allow_Isolated).Predecessor = F_Padding
                                                                  and then Ctx.Cursors (F_Allow_Isolated).First = (Ctx.Cursors (F_Padding).Last + 1)
                                                                  and then (if Structural_Valid (Ctx.Cursors (F_Dump_Flags)) then
                                                                     (Ctx.Cursors (F_Dump_Flags).Last - Ctx.Cursors (F_Dump_Flags).First + 1) = Service_Manager.Integer_Base'Size
                                                                       and then Ctx.Cursors (F_Dump_Flags).Predecessor = F_Allow_Isolated
                                                                       and then Ctx.Cursors (F_Dump_Flags).First = (Ctx.Cursors (F_Allow_Isolated).Last + 1))))))
                                              and then (if Structural_Valid (Ctx.Cursors (F_Server_Handle))
                                                   and then (Types.Bit_Length (Ctx.Cursors (F_Server_Kind).Value.Server_Kind_Value) = Types.Bit_Length (Convert (BK_STRONG_HANDLE))
                                                     or Types.Bit_Length (Ctx.Cursors (F_Server_Kind).Value.Server_Kind_Value) = Types.Bit_Length (Convert (BK_WEAK_HANDLE))) then
                                                 (Ctx.Cursors (F_Server_Handle).Last - Ctx.Cursors (F_Server_Handle).First + 1) = Binder.Handle_Base'Size
                                                   and then Ctx.Cursors (F_Server_Handle).Predecessor = F_Server_Flags
                                                   and then Ctx.Cursors (F_Server_Handle).First = (Ctx.Cursors (F_Server_Flags).Last + 1)
                                                   and then (if Structural_Valid (Ctx.Cursors (F_Server_Unused_Padding)) then
                                                      (Ctx.Cursors (F_Server_Unused_Padding).Last - Ctx.Cursors (F_Server_Unused_Padding).First + 1) = Binder.MBZ32_Base'Size
                                                        and then Ctx.Cursors (F_Server_Unused_Padding).Predecessor = F_Server_Handle
                                                        and then Ctx.Cursors (F_Server_Unused_Padding).First = (Ctx.Cursors (F_Server_Handle).Last + 1)
                                                        and then (if Structural_Valid (Ctx.Cursors (F_Server_Cookie)) then
                                                           (Ctx.Cursors (F_Server_Cookie).Last - Ctx.Cursors (F_Server_Cookie).First + 1) = Binder.Cookie'Size
                                                             and then Ctx.Cursors (F_Server_Cookie).Predecessor = F_Server_Unused_Padding
                                                             and then Ctx.Cursors (F_Server_Cookie).First = (Ctx.Cursors (F_Server_Unused_Padding).Last + 1)
                                                             and then (if Structural_Valid (Ctx.Cursors (F_Padding)) then
                                                                (Ctx.Cursors (F_Padding).Last - Ctx.Cursors (F_Padding).First + 1) = Service_Manager.MBZ_7_Base'Size
                                                                  and then Ctx.Cursors (F_Padding).Predecessor = F_Server_Cookie
                                                                  and then Ctx.Cursors (F_Padding).First = (Ctx.Cursors (F_Server_Cookie).Last + 1)
                                                                  and then (if Structural_Valid (Ctx.Cursors (F_Allow_Isolated)) then
                                                                     (Ctx.Cursors (F_Allow_Isolated).Last - Ctx.Cursors (F_Allow_Isolated).First + 1) = Builtin_Types.Boolean_Base'Size
                                                                       and then Ctx.Cursors (F_Allow_Isolated).Predecessor = F_Padding
                                                                       and then Ctx.Cursors (F_Allow_Isolated).First = (Ctx.Cursors (F_Padding).Last + 1)
                                                                       and then (if Structural_Valid (Ctx.Cursors (F_Dump_Flags)) then
                                                                          (Ctx.Cursors (F_Dump_Flags).Last - Ctx.Cursors (F_Dump_Flags).First + 1) = Service_Manager.Integer_Base'Size
                                                                            and then Ctx.Cursors (F_Dump_Flags).Predecessor = F_Allow_Isolated
                                                                            and then Ctx.Cursors (F_Dump_Flags).First = (Ctx.Cursors (F_Allow_Isolated).Last + 1))))))))))))));
               if Fld = F_Len then
                  Ctx.Cursors (Successor (Ctx, Fld)) := (State => S_Invalid, Predecessor => Fld);
               elsif Fld = F_Name then
                  Ctx.Cursors (Successor (Ctx, Fld)) := (State => S_Invalid, Predecessor => Fld);
               elsif Fld = F_Server_Kind then
                  Ctx.Cursors (Successor (Ctx, Fld)) := (State => S_Invalid, Predecessor => Fld);
               elsif Fld = F_Server_Arity then
                  Ctx.Cursors (Successor (Ctx, Fld)) := (State => S_Invalid, Predecessor => Fld);
               elsif Fld = F_Server_Tag then
                  Ctx.Cursors (Successor (Ctx, Fld)) := (State => S_Invalid, Predecessor => Fld);
               elsif Fld = F_Server_Legacy_Flags then
                  Ctx.Cursors (Successor (Ctx, Fld)) := (State => S_Invalid, Predecessor => Fld);
               elsif Fld = F_Server_Has_Parent then
                  Ctx.Cursors (Successor (Ctx, Fld)) := (State => S_Invalid, Predecessor => Fld);
               elsif Fld = F_Server_Flags then
                  Ctx.Cursors (Successor (Ctx, Fld)) := (State => S_Invalid, Predecessor => Fld);
               elsif Fld = F_Server_FD then
                  Ctx.Cursors (Successor (Ctx, Fld)) := (State => S_Invalid, Predecessor => Fld);
               elsif Fld = F_Server_Num_FDs then
                  Ctx.Cursors (Successor (Ctx, Fld)) := (State => S_Invalid, Predecessor => Fld);
               elsif Fld = F_Server_Padding then
                  Ctx.Cursors (Successor (Ctx, Fld)) := (State => S_Invalid, Predecessor => Fld);
               elsif Fld = F_Server_Binder then
                  Ctx.Cursors (Successor (Ctx, Fld)) := (State => S_Invalid, Predecessor => Fld);
               elsif Fld = F_Server_Handle then
                  Ctx.Cursors (Successor (Ctx, Fld)) := (State => S_Invalid, Predecessor => Fld);
               elsif Fld = F_Server_Parent then
                  Ctx.Cursors (Successor (Ctx, Fld)) := (State => S_Invalid, Predecessor => Fld);
               elsif Fld = F_Server_Buffer then
                  Ctx.Cursors (Successor (Ctx, Fld)) := (State => S_Invalid, Predecessor => Fld);
               elsif Fld = F_Server_Unused_Padding then
                  Ctx.Cursors (Successor (Ctx, Fld)) := (State => S_Invalid, Predecessor => Fld);
               elsif Fld = F_Server_Parent_Offset then
                  Ctx.Cursors (Successor (Ctx, Fld)) := (State => S_Invalid, Predecessor => Fld);
               elsif Fld = F_Server_Length then
                  Ctx.Cursors (Successor (Ctx, Fld)) := (State => S_Invalid, Predecessor => Fld);
               elsif Fld = F_Server_Cookie then
                  Ctx.Cursors (Successor (Ctx, Fld)) := (State => S_Invalid, Predecessor => Fld);
               elsif Fld = F_Server_Index then
                  Ctx.Cursors (Successor (Ctx, Fld)) := (State => S_Invalid, Predecessor => Fld);
               elsif Fld = F_Padding then
                  Ctx.Cursors (Successor (Ctx, Fld)) := (State => S_Invalid, Predecessor => Fld);
               elsif Fld = F_Allow_Isolated then
                  Ctx.Cursors (Successor (Ctx, Fld)) := (State => S_Invalid, Predecessor => Fld);
               elsif Fld = F_Dump_Flags then
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
      Verify (Ctx, F_Server_Kind);
      Verify (Ctx, F_Server_Arity);
      Verify (Ctx, F_Server_Tag);
      Verify (Ctx, F_Server_Legacy_Flags);
      Verify (Ctx, F_Server_Has_Parent);
      Verify (Ctx, F_Server_Flags);
      Verify (Ctx, F_Server_FD);
      Verify (Ctx, F_Server_Num_FDs);
      Verify (Ctx, F_Server_Padding);
      Verify (Ctx, F_Server_Binder);
      Verify (Ctx, F_Server_Handle);
      Verify (Ctx, F_Server_Parent);
      Verify (Ctx, F_Server_Buffer);
      Verify (Ctx, F_Server_Unused_Padding);
      Verify (Ctx, F_Server_Parent_Offset);
      Verify (Ctx, F_Server_Length);
      Verify (Ctx, F_Server_Cookie);
      Verify (Ctx, F_Server_Index);
      Verify (Ctx, F_Padding);
      Verify (Ctx, F_Allow_Isolated);
      Verify (Ctx, F_Dump_Flags);
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
      and then Structural_Valid (Ctx, F_Name)
      and then Valid (Ctx, F_Server_Kind)
      and then Valid (Ctx, F_Server_Arity)
      and then Valid (Ctx, F_Server_Tag)
      and then (Types.Bit_Length (Ctx.Cursors (F_Server_Arity).Value.Server_Arity_Value) = Types.Bit_Length (Convert (BA_SINGLE))
        or (Types.Bit_Length (Ctx.Cursors (F_Server_Kind).Value.Server_Kind_Value) = Types.Bit_Length (Convert (BK_FD))
          and Types.Bit_Length (Ctx.Cursors (F_Server_Arity).Value.Server_Arity_Value) = Types.Bit_Length (Convert (BA_ARRAY))))
      and then ((Valid (Ctx, F_Server_Legacy_Flags)
          and then Types.Bit_Length (Ctx.Cursors (F_Server_Kind).Value.Server_Kind_Value) = Types.Bit_Length (Convert (BK_FD))
          and then ((Valid (Ctx, F_Server_FD)
              and then Types.Bit_Length (Ctx.Cursors (F_Server_Arity).Value.Server_Arity_Value) = Types.Bit_Length (Convert (BA_SINGLE))
              and then Valid (Ctx, F_Server_Cookie)
              and then Valid (Ctx, F_Padding)
              and then Valid (Ctx, F_Allow_Isolated)
              and then Valid (Ctx, F_Dump_Flags))
            or (Valid (Ctx, F_Server_Num_FDs)
              and then Types.Bit_Length (Ctx.Cursors (F_Server_Arity).Value.Server_Arity_Value) = Types.Bit_Length (Convert (BA_ARRAY))
              and then Valid (Ctx, F_Server_Parent)
              and then Valid (Ctx, F_Server_Parent_Offset)
              and then Valid (Ctx, F_Padding)
              and then Valid (Ctx, F_Allow_Isolated)
              and then Valid (Ctx, F_Dump_Flags))))
        or (Valid (Ctx, F_Server_Has_Parent)
          and then Types.Bit_Length (Ctx.Cursors (F_Server_Kind).Value.Server_Kind_Value) = Types.Bit_Length (Convert (BK_POINTER))
          and then Valid (Ctx, F_Server_Padding)
          and then Valid (Ctx, F_Server_Buffer)
          and then Valid (Ctx, F_Server_Length)
          and then ((Valid (Ctx, F_Padding)
              and then Types.Bit_Length (Ctx.Cursors (F_Server_Has_Parent).Value.Server_Has_Parent_Value) = Types.Bit_Length (Convert (False))
              and then Valid (Ctx, F_Allow_Isolated)
              and then Valid (Ctx, F_Dump_Flags))
            or (Valid (Ctx, F_Server_Index)
              and then Types.Bit_Length (Ctx.Cursors (F_Server_Has_Parent).Value.Server_Has_Parent_Value) = Types.Bit_Length (Convert (True))
              and then Valid (Ctx, F_Padding)
              and then Valid (Ctx, F_Allow_Isolated)
              and then Valid (Ctx, F_Dump_Flags))))
        or (Valid (Ctx, F_Server_Flags)
          and then (Types.Bit_Length (Ctx.Cursors (F_Server_Kind).Value.Server_Kind_Value) /= Types.Bit_Length (Convert (BK_POINTER))
            and Types.Bit_Length (Ctx.Cursors (F_Server_Kind).Value.Server_Kind_Value) /= Types.Bit_Length (Convert (BK_FD)))
          and then ((Valid (Ctx, F_Server_Binder)
              and then (Types.Bit_Length (Ctx.Cursors (F_Server_Kind).Value.Server_Kind_Value) = Types.Bit_Length (Convert (BK_STRONG_BINDER))
                or Types.Bit_Length (Ctx.Cursors (F_Server_Kind).Value.Server_Kind_Value) = Types.Bit_Length (Convert (BK_WEAK_BINDER)))
              and then Valid (Ctx, F_Server_Cookie)
              and then Valid (Ctx, F_Padding)
              and then Valid (Ctx, F_Allow_Isolated)
              and then Valid (Ctx, F_Dump_Flags))
            or (Valid (Ctx, F_Server_Handle)
              and then (Types.Bit_Length (Ctx.Cursors (F_Server_Kind).Value.Server_Kind_Value) = Types.Bit_Length (Convert (BK_STRONG_HANDLE))
                or Types.Bit_Length (Ctx.Cursors (F_Server_Kind).Value.Server_Kind_Value) = Types.Bit_Length (Convert (BK_WEAK_HANDLE)))
              and then Valid (Ctx, F_Server_Unused_Padding)
              and then Valid (Ctx, F_Server_Cookie)
              and then Valid (Ctx, F_Padding)
              and then Valid (Ctx, F_Allow_Isolated)
              and then Valid (Ctx, F_Dump_Flags))))));

   function Valid_Message (Ctx : Context) return Boolean is
     (Valid (Ctx, F_Len)
      and then Valid (Ctx, F_Name)
      and then Valid (Ctx, F_Server_Kind)
      and then Valid (Ctx, F_Server_Arity)
      and then Valid (Ctx, F_Server_Tag)
      and then (Types.Bit_Length (Ctx.Cursors (F_Server_Arity).Value.Server_Arity_Value) = Types.Bit_Length (Convert (BA_SINGLE))
        or (Types.Bit_Length (Ctx.Cursors (F_Server_Kind).Value.Server_Kind_Value) = Types.Bit_Length (Convert (BK_FD))
          and Types.Bit_Length (Ctx.Cursors (F_Server_Arity).Value.Server_Arity_Value) = Types.Bit_Length (Convert (BA_ARRAY))))
      and then ((Valid (Ctx, F_Server_Legacy_Flags)
          and then Types.Bit_Length (Ctx.Cursors (F_Server_Kind).Value.Server_Kind_Value) = Types.Bit_Length (Convert (BK_FD))
          and then ((Valid (Ctx, F_Server_FD)
              and then Types.Bit_Length (Ctx.Cursors (F_Server_Arity).Value.Server_Arity_Value) = Types.Bit_Length (Convert (BA_SINGLE))
              and then Valid (Ctx, F_Server_Cookie)
              and then Valid (Ctx, F_Padding)
              and then Valid (Ctx, F_Allow_Isolated)
              and then Valid (Ctx, F_Dump_Flags))
            or (Valid (Ctx, F_Server_Num_FDs)
              and then Types.Bit_Length (Ctx.Cursors (F_Server_Arity).Value.Server_Arity_Value) = Types.Bit_Length (Convert (BA_ARRAY))
              and then Valid (Ctx, F_Server_Parent)
              and then Valid (Ctx, F_Server_Parent_Offset)
              and then Valid (Ctx, F_Padding)
              and then Valid (Ctx, F_Allow_Isolated)
              and then Valid (Ctx, F_Dump_Flags))))
        or (Valid (Ctx, F_Server_Has_Parent)
          and then Types.Bit_Length (Ctx.Cursors (F_Server_Kind).Value.Server_Kind_Value) = Types.Bit_Length (Convert (BK_POINTER))
          and then Valid (Ctx, F_Server_Padding)
          and then Valid (Ctx, F_Server_Buffer)
          and then Valid (Ctx, F_Server_Length)
          and then ((Valid (Ctx, F_Padding)
              and then Types.Bit_Length (Ctx.Cursors (F_Server_Has_Parent).Value.Server_Has_Parent_Value) = Types.Bit_Length (Convert (False))
              and then Valid (Ctx, F_Allow_Isolated)
              and then Valid (Ctx, F_Dump_Flags))
            or (Valid (Ctx, F_Server_Index)
              and then Types.Bit_Length (Ctx.Cursors (F_Server_Has_Parent).Value.Server_Has_Parent_Value) = Types.Bit_Length (Convert (True))
              and then Valid (Ctx, F_Padding)
              and then Valid (Ctx, F_Allow_Isolated)
              and then Valid (Ctx, F_Dump_Flags))))
        or (Valid (Ctx, F_Server_Flags)
          and then (Types.Bit_Length (Ctx.Cursors (F_Server_Kind).Value.Server_Kind_Value) /= Types.Bit_Length (Convert (BK_POINTER))
            and Types.Bit_Length (Ctx.Cursors (F_Server_Kind).Value.Server_Kind_Value) /= Types.Bit_Length (Convert (BK_FD)))
          and then ((Valid (Ctx, F_Server_Binder)
              and then (Types.Bit_Length (Ctx.Cursors (F_Server_Kind).Value.Server_Kind_Value) = Types.Bit_Length (Convert (BK_STRONG_BINDER))
                or Types.Bit_Length (Ctx.Cursors (F_Server_Kind).Value.Server_Kind_Value) = Types.Bit_Length (Convert (BK_WEAK_BINDER)))
              and then Valid (Ctx, F_Server_Cookie)
              and then Valid (Ctx, F_Padding)
              and then Valid (Ctx, F_Allow_Isolated)
              and then Valid (Ctx, F_Dump_Flags))
            or (Valid (Ctx, F_Server_Handle)
              and then (Types.Bit_Length (Ctx.Cursors (F_Server_Kind).Value.Server_Kind_Value) = Types.Bit_Length (Convert (BK_STRONG_HANDLE))
                or Types.Bit_Length (Ctx.Cursors (F_Server_Kind).Value.Server_Kind_Value) = Types.Bit_Length (Convert (BK_WEAK_HANDLE)))
              and then Valid (Ctx, F_Server_Unused_Padding)
              and then Valid (Ctx, F_Server_Cookie)
              and then Valid (Ctx, F_Padding)
              and then Valid (Ctx, F_Allow_Isolated)
              and then Valid (Ctx, F_Dump_Flags))))));

   function Incomplete_Message (Ctx : Context) return Boolean is
     (Incomplete (Ctx, F_Len)
      or Incomplete (Ctx, F_Name)
      or Incomplete (Ctx, F_Server_Kind)
      or Incomplete (Ctx, F_Server_Arity)
      or Incomplete (Ctx, F_Server_Tag)
      or Incomplete (Ctx, F_Server_Legacy_Flags)
      or Incomplete (Ctx, F_Server_Has_Parent)
      or Incomplete (Ctx, F_Server_Flags)
      or Incomplete (Ctx, F_Server_FD)
      or Incomplete (Ctx, F_Server_Num_FDs)
      or Incomplete (Ctx, F_Server_Padding)
      or Incomplete (Ctx, F_Server_Binder)
      or Incomplete (Ctx, F_Server_Handle)
      or Incomplete (Ctx, F_Server_Parent)
      or Incomplete (Ctx, F_Server_Buffer)
      or Incomplete (Ctx, F_Server_Unused_Padding)
      or Incomplete (Ctx, F_Server_Parent_Offset)
      or Incomplete (Ctx, F_Server_Length)
      or Incomplete (Ctx, F_Server_Cookie)
      or Incomplete (Ctx, F_Server_Index)
      or Incomplete (Ctx, F_Padding)
      or Incomplete (Ctx, F_Allow_Isolated)
      or Incomplete (Ctx, F_Dump_Flags));

   function Get_Len (Ctx : Context) return Service_Manager.Len is
     (Ctx.Cursors (F_Len).Value.Len_Value);

   function Get_Server_Kind (Ctx : Context) return Binder.Binder_Kind is
     (Convert (Ctx.Cursors (F_Server_Kind).Value.Server_Kind_Value));

   function Get_Server_Arity (Ctx : Context) return Binder.Binder_Arity is
     (Convert (Ctx.Cursors (F_Server_Arity).Value.Server_Arity_Value));

   function Get_Server_Tag (Ctx : Context) return Binder.Binder_Tag is
     (Ctx.Cursors (F_Server_Tag).Value.Server_Tag_Value);

   function Get_Server_Legacy_Flags (Ctx : Context) return Binder.MBZ32 is
     (Ctx.Cursors (F_Server_Legacy_Flags).Value.Server_Legacy_Flags_Value);

   function Get_Server_Has_Parent (Ctx : Context) return Boolean is
     (Convert (Ctx.Cursors (F_Server_Has_Parent).Value.Server_Has_Parent_Value));

   function Get_Server_Flags (Ctx : Context) return Binder.Flat_Binder_Flags is
     (Convert (Ctx.Cursors (F_Server_Flags).Value.Server_Flags_Value));

   function Get_Server_FD (Ctx : Context) return Binder.Handle is
     (Ctx.Cursors (F_Server_FD).Value.Server_FD_Value);

   function Get_Server_Num_FDs (Ctx : Context) return Binder.Count is
     (Ctx.Cursors (F_Server_Num_FDs).Value.Server_Num_FDs_Value);

   function Get_Server_Padding (Ctx : Context) return Binder.MBZ31 is
     (Ctx.Cursors (F_Server_Padding).Value.Server_Padding_Value);

   function Get_Server_Binder (Ctx : Context) return Binder.Value is
     (Ctx.Cursors (F_Server_Binder).Value.Server_Binder_Value);

   function Get_Server_Handle (Ctx : Context) return Binder.Handle is
     (Ctx.Cursors (F_Server_Handle).Value.Server_Handle_Value);

   function Get_Server_Parent (Ctx : Context) return Binder.Index is
     (Ctx.Cursors (F_Server_Parent).Value.Server_Parent_Value);

   function Get_Server_Buffer (Ctx : Context) return Binder.Index is
     (Ctx.Cursors (F_Server_Buffer).Value.Server_Buffer_Value);

   function Get_Server_Unused_Padding (Ctx : Context) return Binder.MBZ32 is
     (Ctx.Cursors (F_Server_Unused_Padding).Value.Server_Unused_Padding_Value);

   function Get_Server_Parent_Offset (Ctx : Context) return Binder.Offset is
     (Ctx.Cursors (F_Server_Parent_Offset).Value.Server_Parent_Offset_Value);

   function Get_Server_Length (Ctx : Context) return Binder.Length is
     (Ctx.Cursors (F_Server_Length).Value.Server_Length_Value);

   function Get_Server_Cookie (Ctx : Context) return Binder.Cookie is
     (Ctx.Cursors (F_Server_Cookie).Value.Server_Cookie_Value);

   function Get_Server_Index (Ctx : Context) return Binder.Index is
     (Ctx.Cursors (F_Server_Index).Value.Server_Index_Value);

   function Get_Padding (Ctx : Context) return Service_Manager.MBZ_7 is
     (Ctx.Cursors (F_Padding).Value.Padding_Value);

   function Get_Allow_Isolated (Ctx : Context) return Boolean is
     (Convert (Ctx.Cursors (F_Allow_Isolated).Value.Allow_Isolated_Value));

   function Get_Dump_Flags (Ctx : Context) return Service_Manager.Integer is
     (Ctx.Cursors (F_Dump_Flags).Value.Dump_Flags_Value);

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
      procedure Insert is new Types.Insert (Service_Manager.Len_Base);
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
      procedure Insert is new Types.Insert (Service_Manager.MBZ_7_Base);
      procedure Insert is new Types.Insert (Service_Manager.Integer_Base);
   begin
      Fst := First;
      Lst := Last;
      case Val.Fld is
         when F_Initial =>
            null;
         when F_Len =>
            Insert (Val.Len_Value, Ctx.Buffer.all (Buffer_First .. Buffer_Last), Offset);
         when F_Name =>
            null;
         when F_Server_Kind =>
            Insert (Val.Server_Kind_Value, Ctx.Buffer.all (Buffer_First .. Buffer_Last), Offset);
         when F_Server_Arity =>
            Insert (Val.Server_Arity_Value, Ctx.Buffer.all (Buffer_First .. Buffer_Last), Offset);
         when F_Server_Tag =>
            Insert (Val.Server_Tag_Value, Ctx.Buffer.all (Buffer_First .. Buffer_Last), Offset);
         when F_Server_Legacy_Flags =>
            Insert (Val.Server_Legacy_Flags_Value, Ctx.Buffer.all (Buffer_First .. Buffer_Last), Offset);
         when F_Server_Has_Parent =>
            Insert (Val.Server_Has_Parent_Value, Ctx.Buffer.all (Buffer_First .. Buffer_Last), Offset);
         when F_Server_Flags =>
            Insert (Val.Server_Flags_Value, Ctx.Buffer.all (Buffer_First .. Buffer_Last), Offset);
         when F_Server_FD =>
            Insert (Val.Server_FD_Value, Ctx.Buffer.all (Buffer_First .. Buffer_Last), Offset);
         when F_Server_Num_FDs =>
            Insert (Val.Server_Num_FDs_Value, Ctx.Buffer.all (Buffer_First .. Buffer_Last), Offset);
         when F_Server_Padding =>
            Insert (Val.Server_Padding_Value, Ctx.Buffer.all (Buffer_First .. Buffer_Last), Offset);
         when F_Server_Binder =>
            Insert (Val.Server_Binder_Value, Ctx.Buffer.all (Buffer_First .. Buffer_Last), Offset);
         when F_Server_Handle =>
            Insert (Val.Server_Handle_Value, Ctx.Buffer.all (Buffer_First .. Buffer_Last), Offset);
         when F_Server_Parent =>
            Insert (Val.Server_Parent_Value, Ctx.Buffer.all (Buffer_First .. Buffer_Last), Offset);
         when F_Server_Buffer =>
            Insert (Val.Server_Buffer_Value, Ctx.Buffer.all (Buffer_First .. Buffer_Last), Offset);
         when F_Server_Unused_Padding =>
            Insert (Val.Server_Unused_Padding_Value, Ctx.Buffer.all (Buffer_First .. Buffer_Last), Offset);
         when F_Server_Parent_Offset =>
            Insert (Val.Server_Parent_Offset_Value, Ctx.Buffer.all (Buffer_First .. Buffer_Last), Offset);
         when F_Server_Length =>
            Insert (Val.Server_Length_Value, Ctx.Buffer.all (Buffer_First .. Buffer_Last), Offset);
         when F_Server_Cookie =>
            Insert (Val.Server_Cookie_Value, Ctx.Buffer.all (Buffer_First .. Buffer_Last), Offset);
         when F_Server_Index =>
            Insert (Val.Server_Index_Value, Ctx.Buffer.all (Buffer_First .. Buffer_Last), Offset);
         when F_Padding =>
            Insert (Val.Padding_Value, Ctx.Buffer.all (Buffer_First .. Buffer_Last), Offset);
         when F_Allow_Isolated =>
            Insert (Val.Allow_Isolated_Value, Ctx.Buffer.all (Buffer_First .. Buffer_Last), Offset);
         when F_Dump_Flags =>
            Insert (Val.Dump_Flags_Value, Ctx.Buffer.all (Buffer_First .. Buffer_Last), Offset);
         when F_Final =>
            null;
      end case;
   end Set_Field_Value;

   procedure Set_Len (Ctx : in out Context; Val : Service_Manager.Len) is
      Field_Value : constant Field_Dependent_Value := (F_Len, Val);
      First, Last : Types.Bit_Index;
   begin
      Reset_Dependent_Fields (Ctx, F_Len);
      Set_Field_Value (Ctx, Field_Value, First, Last);
      Ctx := (Ctx.Buffer_First, Ctx.Buffer_Last, Ctx.First, Last, Ctx.Buffer, Ctx.Cursors);
      Ctx.Cursors (F_Len) := (State => S_Valid, First => First, Last => Last, Value => Field_Value, Predecessor => Ctx.Cursors (F_Len).Predecessor);
      Ctx.Cursors (Successor (Ctx, F_Len)) := (State => S_Invalid, Predecessor => F_Len);
   end Set_Len;

   procedure Set_Server_Kind (Ctx : in out Context; Val : Binder.Binder_Kind) is
      Field_Value : constant Field_Dependent_Value := (F_Server_Kind, Convert (Val));
      First, Last : Types.Bit_Index;
   begin
      Reset_Dependent_Fields (Ctx, F_Server_Kind);
      Set_Field_Value (Ctx, Field_Value, First, Last);
      Ctx := (Ctx.Buffer_First, Ctx.Buffer_Last, Ctx.First, Last, Ctx.Buffer, Ctx.Cursors);
      Ctx.Cursors (F_Server_Kind) := (State => S_Valid, First => First, Last => Last, Value => Field_Value, Predecessor => Ctx.Cursors (F_Server_Kind).Predecessor);
      Ctx.Cursors (Successor (Ctx, F_Server_Kind)) := (State => S_Invalid, Predecessor => F_Server_Kind);
   end Set_Server_Kind;

   procedure Set_Server_Arity (Ctx : in out Context; Val : Binder.Binder_Arity) is
      Field_Value : constant Field_Dependent_Value := (F_Server_Arity, Convert (Val));
      First, Last : Types.Bit_Index;
   begin
      Reset_Dependent_Fields (Ctx, F_Server_Arity);
      Set_Field_Value (Ctx, Field_Value, First, Last);
      Ctx := (Ctx.Buffer_First, Ctx.Buffer_Last, Ctx.First, Last, Ctx.Buffer, Ctx.Cursors);
      Ctx.Cursors (F_Server_Arity) := (State => S_Valid, First => First, Last => Last, Value => Field_Value, Predecessor => Ctx.Cursors (F_Server_Arity).Predecessor);
      Ctx.Cursors (Successor (Ctx, F_Server_Arity)) := (State => S_Invalid, Predecessor => F_Server_Arity);
   end Set_Server_Arity;

   procedure Set_Server_Tag (Ctx : in out Context; Val : Binder.Binder_Tag) is
      Field_Value : constant Field_Dependent_Value := (F_Server_Tag, Val);
      First, Last : Types.Bit_Index;
   begin
      Reset_Dependent_Fields (Ctx, F_Server_Tag);
      Set_Field_Value (Ctx, Field_Value, First, Last);
      Ctx := (Ctx.Buffer_First, Ctx.Buffer_Last, Ctx.First, Last, Ctx.Buffer, Ctx.Cursors);
      Ctx.Cursors (F_Server_Tag) := (State => S_Valid, First => First, Last => Last, Value => Field_Value, Predecessor => Ctx.Cursors (F_Server_Tag).Predecessor);
      Ctx.Cursors (Successor (Ctx, F_Server_Tag)) := (State => S_Invalid, Predecessor => F_Server_Tag);
   end Set_Server_Tag;

   procedure Set_Server_Legacy_Flags (Ctx : in out Context; Val : Binder.MBZ32) is
      Field_Value : constant Field_Dependent_Value := (F_Server_Legacy_Flags, Val);
      First, Last : Types.Bit_Index;
   begin
      Reset_Dependent_Fields (Ctx, F_Server_Legacy_Flags);
      Set_Field_Value (Ctx, Field_Value, First, Last);
      Ctx := (Ctx.Buffer_First, Ctx.Buffer_Last, Ctx.First, Last, Ctx.Buffer, Ctx.Cursors);
      Ctx.Cursors (F_Server_Legacy_Flags) := (State => S_Valid, First => First, Last => Last, Value => Field_Value, Predecessor => Ctx.Cursors (F_Server_Legacy_Flags).Predecessor);
      Ctx.Cursors (Successor (Ctx, F_Server_Legacy_Flags)) := (State => S_Invalid, Predecessor => F_Server_Legacy_Flags);
   end Set_Server_Legacy_Flags;

   procedure Set_Server_Has_Parent (Ctx : in out Context; Val : Boolean) is
      Field_Value : constant Field_Dependent_Value := (F_Server_Has_Parent, Convert (Val));
      First, Last : Types.Bit_Index;
   begin
      Reset_Dependent_Fields (Ctx, F_Server_Has_Parent);
      Set_Field_Value (Ctx, Field_Value, First, Last);
      Ctx := (Ctx.Buffer_First, Ctx.Buffer_Last, Ctx.First, Last, Ctx.Buffer, Ctx.Cursors);
      Ctx.Cursors (F_Server_Has_Parent) := (State => S_Valid, First => First, Last => Last, Value => Field_Value, Predecessor => Ctx.Cursors (F_Server_Has_Parent).Predecessor);
      Ctx.Cursors (Successor (Ctx, F_Server_Has_Parent)) := (State => S_Invalid, Predecessor => F_Server_Has_Parent);
   end Set_Server_Has_Parent;

   procedure Set_Server_Flags (Ctx : in out Context; Val : Binder.Flat_Binder_Flags) is
      Field_Value : constant Field_Dependent_Value := (F_Server_Flags, Convert (Val));
      First, Last : Types.Bit_Index;
   begin
      Reset_Dependent_Fields (Ctx, F_Server_Flags);
      Set_Field_Value (Ctx, Field_Value, First, Last);
      Ctx := (Ctx.Buffer_First, Ctx.Buffer_Last, Ctx.First, Last, Ctx.Buffer, Ctx.Cursors);
      Ctx.Cursors (F_Server_Flags) := (State => S_Valid, First => First, Last => Last, Value => Field_Value, Predecessor => Ctx.Cursors (F_Server_Flags).Predecessor);
      Ctx.Cursors (Successor (Ctx, F_Server_Flags)) := (State => S_Invalid, Predecessor => F_Server_Flags);
   end Set_Server_Flags;

   procedure Set_Server_FD (Ctx : in out Context; Val : Binder.Handle) is
      Field_Value : constant Field_Dependent_Value := (F_Server_FD, Val);
      First, Last : Types.Bit_Index;
   begin
      Reset_Dependent_Fields (Ctx, F_Server_FD);
      Set_Field_Value (Ctx, Field_Value, First, Last);
      Ctx := (Ctx.Buffer_First, Ctx.Buffer_Last, Ctx.First, Last, Ctx.Buffer, Ctx.Cursors);
      Ctx.Cursors (F_Server_FD) := (State => S_Valid, First => First, Last => Last, Value => Field_Value, Predecessor => Ctx.Cursors (F_Server_FD).Predecessor);
      Ctx.Cursors (Successor (Ctx, F_Server_FD)) := (State => S_Invalid, Predecessor => F_Server_FD);
   end Set_Server_FD;

   procedure Set_Server_Num_FDs (Ctx : in out Context; Val : Binder.Count) is
      Field_Value : constant Field_Dependent_Value := (F_Server_Num_FDs, Val);
      First, Last : Types.Bit_Index;
   begin
      Reset_Dependent_Fields (Ctx, F_Server_Num_FDs);
      Set_Field_Value (Ctx, Field_Value, First, Last);
      Ctx := (Ctx.Buffer_First, Ctx.Buffer_Last, Ctx.First, Last, Ctx.Buffer, Ctx.Cursors);
      Ctx.Cursors (F_Server_Num_FDs) := (State => S_Valid, First => First, Last => Last, Value => Field_Value, Predecessor => Ctx.Cursors (F_Server_Num_FDs).Predecessor);
      Ctx.Cursors (Successor (Ctx, F_Server_Num_FDs)) := (State => S_Invalid, Predecessor => F_Server_Num_FDs);
   end Set_Server_Num_FDs;

   procedure Set_Server_Padding (Ctx : in out Context; Val : Binder.MBZ31) is
      Field_Value : constant Field_Dependent_Value := (F_Server_Padding, Val);
      First, Last : Types.Bit_Index;
   begin
      Reset_Dependent_Fields (Ctx, F_Server_Padding);
      Set_Field_Value (Ctx, Field_Value, First, Last);
      Ctx := (Ctx.Buffer_First, Ctx.Buffer_Last, Ctx.First, Last, Ctx.Buffer, Ctx.Cursors);
      Ctx.Cursors (F_Server_Padding) := (State => S_Valid, First => First, Last => Last, Value => Field_Value, Predecessor => Ctx.Cursors (F_Server_Padding).Predecessor);
      Ctx.Cursors (Successor (Ctx, F_Server_Padding)) := (State => S_Invalid, Predecessor => F_Server_Padding);
   end Set_Server_Padding;

   procedure Set_Server_Binder (Ctx : in out Context; Val : Binder.Value) is
      Field_Value : constant Field_Dependent_Value := (F_Server_Binder, Val);
      First, Last : Types.Bit_Index;
   begin
      Reset_Dependent_Fields (Ctx, F_Server_Binder);
      Set_Field_Value (Ctx, Field_Value, First, Last);
      Ctx := (Ctx.Buffer_First, Ctx.Buffer_Last, Ctx.First, Last, Ctx.Buffer, Ctx.Cursors);
      Ctx.Cursors (F_Server_Binder) := (State => S_Valid, First => First, Last => Last, Value => Field_Value, Predecessor => Ctx.Cursors (F_Server_Binder).Predecessor);
      Ctx.Cursors (Successor (Ctx, F_Server_Binder)) := (State => S_Invalid, Predecessor => F_Server_Binder);
   end Set_Server_Binder;

   procedure Set_Server_Handle (Ctx : in out Context; Val : Binder.Handle) is
      Field_Value : constant Field_Dependent_Value := (F_Server_Handle, Val);
      First, Last : Types.Bit_Index;
   begin
      Reset_Dependent_Fields (Ctx, F_Server_Handle);
      Set_Field_Value (Ctx, Field_Value, First, Last);
      Ctx := (Ctx.Buffer_First, Ctx.Buffer_Last, Ctx.First, Last, Ctx.Buffer, Ctx.Cursors);
      Ctx.Cursors (F_Server_Handle) := (State => S_Valid, First => First, Last => Last, Value => Field_Value, Predecessor => Ctx.Cursors (F_Server_Handle).Predecessor);
      Ctx.Cursors (Successor (Ctx, F_Server_Handle)) := (State => S_Invalid, Predecessor => F_Server_Handle);
   end Set_Server_Handle;

   procedure Set_Server_Parent (Ctx : in out Context; Val : Binder.Index) is
      Field_Value : constant Field_Dependent_Value := (F_Server_Parent, Val);
      First, Last : Types.Bit_Index;
   begin
      Reset_Dependent_Fields (Ctx, F_Server_Parent);
      Set_Field_Value (Ctx, Field_Value, First, Last);
      Ctx := (Ctx.Buffer_First, Ctx.Buffer_Last, Ctx.First, Last, Ctx.Buffer, Ctx.Cursors);
      Ctx.Cursors (F_Server_Parent) := (State => S_Valid, First => First, Last => Last, Value => Field_Value, Predecessor => Ctx.Cursors (F_Server_Parent).Predecessor);
      Ctx.Cursors (Successor (Ctx, F_Server_Parent)) := (State => S_Invalid, Predecessor => F_Server_Parent);
   end Set_Server_Parent;

   procedure Set_Server_Buffer (Ctx : in out Context; Val : Binder.Index) is
      Field_Value : constant Field_Dependent_Value := (F_Server_Buffer, Val);
      First, Last : Types.Bit_Index;
   begin
      Reset_Dependent_Fields (Ctx, F_Server_Buffer);
      Set_Field_Value (Ctx, Field_Value, First, Last);
      Ctx := (Ctx.Buffer_First, Ctx.Buffer_Last, Ctx.First, Last, Ctx.Buffer, Ctx.Cursors);
      Ctx.Cursors (F_Server_Buffer) := (State => S_Valid, First => First, Last => Last, Value => Field_Value, Predecessor => Ctx.Cursors (F_Server_Buffer).Predecessor);
      Ctx.Cursors (Successor (Ctx, F_Server_Buffer)) := (State => S_Invalid, Predecessor => F_Server_Buffer);
   end Set_Server_Buffer;

   procedure Set_Server_Unused_Padding (Ctx : in out Context; Val : Binder.MBZ32) is
      Field_Value : constant Field_Dependent_Value := (F_Server_Unused_Padding, Val);
      First, Last : Types.Bit_Index;
   begin
      Reset_Dependent_Fields (Ctx, F_Server_Unused_Padding);
      Set_Field_Value (Ctx, Field_Value, First, Last);
      Ctx := (Ctx.Buffer_First, Ctx.Buffer_Last, Ctx.First, Last, Ctx.Buffer, Ctx.Cursors);
      Ctx.Cursors (F_Server_Unused_Padding) := (State => S_Valid, First => First, Last => Last, Value => Field_Value, Predecessor => Ctx.Cursors (F_Server_Unused_Padding).Predecessor);
      Ctx.Cursors (Successor (Ctx, F_Server_Unused_Padding)) := (State => S_Invalid, Predecessor => F_Server_Unused_Padding);
   end Set_Server_Unused_Padding;

   procedure Set_Server_Parent_Offset (Ctx : in out Context; Val : Binder.Offset) is
      Field_Value : constant Field_Dependent_Value := (F_Server_Parent_Offset, Val);
      First, Last : Types.Bit_Index;
   begin
      Reset_Dependent_Fields (Ctx, F_Server_Parent_Offset);
      Set_Field_Value (Ctx, Field_Value, First, Last);
      Ctx := (Ctx.Buffer_First, Ctx.Buffer_Last, Ctx.First, Last, Ctx.Buffer, Ctx.Cursors);
      Ctx.Cursors (F_Server_Parent_Offset) := (State => S_Valid, First => First, Last => Last, Value => Field_Value, Predecessor => Ctx.Cursors (F_Server_Parent_Offset).Predecessor);
      Ctx.Cursors (Successor (Ctx, F_Server_Parent_Offset)) := (State => S_Invalid, Predecessor => F_Server_Parent_Offset);
   end Set_Server_Parent_Offset;

   procedure Set_Server_Length (Ctx : in out Context; Val : Binder.Length) is
      Field_Value : constant Field_Dependent_Value := (F_Server_Length, Val);
      First, Last : Types.Bit_Index;
   begin
      Reset_Dependent_Fields (Ctx, F_Server_Length);
      Set_Field_Value (Ctx, Field_Value, First, Last);
      Ctx := (Ctx.Buffer_First, Ctx.Buffer_Last, Ctx.First, Last, Ctx.Buffer, Ctx.Cursors);
      Ctx.Cursors (F_Server_Length) := (State => S_Valid, First => First, Last => Last, Value => Field_Value, Predecessor => Ctx.Cursors (F_Server_Length).Predecessor);
      Ctx.Cursors (Successor (Ctx, F_Server_Length)) := (State => S_Invalid, Predecessor => F_Server_Length);
   end Set_Server_Length;

   procedure Set_Server_Cookie (Ctx : in out Context; Val : Binder.Cookie) is
      Field_Value : constant Field_Dependent_Value := (F_Server_Cookie, Val);
      First, Last : Types.Bit_Index;
   begin
      Reset_Dependent_Fields (Ctx, F_Server_Cookie);
      Set_Field_Value (Ctx, Field_Value, First, Last);
      Ctx := (Ctx.Buffer_First, Ctx.Buffer_Last, Ctx.First, Last, Ctx.Buffer, Ctx.Cursors);
      Ctx.Cursors (F_Server_Cookie) := (State => S_Valid, First => First, Last => Last, Value => Field_Value, Predecessor => Ctx.Cursors (F_Server_Cookie).Predecessor);
      Ctx.Cursors (Successor (Ctx, F_Server_Cookie)) := (State => S_Invalid, Predecessor => F_Server_Cookie);
   end Set_Server_Cookie;

   procedure Set_Server_Index (Ctx : in out Context; Val : Binder.Index) is
      Field_Value : constant Field_Dependent_Value := (F_Server_Index, Val);
      First, Last : Types.Bit_Index;
   begin
      Reset_Dependent_Fields (Ctx, F_Server_Index);
      Set_Field_Value (Ctx, Field_Value, First, Last);
      Ctx := (Ctx.Buffer_First, Ctx.Buffer_Last, Ctx.First, Last, Ctx.Buffer, Ctx.Cursors);
      Ctx.Cursors (F_Server_Index) := (State => S_Valid, First => First, Last => Last, Value => Field_Value, Predecessor => Ctx.Cursors (F_Server_Index).Predecessor);
      Ctx.Cursors (Successor (Ctx, F_Server_Index)) := (State => S_Invalid, Predecessor => F_Server_Index);
   end Set_Server_Index;

   procedure Set_Padding (Ctx : in out Context; Val : Service_Manager.MBZ_7) is
      Field_Value : constant Field_Dependent_Value := (F_Padding, Val);
      First, Last : Types.Bit_Index;
   begin
      Reset_Dependent_Fields (Ctx, F_Padding);
      Set_Field_Value (Ctx, Field_Value, First, Last);
      Ctx := (Ctx.Buffer_First, Ctx.Buffer_Last, Ctx.First, Last, Ctx.Buffer, Ctx.Cursors);
      Ctx.Cursors (F_Padding) := (State => S_Valid, First => First, Last => Last, Value => Field_Value, Predecessor => Ctx.Cursors (F_Padding).Predecessor);
      Ctx.Cursors (Successor (Ctx, F_Padding)) := (State => S_Invalid, Predecessor => F_Padding);
   end Set_Padding;

   procedure Set_Allow_Isolated (Ctx : in out Context; Val : Boolean) is
      Field_Value : constant Field_Dependent_Value := (F_Allow_Isolated, Convert (Val));
      First, Last : Types.Bit_Index;
   begin
      Reset_Dependent_Fields (Ctx, F_Allow_Isolated);
      Set_Field_Value (Ctx, Field_Value, First, Last);
      Ctx := (Ctx.Buffer_First, Ctx.Buffer_Last, Ctx.First, Last, Ctx.Buffer, Ctx.Cursors);
      Ctx.Cursors (F_Allow_Isolated) := (State => S_Valid, First => First, Last => Last, Value => Field_Value, Predecessor => Ctx.Cursors (F_Allow_Isolated).Predecessor);
      Ctx.Cursors (Successor (Ctx, F_Allow_Isolated)) := (State => S_Invalid, Predecessor => F_Allow_Isolated);
   end Set_Allow_Isolated;

   procedure Set_Dump_Flags (Ctx : in out Context; Val : Service_Manager.Integer) is
      Field_Value : constant Field_Dependent_Value := (F_Dump_Flags, Val);
      First, Last : Types.Bit_Index;
   begin
      Reset_Dependent_Fields (Ctx, F_Dump_Flags);
      Set_Field_Value (Ctx, Field_Value, First, Last);
      Ctx := (Ctx.Buffer_First, Ctx.Buffer_Last, Ctx.First, Last, Ctx.Buffer, Ctx.Cursors);
      Ctx.Cursors (F_Dump_Flags) := (State => S_Valid, First => First, Last => Last, Value => Field_Value, Predecessor => Ctx.Cursors (F_Dump_Flags).Predecessor);
      Ctx.Cursors (Successor (Ctx, F_Dump_Flags)) := (State => S_Invalid, Predecessor => F_Dump_Flags);
   end Set_Dump_Flags;

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
          (Ctx.Cursors (F_Len).Last - Ctx.Cursors (F_Len).First + 1) = Service_Manager.Len_Base'Size
            and then Ctx.Cursors (F_Len).Predecessor = F_Initial
            and then Ctx.Cursors (F_Len).First = Ctx.First
            and then (if Structural_Valid (Ctx.Cursors (F_Name)) then
               (Ctx.Cursors (F_Name).Last - Ctx.Cursors (F_Name).First + 1) = Types.Bit_Length (Ctx.Cursors (F_Len).Value.Len_Value)
                 and then Ctx.Cursors (F_Name).Predecessor = F_Len
                 and then Ctx.Cursors (F_Name).First = (Ctx.Cursors (F_Len).Last + 1)
                 and then (if Structural_Valid (Ctx.Cursors (F_Server_Kind)) then
                    (Ctx.Cursors (F_Server_Kind).Last - Ctx.Cursors (F_Server_Kind).First + 1) = Binder.Binder_Kind_Base'Size
                      and then Ctx.Cursors (F_Server_Kind).Predecessor = F_Name
                      and then Ctx.Cursors (F_Server_Kind).First = (Ctx.Cursors (F_Name).Last + 1)
                      and then (if Structural_Valid (Ctx.Cursors (F_Server_Arity)) then
                         (Ctx.Cursors (F_Server_Arity).Last - Ctx.Cursors (F_Server_Arity).First + 1) = Binder.Binder_Arity_Base'Size
                           and then Ctx.Cursors (F_Server_Arity).Predecessor = F_Server_Kind
                           and then Ctx.Cursors (F_Server_Arity).First = (Ctx.Cursors (F_Server_Kind).Last + 1)
                           and then (if Structural_Valid (Ctx.Cursors (F_Server_Tag))
                                and then (Types.Bit_Length (Ctx.Cursors (F_Server_Arity).Value.Server_Arity_Value) = Types.Bit_Length (Convert (BA_SINGLE))
                                  or (Types.Bit_Length (Ctx.Cursors (F_Server_Kind).Value.Server_Kind_Value) = Types.Bit_Length (Convert (BK_FD))
                                    and Types.Bit_Length (Ctx.Cursors (F_Server_Arity).Value.Server_Arity_Value) = Types.Bit_Length (Convert (BA_ARRAY)))) then
                              (Ctx.Cursors (F_Server_Tag).Last - Ctx.Cursors (F_Server_Tag).First + 1) = Binder.Binder_Tag_Base'Size
                                and then Ctx.Cursors (F_Server_Tag).Predecessor = F_Server_Arity
                                and then Ctx.Cursors (F_Server_Tag).First = (Ctx.Cursors (F_Server_Arity).Last + 1)
                                and then (if Structural_Valid (Ctx.Cursors (F_Server_Legacy_Flags))
                                     and then Types.Bit_Length (Ctx.Cursors (F_Server_Kind).Value.Server_Kind_Value) = Types.Bit_Length (Convert (BK_FD)) then
                                   (Ctx.Cursors (F_Server_Legacy_Flags).Last - Ctx.Cursors (F_Server_Legacy_Flags).First + 1) = Binder.MBZ32_Base'Size
                                     and then Ctx.Cursors (F_Server_Legacy_Flags).Predecessor = F_Server_Tag
                                     and then Ctx.Cursors (F_Server_Legacy_Flags).First = (Ctx.Cursors (F_Server_Tag).Last + 1)
                                     and then (if Structural_Valid (Ctx.Cursors (F_Server_FD))
                                          and then Types.Bit_Length (Ctx.Cursors (F_Server_Arity).Value.Server_Arity_Value) = Types.Bit_Length (Convert (BA_SINGLE)) then
                                        (Ctx.Cursors (F_Server_FD).Last - Ctx.Cursors (F_Server_FD).First + 1) = Binder.Handle_Base'Size
                                          and then Ctx.Cursors (F_Server_FD).Predecessor = F_Server_Legacy_Flags
                                          and then Ctx.Cursors (F_Server_FD).First = (Ctx.Cursors (F_Server_Legacy_Flags).Last + 1)
                                          and then (if Structural_Valid (Ctx.Cursors (F_Server_Cookie)) then
                                             (Ctx.Cursors (F_Server_Cookie).Last - Ctx.Cursors (F_Server_Cookie).First + 1) = Binder.Cookie'Size
                                               and then Ctx.Cursors (F_Server_Cookie).Predecessor = F_Server_FD
                                               and then Ctx.Cursors (F_Server_Cookie).First = (Ctx.Cursors (F_Server_FD).Last + 1)
                                               and then (if Structural_Valid (Ctx.Cursors (F_Padding)) then
                                                  (Ctx.Cursors (F_Padding).Last - Ctx.Cursors (F_Padding).First + 1) = Service_Manager.MBZ_7_Base'Size
                                                    and then Ctx.Cursors (F_Padding).Predecessor = F_Server_Cookie
                                                    and then Ctx.Cursors (F_Padding).First = (Ctx.Cursors (F_Server_Cookie).Last + 1)
                                                    and then (if Structural_Valid (Ctx.Cursors (F_Allow_Isolated)) then
                                                       (Ctx.Cursors (F_Allow_Isolated).Last - Ctx.Cursors (F_Allow_Isolated).First + 1) = Builtin_Types.Boolean_Base'Size
                                                         and then Ctx.Cursors (F_Allow_Isolated).Predecessor = F_Padding
                                                         and then Ctx.Cursors (F_Allow_Isolated).First = (Ctx.Cursors (F_Padding).Last + 1)
                                                         and then (if Structural_Valid (Ctx.Cursors (F_Dump_Flags)) then
                                                            (Ctx.Cursors (F_Dump_Flags).Last - Ctx.Cursors (F_Dump_Flags).First + 1) = Service_Manager.Integer_Base'Size
                                                              and then Ctx.Cursors (F_Dump_Flags).Predecessor = F_Allow_Isolated
                                                              and then Ctx.Cursors (F_Dump_Flags).First = (Ctx.Cursors (F_Allow_Isolated).Last + 1))))))
                                     and then (if Structural_Valid (Ctx.Cursors (F_Server_Num_FDs))
                                          and then Types.Bit_Length (Ctx.Cursors (F_Server_Arity).Value.Server_Arity_Value) = Types.Bit_Length (Convert (BA_ARRAY)) then
                                        (Ctx.Cursors (F_Server_Num_FDs).Last - Ctx.Cursors (F_Server_Num_FDs).First + 1) = Binder.Count'Size
                                          and then Ctx.Cursors (F_Server_Num_FDs).Predecessor = F_Server_Legacy_Flags
                                          and then Ctx.Cursors (F_Server_Num_FDs).First = (Ctx.Cursors (F_Server_Legacy_Flags).Last + 1)
                                          and then (if Structural_Valid (Ctx.Cursors (F_Server_Parent)) then
                                             (Ctx.Cursors (F_Server_Parent).Last - Ctx.Cursors (F_Server_Parent).First + 1) = Binder.Index'Size
                                               and then Ctx.Cursors (F_Server_Parent).Predecessor = F_Server_Num_FDs
                                               and then Ctx.Cursors (F_Server_Parent).First = (Ctx.Cursors (F_Server_Num_FDs).Last + 1)
                                               and then (if Structural_Valid (Ctx.Cursors (F_Server_Parent_Offset)) then
                                                  (Ctx.Cursors (F_Server_Parent_Offset).Last - Ctx.Cursors (F_Server_Parent_Offset).First + 1) = Binder.Offset'Size
                                                    and then Ctx.Cursors (F_Server_Parent_Offset).Predecessor = F_Server_Parent
                                                    and then Ctx.Cursors (F_Server_Parent_Offset).First = (Ctx.Cursors (F_Server_Parent).Last + 1)
                                                    and then (if Structural_Valid (Ctx.Cursors (F_Padding)) then
                                                       (Ctx.Cursors (F_Padding).Last - Ctx.Cursors (F_Padding).First + 1) = Service_Manager.MBZ_7_Base'Size
                                                         and then Ctx.Cursors (F_Padding).Predecessor = F_Server_Parent_Offset
                                                         and then Ctx.Cursors (F_Padding).First = (Ctx.Cursors (F_Server_Parent_Offset).Last + 1)
                                                         and then (if Structural_Valid (Ctx.Cursors (F_Allow_Isolated)) then
                                                            (Ctx.Cursors (F_Allow_Isolated).Last - Ctx.Cursors (F_Allow_Isolated).First + 1) = Builtin_Types.Boolean_Base'Size
                                                              and then Ctx.Cursors (F_Allow_Isolated).Predecessor = F_Padding
                                                              and then Ctx.Cursors (F_Allow_Isolated).First = (Ctx.Cursors (F_Padding).Last + 1)
                                                              and then (if Structural_Valid (Ctx.Cursors (F_Dump_Flags)) then
                                                                 (Ctx.Cursors (F_Dump_Flags).Last - Ctx.Cursors (F_Dump_Flags).First + 1) = Service_Manager.Integer_Base'Size
                                                                   and then Ctx.Cursors (F_Dump_Flags).Predecessor = F_Allow_Isolated
                                                                   and then Ctx.Cursors (F_Dump_Flags).First = (Ctx.Cursors (F_Allow_Isolated).Last + 1))))))))
                                and then (if Structural_Valid (Ctx.Cursors (F_Server_Has_Parent))
                                     and then Types.Bit_Length (Ctx.Cursors (F_Server_Kind).Value.Server_Kind_Value) = Types.Bit_Length (Convert (BK_POINTER)) then
                                   (Ctx.Cursors (F_Server_Has_Parent).Last - Ctx.Cursors (F_Server_Has_Parent).First + 1) = Builtin_Types.Boolean_Base'Size
                                     and then Ctx.Cursors (F_Server_Has_Parent).Predecessor = F_Server_Tag
                                     and then Ctx.Cursors (F_Server_Has_Parent).First = (Ctx.Cursors (F_Server_Tag).Last + 1)
                                     and then (if Structural_Valid (Ctx.Cursors (F_Server_Padding)) then
                                        (Ctx.Cursors (F_Server_Padding).Last - Ctx.Cursors (F_Server_Padding).First + 1) = Binder.MBZ31_Base'Size
                                          and then Ctx.Cursors (F_Server_Padding).Predecessor = F_Server_Has_Parent
                                          and then Ctx.Cursors (F_Server_Padding).First = (Ctx.Cursors (F_Server_Has_Parent).Last + 1)
                                          and then (if Structural_Valid (Ctx.Cursors (F_Server_Buffer)) then
                                             (Ctx.Cursors (F_Server_Buffer).Last - Ctx.Cursors (F_Server_Buffer).First + 1) = Binder.Index'Size
                                               and then Ctx.Cursors (F_Server_Buffer).Predecessor = F_Server_Padding
                                               and then Ctx.Cursors (F_Server_Buffer).First = (Ctx.Cursors (F_Server_Padding).Last + 1)
                                               and then (if Structural_Valid (Ctx.Cursors (F_Server_Length)) then
                                                  (Ctx.Cursors (F_Server_Length).Last - Ctx.Cursors (F_Server_Length).First + 1) = Binder.Length_Base'Size
                                                    and then Ctx.Cursors (F_Server_Length).Predecessor = F_Server_Buffer
                                                    and then Ctx.Cursors (F_Server_Length).First = (Ctx.Cursors (F_Server_Buffer).Last + 1)
                                                    and then (if Structural_Valid (Ctx.Cursors (F_Padding))
                                                         and then Types.Bit_Length (Ctx.Cursors (F_Server_Has_Parent).Value.Server_Has_Parent_Value) = Types.Bit_Length (Convert (False)) then
                                                       (Ctx.Cursors (F_Padding).Last - Ctx.Cursors (F_Padding).First + 1) = Service_Manager.MBZ_7_Base'Size
                                                         and then Ctx.Cursors (F_Padding).Predecessor = F_Server_Length
                                                         and then Ctx.Cursors (F_Padding).First = (Ctx.Cursors (F_Server_Length).Last + 1)
                                                         and then (if Structural_Valid (Ctx.Cursors (F_Allow_Isolated)) then
                                                            (Ctx.Cursors (F_Allow_Isolated).Last - Ctx.Cursors (F_Allow_Isolated).First + 1) = Builtin_Types.Boolean_Base'Size
                                                              and then Ctx.Cursors (F_Allow_Isolated).Predecessor = F_Padding
                                                              and then Ctx.Cursors (F_Allow_Isolated).First = (Ctx.Cursors (F_Padding).Last + 1)
                                                              and then (if Structural_Valid (Ctx.Cursors (F_Dump_Flags)) then
                                                                 (Ctx.Cursors (F_Dump_Flags).Last - Ctx.Cursors (F_Dump_Flags).First + 1) = Service_Manager.Integer_Base'Size
                                                                   and then Ctx.Cursors (F_Dump_Flags).Predecessor = F_Allow_Isolated
                                                                   and then Ctx.Cursors (F_Dump_Flags).First = (Ctx.Cursors (F_Allow_Isolated).Last + 1))))
                                                    and then (if Structural_Valid (Ctx.Cursors (F_Server_Index))
                                                         and then Types.Bit_Length (Ctx.Cursors (F_Server_Has_Parent).Value.Server_Has_Parent_Value) = Types.Bit_Length (Convert (True)) then
                                                       (Ctx.Cursors (F_Server_Index).Last - Ctx.Cursors (F_Server_Index).First + 1) = Binder.Index'Size
                                                         and then Ctx.Cursors (F_Server_Index).Predecessor = F_Server_Length
                                                         and then Ctx.Cursors (F_Server_Index).First = (Ctx.Cursors (F_Server_Length).Last + 1)
                                                         and then (if Structural_Valid (Ctx.Cursors (F_Padding)) then
                                                            (Ctx.Cursors (F_Padding).Last - Ctx.Cursors (F_Padding).First + 1) = Service_Manager.MBZ_7_Base'Size
                                                              and then Ctx.Cursors (F_Padding).Predecessor = F_Server_Index
                                                              and then Ctx.Cursors (F_Padding).First = (Ctx.Cursors (F_Server_Index).Last + 1)
                                                              and then (if Structural_Valid (Ctx.Cursors (F_Allow_Isolated)) then
                                                                 (Ctx.Cursors (F_Allow_Isolated).Last - Ctx.Cursors (F_Allow_Isolated).First + 1) = Builtin_Types.Boolean_Base'Size
                                                                   and then Ctx.Cursors (F_Allow_Isolated).Predecessor = F_Padding
                                                                   and then Ctx.Cursors (F_Allow_Isolated).First = (Ctx.Cursors (F_Padding).Last + 1)
                                                                   and then (if Structural_Valid (Ctx.Cursors (F_Dump_Flags)) then
                                                                      (Ctx.Cursors (F_Dump_Flags).Last - Ctx.Cursors (F_Dump_Flags).First + 1) = Service_Manager.Integer_Base'Size
                                                                        and then Ctx.Cursors (F_Dump_Flags).Predecessor = F_Allow_Isolated
                                                                        and then Ctx.Cursors (F_Dump_Flags).First = (Ctx.Cursors (F_Allow_Isolated).Last + 1)))))))))
                                and then (if Structural_Valid (Ctx.Cursors (F_Server_Flags))
                                     and then (Types.Bit_Length (Ctx.Cursors (F_Server_Kind).Value.Server_Kind_Value) /= Types.Bit_Length (Convert (BK_POINTER))
                                       and Types.Bit_Length (Ctx.Cursors (F_Server_Kind).Value.Server_Kind_Value) /= Types.Bit_Length (Convert (BK_FD))) then
                                   (Ctx.Cursors (F_Server_Flags).Last - Ctx.Cursors (F_Server_Flags).First + 1) = Binder.Flat_Binder_Flags_Base'Size
                                     and then Ctx.Cursors (F_Server_Flags).Predecessor = F_Server_Tag
                                     and then Ctx.Cursors (F_Server_Flags).First = (Ctx.Cursors (F_Server_Tag).Last + 1)
                                     and then (if Structural_Valid (Ctx.Cursors (F_Server_Binder))
                                          and then (Types.Bit_Length (Ctx.Cursors (F_Server_Kind).Value.Server_Kind_Value) = Types.Bit_Length (Convert (BK_STRONG_BINDER))
                                            or Types.Bit_Length (Ctx.Cursors (F_Server_Kind).Value.Server_Kind_Value) = Types.Bit_Length (Convert (BK_WEAK_BINDER))) then
                                        (Ctx.Cursors (F_Server_Binder).Last - Ctx.Cursors (F_Server_Binder).First + 1) = Binder.Value'Size
                                          and then Ctx.Cursors (F_Server_Binder).Predecessor = F_Server_Flags
                                          and then Ctx.Cursors (F_Server_Binder).First = (Ctx.Cursors (F_Server_Flags).Last + 1)
                                          and then (if Structural_Valid (Ctx.Cursors (F_Server_Cookie)) then
                                             (Ctx.Cursors (F_Server_Cookie).Last - Ctx.Cursors (F_Server_Cookie).First + 1) = Binder.Cookie'Size
                                               and then Ctx.Cursors (F_Server_Cookie).Predecessor = F_Server_Binder
                                               and then Ctx.Cursors (F_Server_Cookie).First = (Ctx.Cursors (F_Server_Binder).Last + 1)
                                               and then (if Structural_Valid (Ctx.Cursors (F_Padding)) then
                                                  (Ctx.Cursors (F_Padding).Last - Ctx.Cursors (F_Padding).First + 1) = Service_Manager.MBZ_7_Base'Size
                                                    and then Ctx.Cursors (F_Padding).Predecessor = F_Server_Cookie
                                                    and then Ctx.Cursors (F_Padding).First = (Ctx.Cursors (F_Server_Cookie).Last + 1)
                                                    and then (if Structural_Valid (Ctx.Cursors (F_Allow_Isolated)) then
                                                       (Ctx.Cursors (F_Allow_Isolated).Last - Ctx.Cursors (F_Allow_Isolated).First + 1) = Builtin_Types.Boolean_Base'Size
                                                         and then Ctx.Cursors (F_Allow_Isolated).Predecessor = F_Padding
                                                         and then Ctx.Cursors (F_Allow_Isolated).First = (Ctx.Cursors (F_Padding).Last + 1)
                                                         and then (if Structural_Valid (Ctx.Cursors (F_Dump_Flags)) then
                                                            (Ctx.Cursors (F_Dump_Flags).Last - Ctx.Cursors (F_Dump_Flags).First + 1) = Service_Manager.Integer_Base'Size
                                                              and then Ctx.Cursors (F_Dump_Flags).Predecessor = F_Allow_Isolated
                                                              and then Ctx.Cursors (F_Dump_Flags).First = (Ctx.Cursors (F_Allow_Isolated).Last + 1))))))
                                     and then (if Structural_Valid (Ctx.Cursors (F_Server_Handle))
                                          and then (Types.Bit_Length (Ctx.Cursors (F_Server_Kind).Value.Server_Kind_Value) = Types.Bit_Length (Convert (BK_STRONG_HANDLE))
                                            or Types.Bit_Length (Ctx.Cursors (F_Server_Kind).Value.Server_Kind_Value) = Types.Bit_Length (Convert (BK_WEAK_HANDLE))) then
                                        (Ctx.Cursors (F_Server_Handle).Last - Ctx.Cursors (F_Server_Handle).First + 1) = Binder.Handle_Base'Size
                                          and then Ctx.Cursors (F_Server_Handle).Predecessor = F_Server_Flags
                                          and then Ctx.Cursors (F_Server_Handle).First = (Ctx.Cursors (F_Server_Flags).Last + 1)
                                          and then (if Structural_Valid (Ctx.Cursors (F_Server_Unused_Padding)) then
                                             (Ctx.Cursors (F_Server_Unused_Padding).Last - Ctx.Cursors (F_Server_Unused_Padding).First + 1) = Binder.MBZ32_Base'Size
                                               and then Ctx.Cursors (F_Server_Unused_Padding).Predecessor = F_Server_Handle
                                               and then Ctx.Cursors (F_Server_Unused_Padding).First = (Ctx.Cursors (F_Server_Handle).Last + 1)
                                               and then (if Structural_Valid (Ctx.Cursors (F_Server_Cookie)) then
                                                  (Ctx.Cursors (F_Server_Cookie).Last - Ctx.Cursors (F_Server_Cookie).First + 1) = Binder.Cookie'Size
                                                    and then Ctx.Cursors (F_Server_Cookie).Predecessor = F_Server_Unused_Padding
                                                    and then Ctx.Cursors (F_Server_Cookie).First = (Ctx.Cursors (F_Server_Unused_Padding).Last + 1)
                                                    and then (if Structural_Valid (Ctx.Cursors (F_Padding)) then
                                                       (Ctx.Cursors (F_Padding).Last - Ctx.Cursors (F_Padding).First + 1) = Service_Manager.MBZ_7_Base'Size
                                                         and then Ctx.Cursors (F_Padding).Predecessor = F_Server_Cookie
                                                         and then Ctx.Cursors (F_Padding).First = (Ctx.Cursors (F_Server_Cookie).Last + 1)
                                                         and then (if Structural_Valid (Ctx.Cursors (F_Allow_Isolated)) then
                                                            (Ctx.Cursors (F_Allow_Isolated).Last - Ctx.Cursors (F_Allow_Isolated).First + 1) = Builtin_Types.Boolean_Base'Size
                                                              and then Ctx.Cursors (F_Allow_Isolated).Predecessor = F_Padding
                                                              and then Ctx.Cursors (F_Allow_Isolated).First = (Ctx.Cursors (F_Padding).Last + 1)
                                                              and then (if Structural_Valid (Ctx.Cursors (F_Dump_Flags)) then
                                                                 (Ctx.Cursors (F_Dump_Flags).Last - Ctx.Cursors (F_Dump_Flags).First + 1) = Service_Manager.Integer_Base'Size
                                                                   and then Ctx.Cursors (F_Dump_Flags).Predecessor = F_Allow_Isolated
                                                                   and then Ctx.Cursors (F_Dump_Flags).First = (Ctx.Cursors (F_Allow_Isolated).Last + 1))))))))))))));
      Ctx.Cursors (F_Name) := (State => S_Structural_Valid, First => First, Last => Last, Value => (Fld => F_Name), Predecessor => Ctx.Cursors (F_Name).Predecessor);
      Ctx.Cursors (Successor (Ctx, F_Name)) := (State => S_Invalid, Predecessor => F_Name);
   end Initialize_Name;

end Parpen.Service_Manager.Generic_Request_Add_Service;
