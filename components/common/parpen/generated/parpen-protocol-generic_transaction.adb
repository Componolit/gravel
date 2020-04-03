package body Parpen.Protocol.Generic_Transaction with
  SPARK_Mode
is

   function Create return Context is
     ((Types.Index'First, Types.Index'First, Types.Bit_Index'First, Types.Bit_Index'First, null, (F_Tag => (State => S_Invalid, Predecessor => F_Initial), others => (State => S_Invalid, Predecessor => F_Final))));

   procedure Initialize (Ctx : out Context; Buffer : in out Types.Bytes_Ptr) is
   begin
      Initialize (Ctx, Buffer, Types.First_Bit_Index (Buffer'First), Types.Last_Bit_Index (Buffer'Last));
   end Initialize;

   procedure Initialize (Ctx : out Context; Buffer : in out Types.Bytes_Ptr; First, Last : Types.Bit_Index) is
      Buffer_First : constant Types.Index := Buffer'First;
      Buffer_Last : constant Types.Index := Buffer'Last;
   begin
      Ctx := (Buffer_First, Buffer_Last, First, Last, Buffer, (F_Tag => (State => S_Invalid, Predecessor => F_Initial), others => (State => S_Invalid, Predecessor => F_Final)));
      Buffer := null;
   end Initialize;

   function Initialized (Ctx : Context) return Boolean is
     (Valid_Next (Ctx, F_Tag)
      and then Available_Space (Ctx, F_Tag) = (Types.Last_Bit_Index (Ctx.Buffer_Last) - Ctx.First + 1)
      and then Invalid (Ctx, F_Tag)
      and then Invalid (Ctx, F_Handle)
      and then Invalid (Ctx, F_Binder)
      and then Invalid (Ctx, F_Code)
      and then Invalid (Ctx, F_Method)
      and then Invalid (Ctx, F_Cookie)
      and then Invalid (Ctx, F_Send_Offset)
      and then Invalid (Ctx, F_Send_Length)
      and then Invalid (Ctx, F_Meta_Offset)
      and then Invalid (Ctx, F_Meta_Length)
      and then Invalid (Ctx, F_Recv_Offset)
      and then Invalid (Ctx, F_Recv_Length));

   procedure Take_Buffer (Ctx : in out Context; Buffer : out Types.Bytes_Ptr) is
   begin
      Buffer := Ctx.Buffer;
      Ctx.Buffer := null;
   end Take_Buffer;

   function Has_Buffer (Ctx : Context) return Boolean is
     (Ctx.Buffer /= null);

   function Message_Last (Ctx : Context) return Types.Bit_Index is
     ((if Structural_Valid (Ctx.Cursors (F_Code)) then
       Ctx.Cursors (F_Code).Last
    elsif Structural_Valid (Ctx.Cursors (F_Recv_Length)) then
       Ctx.Cursors (F_Recv_Length).Last
    else
       Types.Unreachable_Bit_Length));

   function Path_Condition (Ctx : Context; Fld : Field) return Boolean is
     ((case Ctx.Cursors (Fld).Predecessor is
         when F_Initial =>
            (case Fld is
                  when F_Tag =>
                     True,
                  when others =>
                     False),
         when F_Tag =>
            (case Fld is
                  when F_Handle =>
                     Types.Bit_Length (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.Bit_Length (Convert (T_HANDLE)),
                  when F_Binder =>
                     Types.Bit_Length (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.Bit_Length (Convert (T_BINDER)),
                  when F_Code =>
                     Types.Bit_Length (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.Bit_Length (Convert (T_STATUS)),
                  when others =>
                     False),
         when F_Handle | F_Binder =>
            (case Fld is
                  when F_Method =>
                     True,
                  when others =>
                     False),
         when F_Code =>
            False,
         when F_Method =>
            (case Fld is
                  when F_Cookie =>
                     True,
                  when others =>
                     False),
         when F_Cookie =>
            (case Fld is
                  when F_Send_Offset =>
                     True,
                  when others =>
                     False),
         when F_Send_Offset =>
            (case Fld is
                  when F_Send_Length =>
                     True,
                  when others =>
                     False),
         when F_Send_Length =>
            (case Fld is
                  when F_Meta_Offset =>
                     True,
                  when others =>
                     False),
         when F_Meta_Offset =>
            (case Fld is
                  when F_Meta_Length =>
                     True,
                  when others =>
                     False),
         when F_Meta_Length =>
            (case Fld is
                  when F_Recv_Offset =>
                     True,
                  when others =>
                     False),
         when F_Recv_Offset =>
            (case Fld is
                  when F_Recv_Length =>
                     True,
                  when others =>
                     False),
         when F_Recv_Length | F_Final =>
            False));

   function Field_Condition (Ctx : Context; Val : Field_Dependent_Value) return Boolean is
     ((case Val.Fld is
         when F_Initial =>
            True,
         when F_Tag =>
            Types.Bit_Length (Val.Tag_Value) = Types.Bit_Length (Convert (T_HANDLE))
               or Types.Bit_Length (Val.Tag_Value) = Types.Bit_Length (Convert (T_BINDER))
               or Types.Bit_Length (Val.Tag_Value) = Types.Bit_Length (Convert (T_STATUS)),
         when F_Handle | F_Binder | F_Code | F_Method | F_Cookie | F_Send_Offset | F_Send_Length | F_Meta_Offset | F_Meta_Length | F_Recv_Offset | F_Recv_Length =>
            True,
         when F_Final =>
            False));

   function Field_Length (Ctx : Context; Fld : Field) return Types.Bit_Length is
     ((case Ctx.Cursors (Fld).Predecessor is
         when F_Initial =>
            (case Fld is
                  when F_Tag =>
                     Parpen.Protocol.Tag_Base'Size,
                  when others =>
                     Types.Unreachable_Bit_Length),
         when F_Tag =>
            (case Fld is
                  when F_Handle =>
                     Parpen.Protocol.Handle_Base'Size,
                  when F_Binder =>
                     Parpen.Protocol.Binder'Size,
                  when F_Code =>
                     Parpen.Protocol.Status_Base'Size,
                  when others =>
                     Types.Unreachable_Bit_Length),
         when F_Handle | F_Binder =>
            (case Fld is
                  when F_Method =>
                     Parpen.Protocol.Method_Base'Size,
                  when others =>
                     Types.Unreachable_Bit_Length),
         when F_Code =>
            0,
         when F_Method =>
            (case Fld is
                  when F_Cookie =>
                     Parpen.Protocol.Cookie'Size,
                  when others =>
                     Types.Unreachable_Bit_Length),
         when F_Cookie =>
            (case Fld is
                  when F_Send_Offset =>
                     Parpen.Protocol.Offset'Size,
                  when others =>
                     Types.Unreachable_Bit_Length),
         when F_Send_Offset =>
            (case Fld is
                  when F_Send_Length =>
                     Parpen.Protocol.Length_Base'Size,
                  when others =>
                     Types.Unreachable_Bit_Length),
         when F_Send_Length =>
            (case Fld is
                  when F_Meta_Offset =>
                     Parpen.Protocol.Offset'Size,
                  when others =>
                     Types.Unreachable_Bit_Length),
         when F_Meta_Offset =>
            (case Fld is
                  when F_Meta_Length =>
                     Parpen.Protocol.Length_Base'Size,
                  when others =>
                     Types.Unreachable_Bit_Length),
         when F_Meta_Length =>
            (case Fld is
                  when F_Recv_Offset =>
                     Parpen.Protocol.Offset'Size,
                  when others =>
                     Types.Unreachable_Bit_Length),
         when F_Recv_Offset =>
            (case Fld is
                  when F_Recv_Length =>
                     Parpen.Protocol.Length_Base'Size,
                  when others =>
                     Types.Unreachable_Bit_Length),
         when F_Recv_Length | F_Final =>
            0));

   function Field_First (Ctx : Context; Fld : Field) return Types.Bit_Index is
     ((case Fld is
         when F_Tag =>
            Ctx.First,
         when F_Handle =>
            (if Ctx.Cursors (Fld).Predecessor = F_Tag
                  and Types.Bit_Length (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.Bit_Length (Convert (T_HANDLE)) then
                (Ctx.Cursors (Ctx.Cursors (Fld).Predecessor).Last + 1)
             else
                Types.Unreachable_Bit_Length),
         when F_Binder =>
            (if Ctx.Cursors (Fld).Predecessor = F_Tag
                  and Types.Bit_Length (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.Bit_Length (Convert (T_BINDER)) then
                (Ctx.Cursors (Ctx.Cursors (Fld).Predecessor).Last + 1)
             else
                Types.Unreachable_Bit_Length),
         when F_Code =>
            (if Ctx.Cursors (Fld).Predecessor = F_Tag
                  and Types.Bit_Length (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.Bit_Length (Convert (T_STATUS)) then
                (Ctx.Cursors (Ctx.Cursors (Fld).Predecessor).Last + 1)
             else
                Types.Unreachable_Bit_Length),
         when F_Method =>
            (if Ctx.Cursors (Fld).Predecessor = F_Handle then
                (Ctx.Cursors (Ctx.Cursors (Fld).Predecessor).Last + 1)
             elsif Ctx.Cursors (Fld).Predecessor = F_Binder then
                (Ctx.Cursors (Ctx.Cursors (Fld).Predecessor).Last + 1)
             else
                Types.Unreachable_Bit_Length),
         when F_Cookie =>
            (if Ctx.Cursors (Fld).Predecessor = F_Method then
                (Ctx.Cursors (Ctx.Cursors (Fld).Predecessor).Last + 1)
             else
                Types.Unreachable_Bit_Length),
         when F_Send_Offset =>
            (if Ctx.Cursors (Fld).Predecessor = F_Cookie then
                (Ctx.Cursors (Ctx.Cursors (Fld).Predecessor).Last + 1)
             else
                Types.Unreachable_Bit_Length),
         when F_Send_Length =>
            (if Ctx.Cursors (Fld).Predecessor = F_Send_Offset then
                (Ctx.Cursors (Ctx.Cursors (Fld).Predecessor).Last + 1)
             else
                Types.Unreachable_Bit_Length),
         when F_Meta_Offset =>
            (if Ctx.Cursors (Fld).Predecessor = F_Send_Length then
                (Ctx.Cursors (Ctx.Cursors (Fld).Predecessor).Last + 1)
             else
                Types.Unreachable_Bit_Length),
         when F_Meta_Length =>
            (if Ctx.Cursors (Fld).Predecessor = F_Meta_Offset then
                (Ctx.Cursors (Ctx.Cursors (Fld).Predecessor).Last + 1)
             else
                Types.Unreachable_Bit_Length),
         when F_Recv_Offset =>
            (if Ctx.Cursors (Fld).Predecessor = F_Meta_Length then
                (Ctx.Cursors (Ctx.Cursors (Fld).Predecessor).Last + 1)
             else
                Types.Unreachable_Bit_Length),
         when F_Recv_Length =>
            (if Ctx.Cursors (Fld).Predecessor = F_Recv_Offset then
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
         when F_Tag =>
            (if Types.Bit_Length (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.Bit_Length (Convert (T_HANDLE)) then
                F_Handle
             elsif Types.Bit_Length (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.Bit_Length (Convert (T_BINDER)) then
                F_Binder
             elsif Types.Bit_Length (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.Bit_Length (Convert (T_STATUS)) then
                F_Code
             else
                F_Initial),
         when F_Handle | F_Binder =>
            F_Method,
         when F_Code =>
            F_Final,
         when F_Method =>
            F_Cookie,
         when F_Cookie =>
            F_Send_Offset,
         when F_Send_Offset =>
            F_Send_Length,
         when F_Send_Length =>
            F_Meta_Offset,
         when F_Meta_Offset =>
            F_Meta_Length,
         when F_Meta_Length =>
            F_Recv_Offset,
         when F_Recv_Offset =>
            F_Recv_Length,
         when F_Recv_Length =>
            F_Final))
    with
     Pre =>
       Structural_Valid (Ctx, Fld)
          and Valid_Predecessor (Ctx, Fld);

   function Valid_Predecessor (Ctx : Context; Fld : Virtual_Field) return Boolean is
     ((case Fld is
         when F_Initial =>
            True,
         when F_Tag =>
            Ctx.Cursors (Fld).Predecessor = F_Initial,
         when F_Handle | F_Binder | F_Code =>
            (Valid (Ctx.Cursors (F_Tag))
                 and Ctx.Cursors (Fld).Predecessor = F_Tag),
         when F_Method =>
            (Valid (Ctx.Cursors (F_Handle))
                 and Ctx.Cursors (Fld).Predecessor = F_Handle)
               or (Valid (Ctx.Cursors (F_Binder))
                 and Ctx.Cursors (Fld).Predecessor = F_Binder),
         when F_Cookie =>
            (Valid (Ctx.Cursors (F_Method))
                 and Ctx.Cursors (Fld).Predecessor = F_Method),
         when F_Send_Offset =>
            (Valid (Ctx.Cursors (F_Cookie))
                 and Ctx.Cursors (Fld).Predecessor = F_Cookie),
         when F_Send_Length =>
            (Valid (Ctx.Cursors (F_Send_Offset))
                 and Ctx.Cursors (Fld).Predecessor = F_Send_Offset),
         when F_Meta_Offset =>
            (Valid (Ctx.Cursors (F_Send_Length))
                 and Ctx.Cursors (Fld).Predecessor = F_Send_Length),
         when F_Meta_Length =>
            (Valid (Ctx.Cursors (F_Meta_Offset))
                 and Ctx.Cursors (Fld).Predecessor = F_Meta_Offset),
         when F_Recv_Offset =>
            (Valid (Ctx.Cursors (F_Meta_Length))
                 and Ctx.Cursors (Fld).Predecessor = F_Meta_Length),
         when F_Recv_Length =>
            (Valid (Ctx.Cursors (F_Recv_Offset))
                 and Ctx.Cursors (Fld).Predecessor = F_Recv_Offset),
         when F_Final =>
            (Valid (Ctx.Cursors (F_Code))
                 and Ctx.Cursors (Fld).Predecessor = F_Code)
               or (Valid (Ctx.Cursors (F_Recv_Length))
                 and Ctx.Cursors (Fld).Predecessor = F_Recv_Length)));

   function Invalid_Successor (Ctx : Context; Fld : Field) return Boolean is
     ((case Fld is
         when F_Tag =>
            Invalid (Ctx.Cursors (F_Handle))
               and Invalid (Ctx.Cursors (F_Binder))
               and Invalid (Ctx.Cursors (F_Code)),
         when F_Handle | F_Binder =>
            Invalid (Ctx.Cursors (F_Method)),
         when F_Code =>
            True,
         when F_Method =>
            Invalid (Ctx.Cursors (F_Cookie)),
         when F_Cookie =>
            Invalid (Ctx.Cursors (F_Send_Offset)),
         when F_Send_Offset =>
            Invalid (Ctx.Cursors (F_Send_Length)),
         when F_Send_Length =>
            Invalid (Ctx.Cursors (F_Meta_Offset)),
         when F_Meta_Offset =>
            Invalid (Ctx.Cursors (F_Meta_Length)),
         when F_Meta_Length =>
            Invalid (Ctx.Cursors (F_Recv_Offset)),
         when F_Recv_Offset =>
            Invalid (Ctx.Cursors (F_Recv_Length)),
         when F_Recv_Length =>
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
               when F_Tag =>
                  Invalid (Ctx, F_Tag)
                     and Invalid (Ctx, F_Handle)
                     and Invalid (Ctx, F_Binder)
                     and Invalid (Ctx, F_Code)
                     and Invalid (Ctx, F_Method)
                     and Invalid (Ctx, F_Cookie)
                     and Invalid (Ctx, F_Send_Offset)
                     and Invalid (Ctx, F_Send_Length)
                     and Invalid (Ctx, F_Meta_Offset)
                     and Invalid (Ctx, F_Meta_Length)
                     and Invalid (Ctx, F_Recv_Offset)
                     and Invalid (Ctx, F_Recv_Length),
               when F_Handle =>
                  Ctx.Cursors (F_Tag) = Ctx.Cursors (F_Tag)'Old
                     and Invalid (Ctx, F_Handle)
                     and Invalid (Ctx, F_Binder)
                     and Invalid (Ctx, F_Code)
                     and Invalid (Ctx, F_Method)
                     and Invalid (Ctx, F_Cookie)
                     and Invalid (Ctx, F_Send_Offset)
                     and Invalid (Ctx, F_Send_Length)
                     and Invalid (Ctx, F_Meta_Offset)
                     and Invalid (Ctx, F_Meta_Length)
                     and Invalid (Ctx, F_Recv_Offset)
                     and Invalid (Ctx, F_Recv_Length),
               when F_Binder =>
                  Ctx.Cursors (F_Tag) = Ctx.Cursors (F_Tag)'Old
                     and Ctx.Cursors (F_Handle) = Ctx.Cursors (F_Handle)'Old
                     and Invalid (Ctx, F_Binder)
                     and Invalid (Ctx, F_Code)
                     and Invalid (Ctx, F_Method)
                     and Invalid (Ctx, F_Cookie)
                     and Invalid (Ctx, F_Send_Offset)
                     and Invalid (Ctx, F_Send_Length)
                     and Invalid (Ctx, F_Meta_Offset)
                     and Invalid (Ctx, F_Meta_Length)
                     and Invalid (Ctx, F_Recv_Offset)
                     and Invalid (Ctx, F_Recv_Length),
               when F_Code =>
                  Ctx.Cursors (F_Tag) = Ctx.Cursors (F_Tag)'Old
                     and Ctx.Cursors (F_Handle) = Ctx.Cursors (F_Handle)'Old
                     and Ctx.Cursors (F_Binder) = Ctx.Cursors (F_Binder)'Old
                     and Invalid (Ctx, F_Code)
                     and Invalid (Ctx, F_Method)
                     and Invalid (Ctx, F_Cookie)
                     and Invalid (Ctx, F_Send_Offset)
                     and Invalid (Ctx, F_Send_Length)
                     and Invalid (Ctx, F_Meta_Offset)
                     and Invalid (Ctx, F_Meta_Length)
                     and Invalid (Ctx, F_Recv_Offset)
                     and Invalid (Ctx, F_Recv_Length),
               when F_Method =>
                  Ctx.Cursors (F_Tag) = Ctx.Cursors (F_Tag)'Old
                     and Ctx.Cursors (F_Handle) = Ctx.Cursors (F_Handle)'Old
                     and Ctx.Cursors (F_Binder) = Ctx.Cursors (F_Binder)'Old
                     and Ctx.Cursors (F_Code) = Ctx.Cursors (F_Code)'Old
                     and Invalid (Ctx, F_Method)
                     and Invalid (Ctx, F_Cookie)
                     and Invalid (Ctx, F_Send_Offset)
                     and Invalid (Ctx, F_Send_Length)
                     and Invalid (Ctx, F_Meta_Offset)
                     and Invalid (Ctx, F_Meta_Length)
                     and Invalid (Ctx, F_Recv_Offset)
                     and Invalid (Ctx, F_Recv_Length),
               when F_Cookie =>
                  Ctx.Cursors (F_Tag) = Ctx.Cursors (F_Tag)'Old
                     and Ctx.Cursors (F_Handle) = Ctx.Cursors (F_Handle)'Old
                     and Ctx.Cursors (F_Binder) = Ctx.Cursors (F_Binder)'Old
                     and Ctx.Cursors (F_Code) = Ctx.Cursors (F_Code)'Old
                     and Ctx.Cursors (F_Method) = Ctx.Cursors (F_Method)'Old
                     and Invalid (Ctx, F_Cookie)
                     and Invalid (Ctx, F_Send_Offset)
                     and Invalid (Ctx, F_Send_Length)
                     and Invalid (Ctx, F_Meta_Offset)
                     and Invalid (Ctx, F_Meta_Length)
                     and Invalid (Ctx, F_Recv_Offset)
                     and Invalid (Ctx, F_Recv_Length),
               when F_Send_Offset =>
                  Ctx.Cursors (F_Tag) = Ctx.Cursors (F_Tag)'Old
                     and Ctx.Cursors (F_Handle) = Ctx.Cursors (F_Handle)'Old
                     and Ctx.Cursors (F_Binder) = Ctx.Cursors (F_Binder)'Old
                     and Ctx.Cursors (F_Code) = Ctx.Cursors (F_Code)'Old
                     and Ctx.Cursors (F_Method) = Ctx.Cursors (F_Method)'Old
                     and Ctx.Cursors (F_Cookie) = Ctx.Cursors (F_Cookie)'Old
                     and Invalid (Ctx, F_Send_Offset)
                     and Invalid (Ctx, F_Send_Length)
                     and Invalid (Ctx, F_Meta_Offset)
                     and Invalid (Ctx, F_Meta_Length)
                     and Invalid (Ctx, F_Recv_Offset)
                     and Invalid (Ctx, F_Recv_Length),
               when F_Send_Length =>
                  Ctx.Cursors (F_Tag) = Ctx.Cursors (F_Tag)'Old
                     and Ctx.Cursors (F_Handle) = Ctx.Cursors (F_Handle)'Old
                     and Ctx.Cursors (F_Binder) = Ctx.Cursors (F_Binder)'Old
                     and Ctx.Cursors (F_Code) = Ctx.Cursors (F_Code)'Old
                     and Ctx.Cursors (F_Method) = Ctx.Cursors (F_Method)'Old
                     and Ctx.Cursors (F_Cookie) = Ctx.Cursors (F_Cookie)'Old
                     and Ctx.Cursors (F_Send_Offset) = Ctx.Cursors (F_Send_Offset)'Old
                     and Invalid (Ctx, F_Send_Length)
                     and Invalid (Ctx, F_Meta_Offset)
                     and Invalid (Ctx, F_Meta_Length)
                     and Invalid (Ctx, F_Recv_Offset)
                     and Invalid (Ctx, F_Recv_Length),
               when F_Meta_Offset =>
                  Ctx.Cursors (F_Tag) = Ctx.Cursors (F_Tag)'Old
                     and Ctx.Cursors (F_Handle) = Ctx.Cursors (F_Handle)'Old
                     and Ctx.Cursors (F_Binder) = Ctx.Cursors (F_Binder)'Old
                     and Ctx.Cursors (F_Code) = Ctx.Cursors (F_Code)'Old
                     and Ctx.Cursors (F_Method) = Ctx.Cursors (F_Method)'Old
                     and Ctx.Cursors (F_Cookie) = Ctx.Cursors (F_Cookie)'Old
                     and Ctx.Cursors (F_Send_Offset) = Ctx.Cursors (F_Send_Offset)'Old
                     and Ctx.Cursors (F_Send_Length) = Ctx.Cursors (F_Send_Length)'Old
                     and Invalid (Ctx, F_Meta_Offset)
                     and Invalid (Ctx, F_Meta_Length)
                     and Invalid (Ctx, F_Recv_Offset)
                     and Invalid (Ctx, F_Recv_Length),
               when F_Meta_Length =>
                  Ctx.Cursors (F_Tag) = Ctx.Cursors (F_Tag)'Old
                     and Ctx.Cursors (F_Handle) = Ctx.Cursors (F_Handle)'Old
                     and Ctx.Cursors (F_Binder) = Ctx.Cursors (F_Binder)'Old
                     and Ctx.Cursors (F_Code) = Ctx.Cursors (F_Code)'Old
                     and Ctx.Cursors (F_Method) = Ctx.Cursors (F_Method)'Old
                     and Ctx.Cursors (F_Cookie) = Ctx.Cursors (F_Cookie)'Old
                     and Ctx.Cursors (F_Send_Offset) = Ctx.Cursors (F_Send_Offset)'Old
                     and Ctx.Cursors (F_Send_Length) = Ctx.Cursors (F_Send_Length)'Old
                     and Ctx.Cursors (F_Meta_Offset) = Ctx.Cursors (F_Meta_Offset)'Old
                     and Invalid (Ctx, F_Meta_Length)
                     and Invalid (Ctx, F_Recv_Offset)
                     and Invalid (Ctx, F_Recv_Length),
               when F_Recv_Offset =>
                  Ctx.Cursors (F_Tag) = Ctx.Cursors (F_Tag)'Old
                     and Ctx.Cursors (F_Handle) = Ctx.Cursors (F_Handle)'Old
                     and Ctx.Cursors (F_Binder) = Ctx.Cursors (F_Binder)'Old
                     and Ctx.Cursors (F_Code) = Ctx.Cursors (F_Code)'Old
                     and Ctx.Cursors (F_Method) = Ctx.Cursors (F_Method)'Old
                     and Ctx.Cursors (F_Cookie) = Ctx.Cursors (F_Cookie)'Old
                     and Ctx.Cursors (F_Send_Offset) = Ctx.Cursors (F_Send_Offset)'Old
                     and Ctx.Cursors (F_Send_Length) = Ctx.Cursors (F_Send_Length)'Old
                     and Ctx.Cursors (F_Meta_Offset) = Ctx.Cursors (F_Meta_Offset)'Old
                     and Ctx.Cursors (F_Meta_Length) = Ctx.Cursors (F_Meta_Length)'Old
                     and Invalid (Ctx, F_Recv_Offset)
                     and Invalid (Ctx, F_Recv_Length),
               when F_Recv_Length =>
                  Ctx.Cursors (F_Tag) = Ctx.Cursors (F_Tag)'Old
                     and Ctx.Cursors (F_Handle) = Ctx.Cursors (F_Handle)'Old
                     and Ctx.Cursors (F_Binder) = Ctx.Cursors (F_Binder)'Old
                     and Ctx.Cursors (F_Code) = Ctx.Cursors (F_Code)'Old
                     and Ctx.Cursors (F_Method) = Ctx.Cursors (F_Method)'Old
                     and Ctx.Cursors (F_Cookie) = Ctx.Cursors (F_Cookie)'Old
                     and Ctx.Cursors (F_Send_Offset) = Ctx.Cursors (F_Send_Offset)'Old
                     and Ctx.Cursors (F_Send_Length) = Ctx.Cursors (F_Send_Length)'Old
                     and Ctx.Cursors (F_Meta_Offset) = Ctx.Cursors (F_Meta_Offset)'Old
                     and Ctx.Cursors (F_Meta_Length) = Ctx.Cursors (F_Meta_Length)'Old
                     and Ctx.Cursors (F_Recv_Offset) = Ctx.Cursors (F_Recv_Offset)'Old
                     and Invalid (Ctx, F_Recv_Length))
   is
      First : constant Types.Bit_Length := Field_First (Ctx, Fld) with
        Ghost;
      Length : constant Types.Bit_Length := Field_Length (Ctx, Fld) with
        Ghost;
   begin
      pragma Assert (Field_First (Ctx, Fld) = First
         and Field_Length (Ctx, Fld) = Length);
      case Fld is
         when F_Tag =>
            Ctx.Cursors (F_Recv_Length) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Recv_Offset) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Meta_Length) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Meta_Offset) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Send_Length) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Send_Offset) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Cookie) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Method) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Code) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Binder) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Handle) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Tag) := (S_Invalid, Ctx.Cursors (F_Tag).Predecessor);
            pragma Assert (Field_First (Ctx, Fld) = First
               and Field_Length (Ctx, Fld) = Length);
         when F_Handle =>
            Ctx.Cursors (F_Recv_Length) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Recv_Offset) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Meta_Length) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Meta_Offset) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Send_Length) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Send_Offset) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Cookie) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Method) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Code) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Binder) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Handle) := (S_Invalid, Ctx.Cursors (F_Handle).Predecessor);
            pragma Assert (Field_First (Ctx, Fld) = First
               and Field_Length (Ctx, Fld) = Length);
         when F_Binder =>
            Ctx.Cursors (F_Recv_Length) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Recv_Offset) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Meta_Length) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Meta_Offset) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Send_Length) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Send_Offset) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Cookie) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Method) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Code) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Binder) := (S_Invalid, Ctx.Cursors (F_Binder).Predecessor);
            pragma Assert (Field_First (Ctx, Fld) = First
               and Field_Length (Ctx, Fld) = Length);
         when F_Code =>
            Ctx.Cursors (F_Recv_Length) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Recv_Offset) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Meta_Length) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Meta_Offset) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Send_Length) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Send_Offset) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Cookie) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Method) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Code) := (S_Invalid, Ctx.Cursors (F_Code).Predecessor);
            pragma Assert (Field_First (Ctx, Fld) = First
               and Field_Length (Ctx, Fld) = Length);
         when F_Method =>
            Ctx.Cursors (F_Recv_Length) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Recv_Offset) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Meta_Length) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Meta_Offset) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Send_Length) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Send_Offset) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Cookie) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Method) := (S_Invalid, Ctx.Cursors (F_Method).Predecessor);
            pragma Assert (Field_First (Ctx, Fld) = First
               and Field_Length (Ctx, Fld) = Length);
         when F_Cookie =>
            Ctx.Cursors (F_Recv_Length) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Recv_Offset) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Meta_Length) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Meta_Offset) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Send_Length) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Send_Offset) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Cookie) := (S_Invalid, Ctx.Cursors (F_Cookie).Predecessor);
            pragma Assert (Field_First (Ctx, Fld) = First
               and Field_Length (Ctx, Fld) = Length);
         when F_Send_Offset =>
            Ctx.Cursors (F_Recv_Length) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Recv_Offset) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Meta_Length) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Meta_Offset) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Send_Length) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Send_Offset) := (S_Invalid, Ctx.Cursors (F_Send_Offset).Predecessor);
            pragma Assert (Field_First (Ctx, Fld) = First
               and Field_Length (Ctx, Fld) = Length);
         when F_Send_Length =>
            Ctx.Cursors (F_Recv_Length) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Recv_Offset) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Meta_Length) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Meta_Offset) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Send_Length) := (S_Invalid, Ctx.Cursors (F_Send_Length).Predecessor);
            pragma Assert (Field_First (Ctx, Fld) = First
               and Field_Length (Ctx, Fld) = Length);
         when F_Meta_Offset =>
            Ctx.Cursors (F_Recv_Length) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Recv_Offset) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Meta_Length) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Meta_Offset) := (S_Invalid, Ctx.Cursors (F_Meta_Offset).Predecessor);
            pragma Assert (Field_First (Ctx, Fld) = First
               and Field_Length (Ctx, Fld) = Length);
         when F_Meta_Length =>
            Ctx.Cursors (F_Recv_Length) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Recv_Offset) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Meta_Length) := (S_Invalid, Ctx.Cursors (F_Meta_Length).Predecessor);
            pragma Assert (Field_First (Ctx, Fld) = First
               and Field_Length (Ctx, Fld) = Length);
         when F_Recv_Offset =>
            Ctx.Cursors (F_Recv_Length) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Recv_Offset) := (S_Invalid, Ctx.Cursors (F_Recv_Offset).Predecessor);
            pragma Assert (Field_First (Ctx, Fld) = First
               and Field_Length (Ctx, Fld) = Length);
         when F_Recv_Length =>
            Ctx.Cursors (F_Recv_Length) := (S_Invalid, Ctx.Cursors (F_Recv_Length).Predecessor);
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
         when F_Tag | F_Handle | F_Binder | F_Code | F_Method | F_Cookie | F_Send_Offset | F_Send_Length | F_Meta_Offset | F_Meta_Length | F_Recv_Offset | F_Recv_Length =>
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
      function Extract is new Types.Extract (Parpen.Protocol.Tag_Base);
      function Extract is new Types.Extract (Parpen.Protocol.Handle_Base);
      function Extract is new Types.Extract (Parpen.Protocol.Binder);
      function Extract is new Types.Extract (Parpen.Protocol.Status_Base);
      function Extract is new Types.Extract (Parpen.Protocol.Method_Base);
      function Extract is new Types.Extract (Parpen.Protocol.Cookie);
      function Extract is new Types.Extract (Parpen.Protocol.Offset);
      function Extract is new Types.Extract (Parpen.Protocol.Length_Base);
   begin
      return ((case Fld is
            when F_Tag =>
               (Fld => F_Tag, Tag_Value => Extract (Ctx.Buffer.all (Buffer_First .. Buffer_Last), Offset)),
            when F_Handle =>
               (Fld => F_Handle, Handle_Value => Extract (Ctx.Buffer.all (Buffer_First .. Buffer_Last), Offset)),
            when F_Binder =>
               (Fld => F_Binder, Binder_Value => Extract (Ctx.Buffer.all (Buffer_First .. Buffer_Last), Offset)),
            when F_Code =>
               (Fld => F_Code, Code_Value => Extract (Ctx.Buffer.all (Buffer_First .. Buffer_Last), Offset)),
            when F_Method =>
               (Fld => F_Method, Method_Value => Extract (Ctx.Buffer.all (Buffer_First .. Buffer_Last), Offset)),
            when F_Cookie =>
               (Fld => F_Cookie, Cookie_Value => Extract (Ctx.Buffer.all (Buffer_First .. Buffer_Last), Offset)),
            when F_Send_Offset =>
               (Fld => F_Send_Offset, Send_Offset_Value => Extract (Ctx.Buffer.all (Buffer_First .. Buffer_Last), Offset)),
            when F_Send_Length =>
               (Fld => F_Send_Length, Send_Length_Value => Extract (Ctx.Buffer.all (Buffer_First .. Buffer_Last), Offset)),
            when F_Meta_Offset =>
               (Fld => F_Meta_Offset, Meta_Offset_Value => Extract (Ctx.Buffer.all (Buffer_First .. Buffer_Last), Offset)),
            when F_Meta_Length =>
               (Fld => F_Meta_Length, Meta_Length_Value => Extract (Ctx.Buffer.all (Buffer_First .. Buffer_Last), Offset)),
            when F_Recv_Offset =>
               (Fld => F_Recv_Offset, Recv_Offset_Value => Extract (Ctx.Buffer.all (Buffer_First .. Buffer_Last), Offset)),
            when F_Recv_Length =>
               (Fld => F_Recv_Length, Recv_Length_Value => Extract (Ctx.Buffer.all (Buffer_First .. Buffer_Last), Offset))));
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
               pragma Assert ((if Structural_Valid (Ctx.Cursors (F_Tag)) then
                   (Ctx.Cursors (F_Tag).Last - Ctx.Cursors (F_Tag).First + 1) = Parpen.Protocol.Tag_Base'Size
                     and then Ctx.Cursors (F_Tag).Predecessor = F_Initial
                     and then Ctx.Cursors (F_Tag).First = Ctx.First
                     and then (if Structural_Valid (Ctx.Cursors (F_Handle))
                          and then Types.Bit_Length (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.Bit_Length (Convert (T_HANDLE)) then
                        (Ctx.Cursors (F_Handle).Last - Ctx.Cursors (F_Handle).First + 1) = Parpen.Protocol.Handle_Base'Size
                          and then Ctx.Cursors (F_Handle).Predecessor = F_Tag
                          and then Ctx.Cursors (F_Handle).First = (Ctx.Cursors (F_Tag).Last + 1)
                          and then (if Structural_Valid (Ctx.Cursors (F_Method)) then
                             (Ctx.Cursors (F_Method).Last - Ctx.Cursors (F_Method).First + 1) = Parpen.Protocol.Method_Base'Size
                               and then Ctx.Cursors (F_Method).Predecessor = F_Handle
                               and then Ctx.Cursors (F_Method).First = (Ctx.Cursors (F_Handle).Last + 1)
                               and then (if Structural_Valid (Ctx.Cursors (F_Cookie)) then
                                  (Ctx.Cursors (F_Cookie).Last - Ctx.Cursors (F_Cookie).First + 1) = Parpen.Protocol.Cookie'Size
                                    and then Ctx.Cursors (F_Cookie).Predecessor = F_Method
                                    and then Ctx.Cursors (F_Cookie).First = (Ctx.Cursors (F_Method).Last + 1)
                                    and then (if Structural_Valid (Ctx.Cursors (F_Send_Offset)) then
                                       (Ctx.Cursors (F_Send_Offset).Last - Ctx.Cursors (F_Send_Offset).First + 1) = Parpen.Protocol.Offset'Size
                                         and then Ctx.Cursors (F_Send_Offset).Predecessor = F_Cookie
                                         and then Ctx.Cursors (F_Send_Offset).First = (Ctx.Cursors (F_Cookie).Last + 1)
                                         and then (if Structural_Valid (Ctx.Cursors (F_Send_Length)) then
                                            (Ctx.Cursors (F_Send_Length).Last - Ctx.Cursors (F_Send_Length).First + 1) = Parpen.Protocol.Length_Base'Size
                                              and then Ctx.Cursors (F_Send_Length).Predecessor = F_Send_Offset
                                              and then Ctx.Cursors (F_Send_Length).First = (Ctx.Cursors (F_Send_Offset).Last + 1)
                                              and then (if Structural_Valid (Ctx.Cursors (F_Meta_Offset)) then
                                                 (Ctx.Cursors (F_Meta_Offset).Last - Ctx.Cursors (F_Meta_Offset).First + 1) = Parpen.Protocol.Offset'Size
                                                   and then Ctx.Cursors (F_Meta_Offset).Predecessor = F_Send_Length
                                                   and then Ctx.Cursors (F_Meta_Offset).First = (Ctx.Cursors (F_Send_Length).Last + 1)
                                                   and then (if Structural_Valid (Ctx.Cursors (F_Meta_Length)) then
                                                      (Ctx.Cursors (F_Meta_Length).Last - Ctx.Cursors (F_Meta_Length).First + 1) = Parpen.Protocol.Length_Base'Size
                                                        and then Ctx.Cursors (F_Meta_Length).Predecessor = F_Meta_Offset
                                                        and then Ctx.Cursors (F_Meta_Length).First = (Ctx.Cursors (F_Meta_Offset).Last + 1)
                                                        and then (if Structural_Valid (Ctx.Cursors (F_Recv_Offset)) then
                                                           (Ctx.Cursors (F_Recv_Offset).Last - Ctx.Cursors (F_Recv_Offset).First + 1) = Parpen.Protocol.Offset'Size
                                                             and then Ctx.Cursors (F_Recv_Offset).Predecessor = F_Meta_Length
                                                             and then Ctx.Cursors (F_Recv_Offset).First = (Ctx.Cursors (F_Meta_Length).Last + 1)
                                                             and then (if Structural_Valid (Ctx.Cursors (F_Recv_Length)) then
                                                                (Ctx.Cursors (F_Recv_Length).Last - Ctx.Cursors (F_Recv_Length).First + 1) = Parpen.Protocol.Length_Base'Size
                                                                  and then Ctx.Cursors (F_Recv_Length).Predecessor = F_Recv_Offset
                                                                  and then Ctx.Cursors (F_Recv_Length).First = (Ctx.Cursors (F_Recv_Offset).Last + 1))))))))))
                     and then (if Structural_Valid (Ctx.Cursors (F_Binder))
                          and then Types.Bit_Length (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.Bit_Length (Convert (T_BINDER)) then
                        (Ctx.Cursors (F_Binder).Last - Ctx.Cursors (F_Binder).First + 1) = Parpen.Protocol.Binder'Size
                          and then Ctx.Cursors (F_Binder).Predecessor = F_Tag
                          and then Ctx.Cursors (F_Binder).First = (Ctx.Cursors (F_Tag).Last + 1)
                          and then (if Structural_Valid (Ctx.Cursors (F_Method)) then
                             (Ctx.Cursors (F_Method).Last - Ctx.Cursors (F_Method).First + 1) = Parpen.Protocol.Method_Base'Size
                               and then Ctx.Cursors (F_Method).Predecessor = F_Binder
                               and then Ctx.Cursors (F_Method).First = (Ctx.Cursors (F_Binder).Last + 1)
                               and then (if Structural_Valid (Ctx.Cursors (F_Cookie)) then
                                  (Ctx.Cursors (F_Cookie).Last - Ctx.Cursors (F_Cookie).First + 1) = Parpen.Protocol.Cookie'Size
                                    and then Ctx.Cursors (F_Cookie).Predecessor = F_Method
                                    and then Ctx.Cursors (F_Cookie).First = (Ctx.Cursors (F_Method).Last + 1)
                                    and then (if Structural_Valid (Ctx.Cursors (F_Send_Offset)) then
                                       (Ctx.Cursors (F_Send_Offset).Last - Ctx.Cursors (F_Send_Offset).First + 1) = Parpen.Protocol.Offset'Size
                                         and then Ctx.Cursors (F_Send_Offset).Predecessor = F_Cookie
                                         and then Ctx.Cursors (F_Send_Offset).First = (Ctx.Cursors (F_Cookie).Last + 1)
                                         and then (if Structural_Valid (Ctx.Cursors (F_Send_Length)) then
                                            (Ctx.Cursors (F_Send_Length).Last - Ctx.Cursors (F_Send_Length).First + 1) = Parpen.Protocol.Length_Base'Size
                                              and then Ctx.Cursors (F_Send_Length).Predecessor = F_Send_Offset
                                              and then Ctx.Cursors (F_Send_Length).First = (Ctx.Cursors (F_Send_Offset).Last + 1)
                                              and then (if Structural_Valid (Ctx.Cursors (F_Meta_Offset)) then
                                                 (Ctx.Cursors (F_Meta_Offset).Last - Ctx.Cursors (F_Meta_Offset).First + 1) = Parpen.Protocol.Offset'Size
                                                   and then Ctx.Cursors (F_Meta_Offset).Predecessor = F_Send_Length
                                                   and then Ctx.Cursors (F_Meta_Offset).First = (Ctx.Cursors (F_Send_Length).Last + 1)
                                                   and then (if Structural_Valid (Ctx.Cursors (F_Meta_Length)) then
                                                      (Ctx.Cursors (F_Meta_Length).Last - Ctx.Cursors (F_Meta_Length).First + 1) = Parpen.Protocol.Length_Base'Size
                                                        and then Ctx.Cursors (F_Meta_Length).Predecessor = F_Meta_Offset
                                                        and then Ctx.Cursors (F_Meta_Length).First = (Ctx.Cursors (F_Meta_Offset).Last + 1)
                                                        and then (if Structural_Valid (Ctx.Cursors (F_Recv_Offset)) then
                                                           (Ctx.Cursors (F_Recv_Offset).Last - Ctx.Cursors (F_Recv_Offset).First + 1) = Parpen.Protocol.Offset'Size
                                                             and then Ctx.Cursors (F_Recv_Offset).Predecessor = F_Meta_Length
                                                             and then Ctx.Cursors (F_Recv_Offset).First = (Ctx.Cursors (F_Meta_Length).Last + 1)
                                                             and then (if Structural_Valid (Ctx.Cursors (F_Recv_Length)) then
                                                                (Ctx.Cursors (F_Recv_Length).Last - Ctx.Cursors (F_Recv_Length).First + 1) = Parpen.Protocol.Length_Base'Size
                                                                  and then Ctx.Cursors (F_Recv_Length).Predecessor = F_Recv_Offset
                                                                  and then Ctx.Cursors (F_Recv_Length).First = (Ctx.Cursors (F_Recv_Offset).Last + 1))))))))))
                     and then (if Structural_Valid (Ctx.Cursors (F_Code))
                          and then Types.Bit_Length (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.Bit_Length (Convert (T_STATUS)) then
                        (Ctx.Cursors (F_Code).Last - Ctx.Cursors (F_Code).First + 1) = Parpen.Protocol.Status_Base'Size
                          and then Ctx.Cursors (F_Code).Predecessor = F_Tag
                          and then Ctx.Cursors (F_Code).First = (Ctx.Cursors (F_Tag).Last + 1))));
               if Fld = F_Tag then
                  Ctx.Cursors (Successor (Ctx, Fld)) := (State => S_Invalid, Predecessor => Fld);
               elsif Fld = F_Handle then
                  Ctx.Cursors (Successor (Ctx, Fld)) := (State => S_Invalid, Predecessor => Fld);
               elsif Fld = F_Binder then
                  Ctx.Cursors (Successor (Ctx, Fld)) := (State => S_Invalid, Predecessor => Fld);
               elsif Fld = F_Code then
                  Ctx.Cursors (Successor (Ctx, Fld)) := (State => S_Invalid, Predecessor => Fld);
               elsif Fld = F_Method then
                  Ctx.Cursors (Successor (Ctx, Fld)) := (State => S_Invalid, Predecessor => Fld);
               elsif Fld = F_Cookie then
                  Ctx.Cursors (Successor (Ctx, Fld)) := (State => S_Invalid, Predecessor => Fld);
               elsif Fld = F_Send_Offset then
                  Ctx.Cursors (Successor (Ctx, Fld)) := (State => S_Invalid, Predecessor => Fld);
               elsif Fld = F_Send_Length then
                  Ctx.Cursors (Successor (Ctx, Fld)) := (State => S_Invalid, Predecessor => Fld);
               elsif Fld = F_Meta_Offset then
                  Ctx.Cursors (Successor (Ctx, Fld)) := (State => S_Invalid, Predecessor => Fld);
               elsif Fld = F_Meta_Length then
                  Ctx.Cursors (Successor (Ctx, Fld)) := (State => S_Invalid, Predecessor => Fld);
               elsif Fld = F_Recv_Offset then
                  Ctx.Cursors (Successor (Ctx, Fld)) := (State => S_Invalid, Predecessor => Fld);
               elsif Fld = F_Recv_Length then
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
      Verify (Ctx, F_Tag);
      Verify (Ctx, F_Handle);
      Verify (Ctx, F_Binder);
      Verify (Ctx, F_Code);
      Verify (Ctx, F_Method);
      Verify (Ctx, F_Cookie);
      Verify (Ctx, F_Send_Offset);
      Verify (Ctx, F_Send_Length);
      Verify (Ctx, F_Meta_Offset);
      Verify (Ctx, F_Meta_Length);
      Verify (Ctx, F_Recv_Offset);
      Verify (Ctx, F_Recv_Length);
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
     (Valid (Ctx, F_Tag)
      and then ((Valid (Ctx, F_Handle)
          and then Types.Bit_Length (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.Bit_Length (Convert (T_HANDLE))
          and then Valid (Ctx, F_Method)
          and then Valid (Ctx, F_Cookie)
          and then Valid (Ctx, F_Send_Offset)
          and then Valid (Ctx, F_Send_Length)
          and then Valid (Ctx, F_Meta_Offset)
          and then Valid (Ctx, F_Meta_Length)
          and then Valid (Ctx, F_Recv_Offset)
          and then Valid (Ctx, F_Recv_Length))
        or (Valid (Ctx, F_Binder)
          and then Types.Bit_Length (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.Bit_Length (Convert (T_BINDER))
          and then Valid (Ctx, F_Method)
          and then Valid (Ctx, F_Cookie)
          and then Valid (Ctx, F_Send_Offset)
          and then Valid (Ctx, F_Send_Length)
          and then Valid (Ctx, F_Meta_Offset)
          and then Valid (Ctx, F_Meta_Length)
          and then Valid (Ctx, F_Recv_Offset)
          and then Valid (Ctx, F_Recv_Length))
        or (Valid (Ctx, F_Code)
          and then Types.Bit_Length (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.Bit_Length (Convert (T_STATUS)))));

   function Valid_Message (Ctx : Context) return Boolean is
     (Valid (Ctx, F_Tag)
      and then ((Valid (Ctx, F_Handle)
          and then Types.Bit_Length (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.Bit_Length (Convert (T_HANDLE))
          and then Valid (Ctx, F_Method)
          and then Valid (Ctx, F_Cookie)
          and then Valid (Ctx, F_Send_Offset)
          and then Valid (Ctx, F_Send_Length)
          and then Valid (Ctx, F_Meta_Offset)
          and then Valid (Ctx, F_Meta_Length)
          and then Valid (Ctx, F_Recv_Offset)
          and then Valid (Ctx, F_Recv_Length))
        or (Valid (Ctx, F_Binder)
          and then Types.Bit_Length (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.Bit_Length (Convert (T_BINDER))
          and then Valid (Ctx, F_Method)
          and then Valid (Ctx, F_Cookie)
          and then Valid (Ctx, F_Send_Offset)
          and then Valid (Ctx, F_Send_Length)
          and then Valid (Ctx, F_Meta_Offset)
          and then Valid (Ctx, F_Meta_Length)
          and then Valid (Ctx, F_Recv_Offset)
          and then Valid (Ctx, F_Recv_Length))
        or (Valid (Ctx, F_Code)
          and then Types.Bit_Length (Ctx.Cursors (F_Tag).Value.Tag_Value) = Types.Bit_Length (Convert (T_STATUS)))));

   function Incomplete_Message (Ctx : Context) return Boolean is
     (Incomplete (Ctx, F_Tag)
      or Incomplete (Ctx, F_Handle)
      or Incomplete (Ctx, F_Binder)
      or Incomplete (Ctx, F_Code)
      or Incomplete (Ctx, F_Method)
      or Incomplete (Ctx, F_Cookie)
      or Incomplete (Ctx, F_Send_Offset)
      or Incomplete (Ctx, F_Send_Length)
      or Incomplete (Ctx, F_Meta_Offset)
      or Incomplete (Ctx, F_Meta_Length)
      or Incomplete (Ctx, F_Recv_Offset)
      or Incomplete (Ctx, F_Recv_Length));

   function Get_Tag (Ctx : Context) return Parpen.Protocol.Tag is
     (Convert (Ctx.Cursors (F_Tag).Value.Tag_Value));

   function Get_Handle (Ctx : Context) return Parpen.Protocol.Handle is
     (Ctx.Cursors (F_Handle).Value.Handle_Value);

   function Get_Binder (Ctx : Context) return Parpen.Protocol.Binder is
     (Ctx.Cursors (F_Binder).Value.Binder_Value);

   function Get_Code (Ctx : Context) return Parpen.Protocol.Status is
     (Convert (Ctx.Cursors (F_Code).Value.Code_Value));

   function Get_Method (Ctx : Context) return Parpen.Protocol.Method is
     (Ctx.Cursors (F_Method).Value.Method_Value);

   function Get_Cookie (Ctx : Context) return Parpen.Protocol.Cookie is
     (Ctx.Cursors (F_Cookie).Value.Cookie_Value);

   function Get_Send_Offset (Ctx : Context) return Parpen.Protocol.Offset is
     (Ctx.Cursors (F_Send_Offset).Value.Send_Offset_Value);

   function Get_Send_Length (Ctx : Context) return Parpen.Protocol.Length is
     (Ctx.Cursors (F_Send_Length).Value.Send_Length_Value);

   function Get_Meta_Offset (Ctx : Context) return Parpen.Protocol.Offset is
     (Ctx.Cursors (F_Meta_Offset).Value.Meta_Offset_Value);

   function Get_Meta_Length (Ctx : Context) return Parpen.Protocol.Length is
     (Ctx.Cursors (F_Meta_Length).Value.Meta_Length_Value);

   function Get_Recv_Offset (Ctx : Context) return Parpen.Protocol.Offset is
     (Ctx.Cursors (F_Recv_Offset).Value.Recv_Offset_Value);

   function Get_Recv_Length (Ctx : Context) return Parpen.Protocol.Length is
     (Ctx.Cursors (F_Recv_Length).Value.Recv_Length_Value);

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
      procedure Insert is new Types.Insert (Parpen.Protocol.Tag_Base);
      procedure Insert is new Types.Insert (Parpen.Protocol.Handle_Base);
      procedure Insert is new Types.Insert (Parpen.Protocol.Binder);
      procedure Insert is new Types.Insert (Parpen.Protocol.Status_Base);
      procedure Insert is new Types.Insert (Parpen.Protocol.Method_Base);
      procedure Insert is new Types.Insert (Parpen.Protocol.Cookie);
      procedure Insert is new Types.Insert (Parpen.Protocol.Offset);
      procedure Insert is new Types.Insert (Parpen.Protocol.Length_Base);
   begin
      Fst := First;
      Lst := Last;
      case Val.Fld is
         when F_Initial =>
            null;
         when F_Tag =>
            Insert (Val.Tag_Value, Ctx.Buffer.all (Buffer_First .. Buffer_Last), Offset);
         when F_Handle =>
            Insert (Val.Handle_Value, Ctx.Buffer.all (Buffer_First .. Buffer_Last), Offset);
         when F_Binder =>
            Insert (Val.Binder_Value, Ctx.Buffer.all (Buffer_First .. Buffer_Last), Offset);
         when F_Code =>
            Insert (Val.Code_Value, Ctx.Buffer.all (Buffer_First .. Buffer_Last), Offset);
         when F_Method =>
            Insert (Val.Method_Value, Ctx.Buffer.all (Buffer_First .. Buffer_Last), Offset);
         when F_Cookie =>
            Insert (Val.Cookie_Value, Ctx.Buffer.all (Buffer_First .. Buffer_Last), Offset);
         when F_Send_Offset =>
            Insert (Val.Send_Offset_Value, Ctx.Buffer.all (Buffer_First .. Buffer_Last), Offset);
         when F_Send_Length =>
            Insert (Val.Send_Length_Value, Ctx.Buffer.all (Buffer_First .. Buffer_Last), Offset);
         when F_Meta_Offset =>
            Insert (Val.Meta_Offset_Value, Ctx.Buffer.all (Buffer_First .. Buffer_Last), Offset);
         when F_Meta_Length =>
            Insert (Val.Meta_Length_Value, Ctx.Buffer.all (Buffer_First .. Buffer_Last), Offset);
         when F_Recv_Offset =>
            Insert (Val.Recv_Offset_Value, Ctx.Buffer.all (Buffer_First .. Buffer_Last), Offset);
         when F_Recv_Length =>
            Insert (Val.Recv_Length_Value, Ctx.Buffer.all (Buffer_First .. Buffer_Last), Offset);
         when F_Final =>
            null;
      end case;
   end Set_Field_Value;

   procedure Set_Tag (Ctx : in out Context; Val : Parpen.Protocol.Tag) is
      Field_Value : constant Field_Dependent_Value := (F_Tag, Convert (Val));
      First, Last : Types.Bit_Index;
   begin
      Reset_Dependent_Fields (Ctx, F_Tag);
      Set_Field_Value (Ctx, Field_Value, First, Last);
      Ctx := (Ctx.Buffer_First, Ctx.Buffer_Last, Ctx.First, Last, Ctx.Buffer, Ctx.Cursors);
      Ctx.Cursors (F_Tag) := (State => S_Valid, First => First, Last => Last, Value => Field_Value, Predecessor => Ctx.Cursors (F_Tag).Predecessor);
      Ctx.Cursors (Successor (Ctx, F_Tag)) := (State => S_Invalid, Predecessor => F_Tag);
   end Set_Tag;

   procedure Set_Handle (Ctx : in out Context; Val : Parpen.Protocol.Handle) is
      Field_Value : constant Field_Dependent_Value := (F_Handle, Val);
      First, Last : Types.Bit_Index;
   begin
      Reset_Dependent_Fields (Ctx, F_Handle);
      Set_Field_Value (Ctx, Field_Value, First, Last);
      Ctx := (Ctx.Buffer_First, Ctx.Buffer_Last, Ctx.First, Last, Ctx.Buffer, Ctx.Cursors);
      Ctx.Cursors (F_Handle) := (State => S_Valid, First => First, Last => Last, Value => Field_Value, Predecessor => Ctx.Cursors (F_Handle).Predecessor);
      Ctx.Cursors (Successor (Ctx, F_Handle)) := (State => S_Invalid, Predecessor => F_Handle);
   end Set_Handle;

   procedure Set_Binder (Ctx : in out Context; Val : Parpen.Protocol.Binder) is
      Field_Value : constant Field_Dependent_Value := (F_Binder, Val);
      First, Last : Types.Bit_Index;
   begin
      Reset_Dependent_Fields (Ctx, F_Binder);
      Set_Field_Value (Ctx, Field_Value, First, Last);
      Ctx := (Ctx.Buffer_First, Ctx.Buffer_Last, Ctx.First, Last, Ctx.Buffer, Ctx.Cursors);
      Ctx.Cursors (F_Binder) := (State => S_Valid, First => First, Last => Last, Value => Field_Value, Predecessor => Ctx.Cursors (F_Binder).Predecessor);
      Ctx.Cursors (Successor (Ctx, F_Binder)) := (State => S_Invalid, Predecessor => F_Binder);
   end Set_Binder;

   procedure Set_Code (Ctx : in out Context; Val : Parpen.Protocol.Status) is
      Field_Value : constant Field_Dependent_Value := (F_Code, Convert (Val));
      First, Last : Types.Bit_Index;
   begin
      Reset_Dependent_Fields (Ctx, F_Code);
      Set_Field_Value (Ctx, Field_Value, First, Last);
      Ctx := (Ctx.Buffer_First, Ctx.Buffer_Last, Ctx.First, Last, Ctx.Buffer, Ctx.Cursors);
      Ctx.Cursors (F_Code) := (State => S_Valid, First => First, Last => Last, Value => Field_Value, Predecessor => Ctx.Cursors (F_Code).Predecessor);
      Ctx.Cursors (Successor (Ctx, F_Code)) := (State => S_Invalid, Predecessor => F_Code);
   end Set_Code;

   procedure Set_Method (Ctx : in out Context; Val : Parpen.Protocol.Method) is
      Field_Value : constant Field_Dependent_Value := (F_Method, Val);
      First, Last : Types.Bit_Index;
   begin
      Reset_Dependent_Fields (Ctx, F_Method);
      Set_Field_Value (Ctx, Field_Value, First, Last);
      Ctx := (Ctx.Buffer_First, Ctx.Buffer_Last, Ctx.First, Last, Ctx.Buffer, Ctx.Cursors);
      Ctx.Cursors (F_Method) := (State => S_Valid, First => First, Last => Last, Value => Field_Value, Predecessor => Ctx.Cursors (F_Method).Predecessor);
      Ctx.Cursors (Successor (Ctx, F_Method)) := (State => S_Invalid, Predecessor => F_Method);
   end Set_Method;

   procedure Set_Cookie (Ctx : in out Context; Val : Parpen.Protocol.Cookie) is
      Field_Value : constant Field_Dependent_Value := (F_Cookie, Val);
      First, Last : Types.Bit_Index;
   begin
      Reset_Dependent_Fields (Ctx, F_Cookie);
      Set_Field_Value (Ctx, Field_Value, First, Last);
      Ctx := (Ctx.Buffer_First, Ctx.Buffer_Last, Ctx.First, Last, Ctx.Buffer, Ctx.Cursors);
      Ctx.Cursors (F_Cookie) := (State => S_Valid, First => First, Last => Last, Value => Field_Value, Predecessor => Ctx.Cursors (F_Cookie).Predecessor);
      Ctx.Cursors (Successor (Ctx, F_Cookie)) := (State => S_Invalid, Predecessor => F_Cookie);
   end Set_Cookie;

   procedure Set_Send_Offset (Ctx : in out Context; Val : Parpen.Protocol.Offset) is
      Field_Value : constant Field_Dependent_Value := (F_Send_Offset, Val);
      First, Last : Types.Bit_Index;
   begin
      Reset_Dependent_Fields (Ctx, F_Send_Offset);
      Set_Field_Value (Ctx, Field_Value, First, Last);
      Ctx := (Ctx.Buffer_First, Ctx.Buffer_Last, Ctx.First, Last, Ctx.Buffer, Ctx.Cursors);
      Ctx.Cursors (F_Send_Offset) := (State => S_Valid, First => First, Last => Last, Value => Field_Value, Predecessor => Ctx.Cursors (F_Send_Offset).Predecessor);
      Ctx.Cursors (Successor (Ctx, F_Send_Offset)) := (State => S_Invalid, Predecessor => F_Send_Offset);
   end Set_Send_Offset;

   procedure Set_Send_Length (Ctx : in out Context; Val : Parpen.Protocol.Length) is
      Field_Value : constant Field_Dependent_Value := (F_Send_Length, Val);
      First, Last : Types.Bit_Index;
   begin
      Reset_Dependent_Fields (Ctx, F_Send_Length);
      Set_Field_Value (Ctx, Field_Value, First, Last);
      Ctx := (Ctx.Buffer_First, Ctx.Buffer_Last, Ctx.First, Last, Ctx.Buffer, Ctx.Cursors);
      Ctx.Cursors (F_Send_Length) := (State => S_Valid, First => First, Last => Last, Value => Field_Value, Predecessor => Ctx.Cursors (F_Send_Length).Predecessor);
      Ctx.Cursors (Successor (Ctx, F_Send_Length)) := (State => S_Invalid, Predecessor => F_Send_Length);
   end Set_Send_Length;

   procedure Set_Meta_Offset (Ctx : in out Context; Val : Parpen.Protocol.Offset) is
      Field_Value : constant Field_Dependent_Value := (F_Meta_Offset, Val);
      First, Last : Types.Bit_Index;
   begin
      Reset_Dependent_Fields (Ctx, F_Meta_Offset);
      Set_Field_Value (Ctx, Field_Value, First, Last);
      Ctx := (Ctx.Buffer_First, Ctx.Buffer_Last, Ctx.First, Last, Ctx.Buffer, Ctx.Cursors);
      Ctx.Cursors (F_Meta_Offset) := (State => S_Valid, First => First, Last => Last, Value => Field_Value, Predecessor => Ctx.Cursors (F_Meta_Offset).Predecessor);
      Ctx.Cursors (Successor (Ctx, F_Meta_Offset)) := (State => S_Invalid, Predecessor => F_Meta_Offset);
   end Set_Meta_Offset;

   procedure Set_Meta_Length (Ctx : in out Context; Val : Parpen.Protocol.Length) is
      Field_Value : constant Field_Dependent_Value := (F_Meta_Length, Val);
      First, Last : Types.Bit_Index;
   begin
      Reset_Dependent_Fields (Ctx, F_Meta_Length);
      Set_Field_Value (Ctx, Field_Value, First, Last);
      Ctx := (Ctx.Buffer_First, Ctx.Buffer_Last, Ctx.First, Last, Ctx.Buffer, Ctx.Cursors);
      Ctx.Cursors (F_Meta_Length) := (State => S_Valid, First => First, Last => Last, Value => Field_Value, Predecessor => Ctx.Cursors (F_Meta_Length).Predecessor);
      Ctx.Cursors (Successor (Ctx, F_Meta_Length)) := (State => S_Invalid, Predecessor => F_Meta_Length);
   end Set_Meta_Length;

   procedure Set_Recv_Offset (Ctx : in out Context; Val : Parpen.Protocol.Offset) is
      Field_Value : constant Field_Dependent_Value := (F_Recv_Offset, Val);
      First, Last : Types.Bit_Index;
   begin
      Reset_Dependent_Fields (Ctx, F_Recv_Offset);
      Set_Field_Value (Ctx, Field_Value, First, Last);
      Ctx := (Ctx.Buffer_First, Ctx.Buffer_Last, Ctx.First, Last, Ctx.Buffer, Ctx.Cursors);
      Ctx.Cursors (F_Recv_Offset) := (State => S_Valid, First => First, Last => Last, Value => Field_Value, Predecessor => Ctx.Cursors (F_Recv_Offset).Predecessor);
      Ctx.Cursors (Successor (Ctx, F_Recv_Offset)) := (State => S_Invalid, Predecessor => F_Recv_Offset);
   end Set_Recv_Offset;

   procedure Set_Recv_Length (Ctx : in out Context; Val : Parpen.Protocol.Length) is
      Field_Value : constant Field_Dependent_Value := (F_Recv_Length, Val);
      First, Last : Types.Bit_Index;
   begin
      Reset_Dependent_Fields (Ctx, F_Recv_Length);
      Set_Field_Value (Ctx, Field_Value, First, Last);
      Ctx := (Ctx.Buffer_First, Ctx.Buffer_Last, Ctx.First, Last, Ctx.Buffer, Ctx.Cursors);
      Ctx.Cursors (F_Recv_Length) := (State => S_Valid, First => First, Last => Last, Value => Field_Value, Predecessor => Ctx.Cursors (F_Recv_Length).Predecessor);
      Ctx.Cursors (Successor (Ctx, F_Recv_Length)) := (State => S_Invalid, Predecessor => F_Recv_Length);
   end Set_Recv_Length;

end Parpen.Protocol.Generic_Transaction;
