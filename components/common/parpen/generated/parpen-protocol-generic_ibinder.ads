with Parpen.Generic_Types;

generic
   with package Types is new Parpen.Generic_Types (<>);
package Parpen.Protocol.Generic_IBinder with
  SPARK_Mode
is

   pragma Annotate (GNATprove, Terminating, Generic_IBinder);

   use type Types.Bytes, Types.Bytes_Ptr, Types.Index, Types.Length, Types.Bit_Index, Types.Bit_Length;

   type Virtual_Field is (F_Initial, F_Kind, F_Arity, F_Tag, F_Flags, F_Binder, F_Handle, F_Cookie, F_Final);

   subtype Field is Virtual_Field range F_Kind .. F_Cookie;

   type Field_Cursor is private with
     Default_Initial_Condition =>
       False;

   type Field_Cursors is array (Virtual_Field) of Field_Cursor;

   type Context (Buffer_First, Buffer_Last : Types.Index := Types.Index'First; First, Last : Types.Bit_Index := Types.Bit_Index'First) is private with
     Default_Initial_Condition =>
       False;

   type Field_Dependent_Value (Fld : Virtual_Field := F_Initial) is
      record
         case Fld is
            when F_Initial | F_Final =>
               null;
            when F_Kind =>
               Kind_Value : Protocol.Binder_Kind_Base;
            when F_Arity =>
               Arity_Value : Protocol.Binder_Arity_Base;
            when F_Tag =>
               Tag_Value : Protocol.Binder_Tag_Base;
            when F_Flags =>
               Flags_Value : Protocol.Flat_Binder_Flags_Base;
            when F_Binder =>
               Binder_Value : Protocol.Binder;
            when F_Handle =>
               Handle_Value : Protocol.Handle_Base;
            when F_Cookie =>
               Cookie_Value : Protocol.Cookie;
         end case;
      end record;

   function Create return Context;

   procedure Initialize (Ctx : out Context; Buffer : in out Types.Bytes_Ptr) with
     Pre =>
       not Ctx'Constrained
          and then Buffer /= null
          and then Buffer'Length > 0
          and then Buffer'Last <= Types.Index'Last / 2,
     Post =>
       Valid_Context (Ctx)
          and Has_Buffer (Ctx)
          and Buffer = null
          and Ctx.Buffer_First = Types.Bytes_First (Buffer)'Old
          and Ctx.Buffer_Last = Types.Bytes_Last (Buffer)'Old
          and Ctx.First = Types.First_Bit_Index (Ctx.Buffer_First)
          and Initialized (Ctx);

   procedure Initialize (Ctx : out Context; Buffer : in out Types.Bytes_Ptr; First, Last : Types.Bit_Index) with
     Pre =>
       not Ctx'Constrained
          and then Buffer /= null
          and then Buffer'Length > 0
          and then Types.Byte_Index (First) >= Buffer'First
          and then Types.Byte_Index (Last) <= Buffer'Last
          and then First <= Last
          and then Last <= Types.Bit_Index'Last / 2,
     Post =>
       Valid_Context (Ctx)
          and Buffer = null
          and Has_Buffer (Ctx)
          and Ctx.Buffer_First = Types.Bytes_First (Buffer)'Old
          and Ctx.Buffer_Last = Types.Bytes_Last (Buffer)'Old
          and Ctx.First = First
          and Ctx.Last = Last
          and Initialized (Ctx);

   function Initialized (Ctx : Context) return Boolean with
     Ghost;

   procedure Take_Buffer (Ctx : in out Context; Buffer : out Types.Bytes_Ptr) with
     Pre =>
       Valid_Context (Ctx)
          and Has_Buffer (Ctx),
     Post =>
       Valid_Context (Ctx)
          and not Has_Buffer (Ctx)
          and Buffer /= null
          and Ctx.Buffer_First = Buffer'First
          and Ctx.Buffer_Last = Buffer'Last
          and Ctx.Buffer_First = Ctx.Buffer_First'Old
          and Ctx.Buffer_Last = Ctx.Buffer_Last'Old
          and Ctx.First = Ctx.First'Old
          and Ctx.Last = Ctx.Last'Old
          and Cursors (Ctx) = Cursors (Ctx)'Old;

   function Has_Buffer (Ctx : Context) return Boolean with
     Pre =>
       Valid_Context (Ctx);

   function Message_Last (Ctx : Context) return Types.Bit_Index with
     Pre =>
       Valid_Context (Ctx)
          and Structural_Valid_Message (Ctx);

   function Path_Condition (Ctx : Context; Fld : Field) return Boolean with
     Pre =>
       Valid_Context (Ctx)
          and Valid_Predecessor (Ctx, Fld);

   function Field_Condition (Ctx : Context; Val : Field_Dependent_Value) return Boolean with
     Pre =>
       Valid_Context (Ctx)
          and Val.Fld in Field'Range
          and Valid_Predecessor (Ctx, Val.Fld);

   function Field_Length (Ctx : Context; Fld : Field) return Types.Bit_Length with
     Pre =>
       Valid_Context (Ctx)
          and Valid_Next (Ctx, Fld);

   function Field_First (Ctx : Context; Fld : Field) return Types.Bit_Index with
     Pre =>
       Valid_Context (Ctx)
          and Valid_Next (Ctx, Fld);

   function Field_Last (Ctx : Context; Fld : Field) return Types.Bit_Index with
     Pre =>
       Valid_Next (Ctx, Fld);

   function Predecessor (Ctx : Context; Fld : Virtual_Field) return Virtual_Field with
     Pre =>
       Valid_Context (Ctx);

   function Valid_Predecessor (Ctx : Context; Fld : Virtual_Field) return Boolean with
     Pre =>
       Valid_Context (Ctx);

   function Valid_Next (Ctx : Context; Fld : Field) return Boolean with
     Pre =>
       Valid_Context (Ctx);

   function Available_Space (Ctx : Context; Fld : Field) return Types.Bit_Length with
     Pre =>
       Valid_Context (Ctx)
          and Valid_Next (Ctx, Fld);

   procedure Verify (Ctx : in out Context; Fld : Field) with
     Pre =>
       Valid_Context (Ctx),
     Post =>
       Valid_Context (Ctx)
          and Has_Buffer (Ctx) = Has_Buffer (Ctx)'Old
          and Ctx.Buffer_First = Ctx.Buffer_First'Old
          and Ctx.Buffer_Last = Ctx.Buffer_Last'Old
          and Ctx.First = Ctx.First'Old
          and Ctx.Last = Ctx.Last'Old;

   procedure Verify_Message (Ctx : in out Context) with
     Pre =>
       Valid_Context (Ctx),
     Post =>
       Valid_Context (Ctx)
          and Has_Buffer (Ctx) = Has_Buffer (Ctx)'Old
          and Ctx.Buffer_First = Ctx.Buffer_First'Old
          and Ctx.Buffer_Last = Ctx.Buffer_Last'Old
          and Ctx.First = Ctx.First'Old
          and Ctx.Last = Ctx.Last'Old;

   function Present (Ctx : Context; Fld : Field) return Boolean with
     Pre =>
       Valid_Context (Ctx);

   function Structural_Valid (Ctx : Context; Fld : Field) return Boolean with
     Pre =>
       Valid_Context (Ctx);

   function Valid (Ctx : Context; Fld : Field) return Boolean with
     Pre =>
       Valid_Context (Ctx),
     Post =>
       (if Valid'Result then
           Structural_Valid (Ctx, Fld)
             and Present (Ctx, Fld));

   function Incomplete (Ctx : Context; Fld : Field) return Boolean with
     Pre =>
       Valid_Context (Ctx);

   function Invalid (Ctx : Context; Fld : Field) return Boolean with
     Pre =>
       Valid_Context (Ctx);

   function Structural_Valid_Message (Ctx : Context) return Boolean with
     Pre =>
       Valid_Context (Ctx);

   function Valid_Message (Ctx : Context) return Boolean with
     Pre =>
       Valid_Context (Ctx);

   function Incomplete_Message (Ctx : Context) return Boolean with
     Pre =>
       Valid_Context (Ctx);

   function Get_Kind (Ctx : Context) return Protocol.Binder_Kind with
     Pre =>
       Valid_Context (Ctx)
          and Valid (Ctx, F_Kind);

   function Get_Arity (Ctx : Context) return Protocol.Binder_Arity with
     Pre =>
       Valid_Context (Ctx)
          and Valid (Ctx, F_Arity);

   function Get_Tag (Ctx : Context) return Protocol.Binder_Tag with
     Pre =>
       Valid_Context (Ctx)
          and Valid (Ctx, F_Tag);

   function Get_Flags (Ctx : Context) return Protocol.Flat_Binder_Flags with
     Pre =>
       Valid_Context (Ctx)
          and Valid (Ctx, F_Flags);

   function Get_Binder (Ctx : Context) return Protocol.Binder with
     Pre =>
       Valid_Context (Ctx)
          and Valid (Ctx, F_Binder);

   function Get_Handle (Ctx : Context) return Protocol.Handle with
     Pre =>
       Valid_Context (Ctx)
          and Valid (Ctx, F_Handle);

   function Get_Cookie (Ctx : Context) return Protocol.Cookie with
     Pre =>
       Valid_Context (Ctx)
          and Valid (Ctx, F_Cookie);

   procedure Set_Kind (Ctx : in out Context; Val : Protocol.Binder_Kind) with
     Pre =>
       Valid_Context (Ctx)
          and then not Ctx'Constrained
          and then Has_Buffer (Ctx)
          and then Valid_Next (Ctx, F_Kind)
          and then Field_Last (Ctx, F_Kind) <= Types.Bit_Index'Last / 2
          and then Field_Condition (Ctx, (F_Kind, Convert (Val)))
          and then True
          and then Available_Space (Ctx, F_Kind) >= Field_Length (Ctx, F_Kind),
     Post =>
       Valid_Context (Ctx)
          and Has_Buffer (Ctx)
          and Valid (Ctx, F_Kind)
          and Get_Kind (Ctx) = Val
          and Invalid (Ctx, F_Arity)
          and Invalid (Ctx, F_Tag)
          and Invalid (Ctx, F_Flags)
          and Invalid (Ctx, F_Binder)
          and Invalid (Ctx, F_Handle)
          and Invalid (Ctx, F_Cookie)
          and (Predecessor (Ctx, F_Arity) = F_Kind
            and Valid_Next (Ctx, F_Arity))
          and Ctx.Buffer_First = Ctx.Buffer_First'Old
          and Ctx.Buffer_Last = Ctx.Buffer_Last'Old
          and Ctx.First = Ctx.First'Old
          and Predecessor (Ctx, F_Kind) = Predecessor (Ctx, F_Kind)'Old
          and Valid_Next (Ctx, F_Kind) = Valid_Next (Ctx, F_Kind)'Old;

   procedure Set_Arity (Ctx : in out Context; Val : Protocol.Binder_Arity) with
     Pre =>
       Valid_Context (Ctx)
          and then not Ctx'Constrained
          and then Has_Buffer (Ctx)
          and then Valid_Next (Ctx, F_Arity)
          and then Field_Last (Ctx, F_Arity) <= Types.Bit_Index'Last / 2
          and then Field_Condition (Ctx, (F_Arity, Convert (Val)))
          and then True
          and then Available_Space (Ctx, F_Arity) >= Field_Length (Ctx, F_Arity),
     Post =>
       Valid_Context (Ctx)
          and Has_Buffer (Ctx)
          and Valid (Ctx, F_Arity)
          and Get_Arity (Ctx) = Val
          and Invalid (Ctx, F_Tag)
          and Invalid (Ctx, F_Flags)
          and Invalid (Ctx, F_Binder)
          and Invalid (Ctx, F_Handle)
          and Invalid (Ctx, F_Cookie)
          and (if Types.Bit_Length (Convert (Get_Arity (Ctx))) = Types.Bit_Length (Convert (BA_SINGLE))
               or (Types.Bit_Length (Convert (Get_Kind (Ctx))) = Types.Bit_Length (Convert (BK_FD))
                 and Types.Bit_Length (Convert (Get_Arity (Ctx))) = Types.Bit_Length (Convert (BA_ARRAY))) then
             Predecessor (Ctx, F_Tag) = F_Arity
               and Valid_Next (Ctx, F_Tag))
          and Ctx.Buffer_First = Ctx.Buffer_First'Old
          and Ctx.Buffer_Last = Ctx.Buffer_Last'Old
          and Ctx.First = Ctx.First'Old
          and Predecessor (Ctx, F_Arity) = Predecessor (Ctx, F_Arity)'Old
          and Valid_Next (Ctx, F_Arity) = Valid_Next (Ctx, F_Arity)'Old
          and Get_Kind (Ctx) = Get_Kind (Ctx)'Old
          and Cursor (Ctx, F_Kind) = Cursor (Ctx, F_Kind)'Old;

   procedure Set_Tag (Ctx : in out Context; Val : Protocol.Binder_Tag) with
     Pre =>
       Valid_Context (Ctx)
          and then not Ctx'Constrained
          and then Has_Buffer (Ctx)
          and then Valid_Next (Ctx, F_Tag)
          and then Field_Last (Ctx, F_Tag) <= Types.Bit_Index'Last / 2
          and then Field_Condition (Ctx, (F_Tag, Val))
          and then Valid (Val)
          and then Available_Space (Ctx, F_Tag) >= Field_Length (Ctx, F_Tag),
     Post =>
       Valid_Context (Ctx)
          and Has_Buffer (Ctx)
          and Valid (Ctx, F_Tag)
          and Get_Tag (Ctx) = Val
          and Invalid (Ctx, F_Flags)
          and Invalid (Ctx, F_Binder)
          and Invalid (Ctx, F_Handle)
          and Invalid (Ctx, F_Cookie)
          and (Predecessor (Ctx, F_Flags) = F_Tag
            and Valid_Next (Ctx, F_Flags))
          and Ctx.Buffer_First = Ctx.Buffer_First'Old
          and Ctx.Buffer_Last = Ctx.Buffer_Last'Old
          and Ctx.First = Ctx.First'Old
          and Predecessor (Ctx, F_Tag) = Predecessor (Ctx, F_Tag)'Old
          and Valid_Next (Ctx, F_Tag) = Valid_Next (Ctx, F_Tag)'Old
          and Get_Kind (Ctx) = Get_Kind (Ctx)'Old
          and Get_Arity (Ctx) = Get_Arity (Ctx)'Old
          and Cursor (Ctx, F_Kind) = Cursor (Ctx, F_Kind)'Old
          and Cursor (Ctx, F_Arity) = Cursor (Ctx, F_Arity)'Old;

   procedure Set_Flags (Ctx : in out Context; Val : Protocol.Flat_Binder_Flags) with
     Pre =>
       Valid_Context (Ctx)
          and then not Ctx'Constrained
          and then Has_Buffer (Ctx)
          and then Valid_Next (Ctx, F_Flags)
          and then Field_Last (Ctx, F_Flags) <= Types.Bit_Index'Last / 2
          and then Field_Condition (Ctx, (F_Flags, Convert (Val)))
          and then True
          and then Available_Space (Ctx, F_Flags) >= Field_Length (Ctx, F_Flags),
     Post =>
       Valid_Context (Ctx)
          and Has_Buffer (Ctx)
          and Valid (Ctx, F_Flags)
          and Get_Flags (Ctx) = Val
          and Invalid (Ctx, F_Binder)
          and Invalid (Ctx, F_Handle)
          and Invalid (Ctx, F_Cookie)
          and (if Types.Bit_Length (Convert (Get_Kind (Ctx))) = Types.Bit_Length (Convert (BK_STRONG_BINDER))
               or Types.Bit_Length (Convert (Get_Kind (Ctx))) = Types.Bit_Length (Convert (BK_WEAK_BINDER)) then
             Predecessor (Ctx, F_Binder) = F_Flags
               and Valid_Next (Ctx, F_Binder))
          and (if Types.Bit_Length (Convert (Get_Kind (Ctx))) = Types.Bit_Length (Convert (BK_STRONG_HANDLE))
               or Types.Bit_Length (Convert (Get_Kind (Ctx))) = Types.Bit_Length (Convert (BK_WEAK_HANDLE)) then
             Predecessor (Ctx, F_Handle) = F_Flags
               and Valid_Next (Ctx, F_Handle))
          and Ctx.Buffer_First = Ctx.Buffer_First'Old
          and Ctx.Buffer_Last = Ctx.Buffer_Last'Old
          and Ctx.First = Ctx.First'Old
          and Predecessor (Ctx, F_Flags) = Predecessor (Ctx, F_Flags)'Old
          and Valid_Next (Ctx, F_Flags) = Valid_Next (Ctx, F_Flags)'Old
          and Get_Kind (Ctx) = Get_Kind (Ctx)'Old
          and Get_Arity (Ctx) = Get_Arity (Ctx)'Old
          and Get_Tag (Ctx) = Get_Tag (Ctx)'Old
          and Cursor (Ctx, F_Kind) = Cursor (Ctx, F_Kind)'Old
          and Cursor (Ctx, F_Arity) = Cursor (Ctx, F_Arity)'Old
          and Cursor (Ctx, F_Tag) = Cursor (Ctx, F_Tag)'Old;

   procedure Set_Binder (Ctx : in out Context; Val : Protocol.Binder) with
     Pre =>
       Valid_Context (Ctx)
          and then not Ctx'Constrained
          and then Has_Buffer (Ctx)
          and then Valid_Next (Ctx, F_Binder)
          and then Field_Last (Ctx, F_Binder) <= Types.Bit_Index'Last / 2
          and then Field_Condition (Ctx, (F_Binder, Val))
          and then Valid (Val)
          and then Available_Space (Ctx, F_Binder) >= Field_Length (Ctx, F_Binder),
     Post =>
       Valid_Context (Ctx)
          and Has_Buffer (Ctx)
          and Valid (Ctx, F_Binder)
          and Get_Binder (Ctx) = Val
          and Invalid (Ctx, F_Handle)
          and Invalid (Ctx, F_Cookie)
          and (Predecessor (Ctx, F_Cookie) = F_Binder
            and Valid_Next (Ctx, F_Cookie))
          and Ctx.Buffer_First = Ctx.Buffer_First'Old
          and Ctx.Buffer_Last = Ctx.Buffer_Last'Old
          and Ctx.First = Ctx.First'Old
          and Predecessor (Ctx, F_Binder) = Predecessor (Ctx, F_Binder)'Old
          and Valid_Next (Ctx, F_Binder) = Valid_Next (Ctx, F_Binder)'Old
          and Get_Kind (Ctx) = Get_Kind (Ctx)'Old
          and Get_Arity (Ctx) = Get_Arity (Ctx)'Old
          and Get_Tag (Ctx) = Get_Tag (Ctx)'Old
          and Get_Flags (Ctx) = Get_Flags (Ctx)'Old
          and Cursor (Ctx, F_Kind) = Cursor (Ctx, F_Kind)'Old
          and Cursor (Ctx, F_Arity) = Cursor (Ctx, F_Arity)'Old
          and Cursor (Ctx, F_Tag) = Cursor (Ctx, F_Tag)'Old
          and Cursor (Ctx, F_Flags) = Cursor (Ctx, F_Flags)'Old;

   procedure Set_Handle (Ctx : in out Context; Val : Protocol.Handle) with
     Pre =>
       Valid_Context (Ctx)
          and then not Ctx'Constrained
          and then Has_Buffer (Ctx)
          and then Valid_Next (Ctx, F_Handle)
          and then Field_Last (Ctx, F_Handle) <= Types.Bit_Index'Last / 2
          and then Field_Condition (Ctx, (F_Handle, Val))
          and then Valid (Val)
          and then Available_Space (Ctx, F_Handle) >= Field_Length (Ctx, F_Handle),
     Post =>
       Valid_Context (Ctx)
          and Has_Buffer (Ctx)
          and Valid (Ctx, F_Handle)
          and Get_Handle (Ctx) = Val
          and Invalid (Ctx, F_Cookie)
          and (Predecessor (Ctx, F_Cookie) = F_Handle
            and Valid_Next (Ctx, F_Cookie))
          and Ctx.Buffer_First = Ctx.Buffer_First'Old
          and Ctx.Buffer_Last = Ctx.Buffer_Last'Old
          and Ctx.First = Ctx.First'Old
          and Predecessor (Ctx, F_Handle) = Predecessor (Ctx, F_Handle)'Old
          and Valid_Next (Ctx, F_Handle) = Valid_Next (Ctx, F_Handle)'Old
          and Get_Kind (Ctx) = Get_Kind (Ctx)'Old
          and Get_Arity (Ctx) = Get_Arity (Ctx)'Old
          and Get_Tag (Ctx) = Get_Tag (Ctx)'Old
          and Get_Flags (Ctx) = Get_Flags (Ctx)'Old
          and Cursor (Ctx, F_Kind) = Cursor (Ctx, F_Kind)'Old
          and Cursor (Ctx, F_Arity) = Cursor (Ctx, F_Arity)'Old
          and Cursor (Ctx, F_Tag) = Cursor (Ctx, F_Tag)'Old
          and Cursor (Ctx, F_Flags) = Cursor (Ctx, F_Flags)'Old
          and Cursor (Ctx, F_Binder) = Cursor (Ctx, F_Binder)'Old;

   procedure Set_Cookie (Ctx : in out Context; Val : Protocol.Cookie) with
     Pre =>
       Valid_Context (Ctx)
          and then not Ctx'Constrained
          and then Has_Buffer (Ctx)
          and then Valid_Next (Ctx, F_Cookie)
          and then Field_Last (Ctx, F_Cookie) <= Types.Bit_Index'Last / 2
          and then Field_Condition (Ctx, (F_Cookie, Val))
          and then Valid (Val)
          and then Available_Space (Ctx, F_Cookie) >= Field_Length (Ctx, F_Cookie),
     Post =>
       Valid_Context (Ctx)
          and Has_Buffer (Ctx)
          and Valid (Ctx, F_Cookie)
          and Get_Cookie (Ctx) = Val
          and Ctx.Buffer_First = Ctx.Buffer_First'Old
          and Ctx.Buffer_Last = Ctx.Buffer_Last'Old
          and Ctx.First = Ctx.First'Old
          and Predecessor (Ctx, F_Cookie) = Predecessor (Ctx, F_Cookie)'Old
          and Valid_Next (Ctx, F_Cookie) = Valid_Next (Ctx, F_Cookie)'Old
          and Get_Kind (Ctx) = Get_Kind (Ctx)'Old
          and Get_Arity (Ctx) = Get_Arity (Ctx)'Old
          and Get_Tag (Ctx) = Get_Tag (Ctx)'Old
          and Get_Flags (Ctx) = Get_Flags (Ctx)'Old
          and Cursor (Ctx, F_Kind) = Cursor (Ctx, F_Kind)'Old
          and Cursor (Ctx, F_Arity) = Cursor (Ctx, F_Arity)'Old
          and Cursor (Ctx, F_Tag) = Cursor (Ctx, F_Tag)'Old
          and Cursor (Ctx, F_Flags) = Cursor (Ctx, F_Flags)'Old
          and Cursor (Ctx, F_Binder) = Cursor (Ctx, F_Binder)'Old
          and Cursor (Ctx, F_Handle) = Cursor (Ctx, F_Handle)'Old;

   function Valid_Context (Ctx : Context) return Boolean with
     Annotate =>
       (GNATprove, Inline_For_Proof),
     Ghost;

   function Cursor (Ctx : Context; Fld : Field) return Field_Cursor with
     Annotate =>
       (GNATprove, Inline_For_Proof),
     Ghost;

   function Cursors (Ctx : Context) return Field_Cursors with
     Annotate =>
       (GNATprove, Inline_For_Proof),
     Ghost;

private

   type Cursor_State is (S_Valid, S_Structural_Valid, S_Invalid, S_Incomplete);

   function Valid_Value (Val : Field_Dependent_Value) return Boolean is
     ((case Val.Fld is
         when F_Kind =>
            Valid (Val.Kind_Value),
         when F_Arity =>
            Valid (Val.Arity_Value),
         when F_Tag =>
            Valid (Val.Tag_Value),
         when F_Flags =>
            Valid (Val.Flags_Value),
         when F_Binder =>
            Valid (Val.Binder_Value),
         when F_Handle =>
            Valid (Val.Handle_Value),
         when F_Cookie =>
            Valid (Val.Cookie_Value),
         when F_Initial | F_Final =>
            False));

   type Field_Cursor (State : Cursor_State := S_Invalid) is
      record
         Predecessor : Virtual_Field := F_Final;
         case State is
            when S_Valid | S_Structural_Valid =>
               First : Types.Bit_Index := Types.Bit_Index'First;
               Last : Types.Bit_Length := Types.Bit_Length'First;
               Value : Field_Dependent_Value := (Fld => F_Final);
            when S_Invalid | S_Incomplete =>
               null;
         end case;
      end record with
     Dynamic_Predicate =>
       (if State = S_Valid
             or State = S_Structural_Valid then
           Valid_Value (Field_Cursor.Value));

   function Structural_Valid (Cursor : Field_Cursor) return Boolean is
     (Cursor.State = S_Valid
      or Cursor.State = S_Structural_Valid);

   function Valid (Cursor : Field_Cursor) return Boolean is
     (Cursor.State = S_Valid);

   function Invalid (Cursor : Field_Cursor) return Boolean is
     (Cursor.State = S_Invalid
      or Cursor.State = S_Incomplete);

   pragma Warnings (Off, """Buffer"" is not modified, could be of access constant type");

   function Valid_Context (Buffer_First, Buffer_Last : Types.Index; First, Last : Types.Bit_Index; Buffer : Types.Bytes_Ptr; Cursors : Field_Cursors) return Boolean is
     ((if Buffer /= null then
         Buffer'First = Buffer_First
           and Buffer'Last = Buffer_Last)
      and then Types.Byte_Index (First) >= Buffer_First
      and then Types.Byte_Index (Last) <= Buffer_Last
      and then First <= Last
      and then Last <= Types.Bit_Index'Last / 2
      and then (for all F in Field'First .. Field'Last =>
        (if Structural_Valid (Cursors (F)) then
         Cursors (F).First >= First
           and Cursors (F).Last <= Last
           and Cursors (F).First <= (Cursors (F).Last + 1)
           and Cursors (F).Value.Fld = F))
      and then ((if Structural_Valid (Cursors (F_Arity)) then
           (Valid (Cursors (F_Kind))
               and then Cursors (F_Arity).Predecessor = F_Kind))
        and then (if Structural_Valid (Cursors (F_Tag)) then
           (Valid (Cursors (F_Arity))
               and then Cursors (F_Tag).Predecessor = F_Arity
               and then (Types.Bit_Length (Cursors (F_Arity).Value.Arity_Value) = Types.Bit_Length (Convert (BA_SINGLE))
                 or (Types.Bit_Length (Cursors (F_Kind).Value.Kind_Value) = Types.Bit_Length (Convert (BK_FD))
                   and Types.Bit_Length (Cursors (F_Arity).Value.Arity_Value) = Types.Bit_Length (Convert (BA_ARRAY))))))
        and then (if Structural_Valid (Cursors (F_Flags)) then
           (Valid (Cursors (F_Tag))
               and then Cursors (F_Flags).Predecessor = F_Tag))
        and then (if Structural_Valid (Cursors (F_Binder)) then
           (Valid (Cursors (F_Flags))
               and then Cursors (F_Binder).Predecessor = F_Flags
               and then (Types.Bit_Length (Cursors (F_Kind).Value.Kind_Value) = Types.Bit_Length (Convert (BK_STRONG_BINDER))
                 or Types.Bit_Length (Cursors (F_Kind).Value.Kind_Value) = Types.Bit_Length (Convert (BK_WEAK_BINDER)))))
        and then (if Structural_Valid (Cursors (F_Handle)) then
           (Valid (Cursors (F_Flags))
               and then Cursors (F_Handle).Predecessor = F_Flags
               and then (Types.Bit_Length (Cursors (F_Kind).Value.Kind_Value) = Types.Bit_Length (Convert (BK_STRONG_HANDLE))
                 or Types.Bit_Length (Cursors (F_Kind).Value.Kind_Value) = Types.Bit_Length (Convert (BK_WEAK_HANDLE)))))
        and then (if Structural_Valid (Cursors (F_Cookie)) then
           (Valid (Cursors (F_Binder))
               and then Cursors (F_Cookie).Predecessor = F_Binder)
             or (Valid (Cursors (F_Handle))
               and then Cursors (F_Cookie).Predecessor = F_Handle)))
      and then ((if Invalid (Cursors (F_Kind)) then
           Invalid (Cursors (F_Arity)))
        and then (if Invalid (Cursors (F_Arity)) then
           Invalid (Cursors (F_Tag)))
        and then (if Invalid (Cursors (F_Tag)) then
           Invalid (Cursors (F_Flags)))
        and then (if Invalid (Cursors (F_Flags)) then
           Invalid (Cursors (F_Binder)))
        and then (if Invalid (Cursors (F_Flags)) then
           Invalid (Cursors (F_Handle)))
        and then (if Invalid (Cursors (F_Binder))
             and then Invalid (Cursors (F_Handle)) then
           Invalid (Cursors (F_Cookie))))
      and then (if Structural_Valid (Cursors (F_Kind)) then
         (Cursors (F_Kind).Last - Cursors (F_Kind).First + 1) = Protocol.Binder_Kind_Base'Size
           and then Cursors (F_Kind).Predecessor = F_Initial
           and then Cursors (F_Kind).First = First
           and then (if Structural_Valid (Cursors (F_Arity)) then
              (Cursors (F_Arity).Last - Cursors (F_Arity).First + 1) = Protocol.Binder_Arity_Base'Size
                and then Cursors (F_Arity).Predecessor = F_Kind
                and then Cursors (F_Arity).First = (Cursors (F_Kind).Last + 1)
                and then (if Structural_Valid (Cursors (F_Tag))
                     and then (Types.Bit_Length (Cursors (F_Arity).Value.Arity_Value) = Types.Bit_Length (Convert (BA_SINGLE))
                       or (Types.Bit_Length (Cursors (F_Kind).Value.Kind_Value) = Types.Bit_Length (Convert (BK_FD))
                         and Types.Bit_Length (Cursors (F_Arity).Value.Arity_Value) = Types.Bit_Length (Convert (BA_ARRAY)))) then
                   (Cursors (F_Tag).Last - Cursors (F_Tag).First + 1) = Protocol.Binder_Tag_Base'Size
                     and then Cursors (F_Tag).Predecessor = F_Arity
                     and then Cursors (F_Tag).First = (Cursors (F_Arity).Last + 1)
                     and then (if Structural_Valid (Cursors (F_Flags)) then
                        (Cursors (F_Flags).Last - Cursors (F_Flags).First + 1) = Protocol.Flat_Binder_Flags_Base'Size
                          and then Cursors (F_Flags).Predecessor = F_Tag
                          and then Cursors (F_Flags).First = (Cursors (F_Tag).Last + 1)
                          and then (if Structural_Valid (Cursors (F_Binder))
                               and then (Types.Bit_Length (Cursors (F_Kind).Value.Kind_Value) = Types.Bit_Length (Convert (BK_STRONG_BINDER))
                                 or Types.Bit_Length (Cursors (F_Kind).Value.Kind_Value) = Types.Bit_Length (Convert (BK_WEAK_BINDER))) then
                             (Cursors (F_Binder).Last - Cursors (F_Binder).First + 1) = Protocol.Binder'Size
                               and then Cursors (F_Binder).Predecessor = F_Flags
                               and then Cursors (F_Binder).First = (Cursors (F_Flags).Last + 1)
                               and then (if Structural_Valid (Cursors (F_Cookie)) then
                                  (Cursors (F_Cookie).Last - Cursors (F_Cookie).First + 1) = Protocol.Cookie'Size
                                    and then Cursors (F_Cookie).Predecessor = F_Binder
                                    and then Cursors (F_Cookie).First = (Cursors (F_Binder).Last + 1)))
                          and then (if Structural_Valid (Cursors (F_Handle))
                               and then (Types.Bit_Length (Cursors (F_Kind).Value.Kind_Value) = Types.Bit_Length (Convert (BK_STRONG_HANDLE))
                                 or Types.Bit_Length (Cursors (F_Kind).Value.Kind_Value) = Types.Bit_Length (Convert (BK_WEAK_HANDLE))) then
                             (Cursors (F_Handle).Last - Cursors (F_Handle).First + 1) = Protocol.Handle_Base'Size
                               and then Cursors (F_Handle).Predecessor = F_Flags
                               and then Cursors (F_Handle).First = (Cursors (F_Flags).Last + 1)
                               and then (if Structural_Valid (Cursors (F_Cookie)) then
                                  (Cursors (F_Cookie).Last - Cursors (F_Cookie).First + 1) = Protocol.Cookie'Size
                                    and then Cursors (F_Cookie).Predecessor = F_Handle
                                    and then Cursors (F_Cookie).First = (Cursors (F_Handle).Last + 1))))))));

   type Context (Buffer_First, Buffer_Last : Types.Index := Types.Index'First; First, Last : Types.Bit_Index := Types.Bit_Index'First) is
      record
         Buffer : Types.Bytes_Ptr := null;
         Cursors : Field_Cursors := (others => (State => S_Invalid, Predecessor => F_Final));
      end record with
     Dynamic_Predicate =>
       Valid_Context (Context.Buffer_First, Context.Buffer_Last, Context.First, Context.Last, Context.Buffer, Context.Cursors);

   function Valid_Context (Ctx : Context) return Boolean is
     (Valid_Context (Ctx.Buffer_First, Ctx.Buffer_Last, Ctx.First, Ctx.Last, Ctx.Buffer, Ctx.Cursors));

   function Cursor (Ctx : Context; Fld : Field) return Field_Cursor is
     (Ctx.Cursors (Fld));

   function Cursors (Ctx : Context) return Field_Cursors is
     (Ctx.Cursors);

end Parpen.Protocol.Generic_IBinder;
