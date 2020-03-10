with Parpen.Generic_Types;

generic
   with package Types is new Parpen.Generic_Types (<>);
package Parpen.Protocol.Generic_Binder with
  SPARK_Mode
is

   pragma Annotate (GNATprove, Terminating, Generic_Binder);

   use type Types.Bytes, Types.Bytes_Ptr, Types.Index, Types.Length, Types.Bit_Index, Types.Bit_Length;

   type Virtual_Field is (F_Initial, F_Guarantee, F_Typ, F_Binder_Tag1, F_Binder_Tag2, F_Final);

   subtype Field is Virtual_Field range F_Guarantee .. F_Binder_Tag2;

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
            when F_Guarantee =>
               Guarantee_Value : Protocol.Binder_Guarantee_Base;
            when F_Typ =>
               Typ_Value : Protocol.Binder_Type_Base;
            when F_Binder_Tag1 =>
               Binder_Tag1_Value : Protocol.Binder_Tag1_Base;
            when F_Binder_Tag2 =>
               Binder_Tag2_Value : Protocol.Binder_Tag2_Base;
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

   function Get_Guarantee (Ctx : Context) return Protocol.Binder_Guarantee with
     Pre =>
       Valid_Context (Ctx)
          and Valid (Ctx, F_Guarantee);

   function Get_Typ (Ctx : Context) return Protocol.Binder_Type with
     Pre =>
       Valid_Context (Ctx)
          and Valid (Ctx, F_Typ);

   function Get_Binder_Tag1 (Ctx : Context) return Protocol.Binder_Tag1 with
     Pre =>
       Valid_Context (Ctx)
          and Valid (Ctx, F_Binder_Tag1);

   function Get_Binder_Tag2 (Ctx : Context) return Protocol.Binder_Tag2 with
     Pre =>
       Valid_Context (Ctx)
          and Valid (Ctx, F_Binder_Tag2);

   procedure Set_Guarantee (Ctx : in out Context; Val : Protocol.Binder_Guarantee) with
     Pre =>
       Valid_Context (Ctx)
          and then not Ctx'Constrained
          and then Has_Buffer (Ctx)
          and then Valid_Next (Ctx, F_Guarantee)
          and then Field_Last (Ctx, F_Guarantee) <= Types.Bit_Index'Last / 2
          and then Field_Condition (Ctx, (F_Guarantee, Convert (Val)))
          and then True
          and then Available_Space (Ctx, F_Guarantee) >= Field_Length (Ctx, F_Guarantee),
     Post =>
       Valid_Context (Ctx)
          and Has_Buffer (Ctx)
          and Valid (Ctx, F_Guarantee)
          and Get_Guarantee (Ctx) = Val
          and Invalid (Ctx, F_Typ)
          and Invalid (Ctx, F_Binder_Tag1)
          and Invalid (Ctx, F_Binder_Tag2)
          and (Predecessor (Ctx, F_Typ) = F_Guarantee
            and Valid_Next (Ctx, F_Typ))
          and Ctx.Buffer_First = Ctx.Buffer_First'Old
          and Ctx.Buffer_Last = Ctx.Buffer_Last'Old
          and Ctx.First = Ctx.First'Old
          and Predecessor (Ctx, F_Guarantee) = Predecessor (Ctx, F_Guarantee)'Old
          and Valid_Next (Ctx, F_Guarantee) = Valid_Next (Ctx, F_Guarantee)'Old;

   procedure Set_Typ (Ctx : in out Context; Val : Protocol.Binder_Type) with
     Pre =>
       Valid_Context (Ctx)
          and then not Ctx'Constrained
          and then Has_Buffer (Ctx)
          and then Valid_Next (Ctx, F_Typ)
          and then Field_Last (Ctx, F_Typ) <= Types.Bit_Index'Last / 2
          and then Field_Condition (Ctx, (F_Typ, Convert (Val)))
          and then True
          and then Available_Space (Ctx, F_Typ) >= Field_Length (Ctx, F_Typ),
     Post =>
       Valid_Context (Ctx)
          and Has_Buffer (Ctx)
          and Valid (Ctx, F_Typ)
          and Get_Typ (Ctx) = Val
          and Invalid (Ctx, F_Binder_Tag1)
          and Invalid (Ctx, F_Binder_Tag2)
          and (Predecessor (Ctx, F_Binder_Tag1) = F_Typ
            and Valid_Next (Ctx, F_Binder_Tag1))
          and Ctx.Buffer_First = Ctx.Buffer_First'Old
          and Ctx.Buffer_Last = Ctx.Buffer_Last'Old
          and Ctx.First = Ctx.First'Old
          and Predecessor (Ctx, F_Typ) = Predecessor (Ctx, F_Typ)'Old
          and Valid_Next (Ctx, F_Typ) = Valid_Next (Ctx, F_Typ)'Old
          and Get_Guarantee (Ctx) = Get_Guarantee (Ctx)'Old
          and Cursor (Ctx, F_Guarantee) = Cursor (Ctx, F_Guarantee)'Old;

   procedure Set_Binder_Tag1 (Ctx : in out Context; Val : Protocol.Binder_Tag1) with
     Pre =>
       Valid_Context (Ctx)
          and then not Ctx'Constrained
          and then Has_Buffer (Ctx)
          and then Valid_Next (Ctx, F_Binder_Tag1)
          and then Field_Last (Ctx, F_Binder_Tag1) <= Types.Bit_Index'Last / 2
          and then Field_Condition (Ctx, (F_Binder_Tag1, Val))
          and then Valid (Val)
          and then Available_Space (Ctx, F_Binder_Tag1) >= Field_Length (Ctx, F_Binder_Tag1),
     Post =>
       Valid_Context (Ctx)
          and Has_Buffer (Ctx)
          and Valid (Ctx, F_Binder_Tag1)
          and Get_Binder_Tag1 (Ctx) = Val
          and Invalid (Ctx, F_Binder_Tag2)
          and (Predecessor (Ctx, F_Binder_Tag2) = F_Binder_Tag1
            and Valid_Next (Ctx, F_Binder_Tag2))
          and Ctx.Buffer_First = Ctx.Buffer_First'Old
          and Ctx.Buffer_Last = Ctx.Buffer_Last'Old
          and Ctx.First = Ctx.First'Old
          and Predecessor (Ctx, F_Binder_Tag1) = Predecessor (Ctx, F_Binder_Tag1)'Old
          and Valid_Next (Ctx, F_Binder_Tag1) = Valid_Next (Ctx, F_Binder_Tag1)'Old
          and Get_Guarantee (Ctx) = Get_Guarantee (Ctx)'Old
          and Get_Typ (Ctx) = Get_Typ (Ctx)'Old
          and Cursor (Ctx, F_Guarantee) = Cursor (Ctx, F_Guarantee)'Old
          and Cursor (Ctx, F_Typ) = Cursor (Ctx, F_Typ)'Old;

   procedure Set_Binder_Tag2 (Ctx : in out Context; Val : Protocol.Binder_Tag2) with
     Pre =>
       Valid_Context (Ctx)
          and then not Ctx'Constrained
          and then Has_Buffer (Ctx)
          and then Valid_Next (Ctx, F_Binder_Tag2)
          and then Field_Last (Ctx, F_Binder_Tag2) <= Types.Bit_Index'Last / 2
          and then Field_Condition (Ctx, (F_Binder_Tag2, Val))
          and then Valid (Val)
          and then Available_Space (Ctx, F_Binder_Tag2) >= Field_Length (Ctx, F_Binder_Tag2),
     Post =>
       Valid_Context (Ctx)
          and Has_Buffer (Ctx)
          and Valid (Ctx, F_Binder_Tag2)
          and Get_Binder_Tag2 (Ctx) = Val
          and Ctx.Buffer_First = Ctx.Buffer_First'Old
          and Ctx.Buffer_Last = Ctx.Buffer_Last'Old
          and Ctx.First = Ctx.First'Old
          and Predecessor (Ctx, F_Binder_Tag2) = Predecessor (Ctx, F_Binder_Tag2)'Old
          and Valid_Next (Ctx, F_Binder_Tag2) = Valid_Next (Ctx, F_Binder_Tag2)'Old
          and Get_Guarantee (Ctx) = Get_Guarantee (Ctx)'Old
          and Get_Typ (Ctx) = Get_Typ (Ctx)'Old
          and Get_Binder_Tag1 (Ctx) = Get_Binder_Tag1 (Ctx)'Old
          and Cursor (Ctx, F_Guarantee) = Cursor (Ctx, F_Guarantee)'Old
          and Cursor (Ctx, F_Typ) = Cursor (Ctx, F_Typ)'Old
          and Cursor (Ctx, F_Binder_Tag1) = Cursor (Ctx, F_Binder_Tag1)'Old;

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
         when F_Guarantee =>
            Valid (Val.Guarantee_Value),
         when F_Typ =>
            Valid (Val.Typ_Value),
         when F_Binder_Tag1 =>
            Valid (Val.Binder_Tag1_Value),
         when F_Binder_Tag2 =>
            Valid (Val.Binder_Tag2_Value),
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
      and then ((if Structural_Valid (Cursors (F_Typ)) then
           (Valid (Cursors (F_Guarantee))
               and then Cursors (F_Typ).Predecessor = F_Guarantee))
        and then (if Structural_Valid (Cursors (F_Binder_Tag1)) then
           (Valid (Cursors (F_Typ))
               and then Cursors (F_Binder_Tag1).Predecessor = F_Typ))
        and then (if Structural_Valid (Cursors (F_Binder_Tag2)) then
           (Valid (Cursors (F_Binder_Tag1))
               and then Cursors (F_Binder_Tag2).Predecessor = F_Binder_Tag1)))
      and then ((if Invalid (Cursors (F_Guarantee)) then
           Invalid (Cursors (F_Typ)))
        and then (if Invalid (Cursors (F_Typ)) then
           Invalid (Cursors (F_Binder_Tag1)))
        and then (if Invalid (Cursors (F_Binder_Tag1)) then
           Invalid (Cursors (F_Binder_Tag2))))
      and then (if Structural_Valid (Cursors (F_Guarantee)) then
         (Cursors (F_Guarantee).Last - Cursors (F_Guarantee).First + 1) = Protocol.Binder_Guarantee_Base'Size
           and then Cursors (F_Guarantee).Predecessor = F_Initial
           and then Cursors (F_Guarantee).First = First
           and then (if Structural_Valid (Cursors (F_Typ)) then
              (Cursors (F_Typ).Last - Cursors (F_Typ).First + 1) = Protocol.Binder_Type_Base'Size
                and then Cursors (F_Typ).Predecessor = F_Guarantee
                and then Cursors (F_Typ).First = (Cursors (F_Guarantee).Last + 1)
                and then (if Structural_Valid (Cursors (F_Binder_Tag1)) then
                   (Cursors (F_Binder_Tag1).Last - Cursors (F_Binder_Tag1).First + 1) = Protocol.Binder_Tag1_Base'Size
                     and then Cursors (F_Binder_Tag1).Predecessor = F_Typ
                     and then Cursors (F_Binder_Tag1).First = (Cursors (F_Typ).Last + 1)
                     and then (if Structural_Valid (Cursors (F_Binder_Tag2)) then
                        (Cursors (F_Binder_Tag2).Last - Cursors (F_Binder_Tag2).First + 1) = Protocol.Binder_Tag2_Base'Size
                          and then Cursors (F_Binder_Tag2).Predecessor = F_Binder_Tag1
                          and then Cursors (F_Binder_Tag2).First = (Cursors (F_Binder_Tag1).Last + 1))))));

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

end Parpen.Protocol.Generic_Binder;
