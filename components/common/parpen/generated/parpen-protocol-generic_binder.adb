package body Parpen.Protocol.Generic_Binder with
  SPARK_Mode
is

   function Create return Context is
     ((Types.Index'First, Types.Index'First, Types.Bit_Index'First, Types.Bit_Index'First, null, (F_Guarantee => (State => S_Invalid, Predecessor => F_Initial), others => (State => S_Invalid, Predecessor => F_Final))));

   procedure Initialize (Ctx : out Context; Buffer : in out Types.Bytes_Ptr) is
   begin
      Initialize (Ctx, Buffer, Types.First_Bit_Index (Buffer'First), Types.Last_Bit_Index (Buffer'Last));
   end Initialize;

   procedure Initialize (Ctx : out Context; Buffer : in out Types.Bytes_Ptr; First, Last : Types.Bit_Index) is
      Buffer_First : constant Types.Index := Buffer'First;
      Buffer_Last : constant Types.Index := Buffer'Last;
   begin
      Ctx := (Buffer_First, Buffer_Last, First, Last, Buffer, (F_Guarantee => (State => S_Invalid, Predecessor => F_Initial), others => (State => S_Invalid, Predecessor => F_Final)));
      Buffer := null;
   end Initialize;

   function Initialized (Ctx : Context) return Boolean is
     (Valid_Next (Ctx, F_Guarantee)
      and then Available_Space (Ctx, F_Guarantee) = (Types.Last_Bit_Index (Ctx.Buffer_Last) - Ctx.First + 1)
      and then Invalid (Ctx, F_Guarantee)
      and then Invalid (Ctx, F_Typ)
      and then Invalid (Ctx, F_Binder_Tag1)
      and then Invalid (Ctx, F_Binder_Tag2));

   procedure Take_Buffer (Ctx : in out Context; Buffer : out Types.Bytes_Ptr) is
   begin
      Buffer := Ctx.Buffer;
      Ctx.Buffer := null;
   end Take_Buffer;

   function Has_Buffer (Ctx : Context) return Boolean is
     (Ctx.Buffer /= null);

   function Message_Last (Ctx : Context) return Types.Bit_Index is
     ((if Structural_Valid (Ctx.Cursors (F_Binder_Tag2)) then
       Ctx.Cursors (F_Binder_Tag2).Last
    else
       Types.Unreachable_Bit_Length));

   function Path_Condition (Ctx : Context; Fld : Field) return Boolean is
     ((case Ctx.Cursors (Fld).Predecessor is
         when F_Initial =>
            (case Fld is
                  when F_Guarantee =>
                     True,
                  when others =>
                     False),
         when F_Guarantee =>
            (case Fld is
                  when F_Typ =>
                     True,
                  when others =>
                     False),
         when F_Typ =>
            (case Fld is
                  when F_Binder_Tag1 =>
                     True,
                  when others =>
                     False),
         when F_Binder_Tag1 =>
            (case Fld is
                  when F_Binder_Tag2 =>
                     True,
                  when others =>
                     False),
         when F_Binder_Tag2 | F_Final =>
            False));

   function Field_Condition (Ctx : Context; Val : Field_Dependent_Value) return Boolean is
     ((case Val.Fld is
         when F_Initial | F_Guarantee | F_Typ | F_Binder_Tag1 | F_Binder_Tag2 =>
            True,
         when F_Final =>
            False));

   function Field_Length (Ctx : Context; Fld : Field) return Types.Bit_Length is
     ((case Ctx.Cursors (Fld).Predecessor is
         when F_Initial =>
            (case Fld is
                  when F_Guarantee =>
                     Protocol.Binder_Guarantee_Base'Size,
                  when others =>
                     Types.Unreachable_Bit_Length),
         when F_Guarantee =>
            (case Fld is
                  when F_Typ =>
                     Protocol.Binder_Type_Base'Size,
                  when others =>
                     Types.Unreachable_Bit_Length),
         when F_Typ =>
            (case Fld is
                  when F_Binder_Tag1 =>
                     Protocol.Binder_Tag1_Base'Size,
                  when others =>
                     Types.Unreachable_Bit_Length),
         when F_Binder_Tag1 =>
            (case Fld is
                  when F_Binder_Tag2 =>
                     Protocol.Binder_Tag2_Base'Size,
                  when others =>
                     Types.Unreachable_Bit_Length),
         when F_Binder_Tag2 | F_Final =>
            0));

   function Field_First (Ctx : Context; Fld : Field) return Types.Bit_Index is
     ((case Fld is
         when F_Guarantee =>
            Ctx.First,
         when F_Typ =>
            (if Ctx.Cursors (Fld).Predecessor = F_Guarantee then
                (Ctx.Cursors (Ctx.Cursors (Fld).Predecessor).Last + 1)
             else
                Types.Unreachable_Bit_Length),
         when F_Binder_Tag1 =>
            (if Ctx.Cursors (Fld).Predecessor = F_Typ then
                (Ctx.Cursors (Ctx.Cursors (Fld).Predecessor).Last + 1)
             else
                Types.Unreachable_Bit_Length),
         when F_Binder_Tag2 =>
            (if Ctx.Cursors (Fld).Predecessor = F_Binder_Tag1 then
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
         when F_Guarantee =>
            F_Typ,
         when F_Typ =>
            F_Binder_Tag1,
         when F_Binder_Tag1 =>
            F_Binder_Tag2,
         when F_Binder_Tag2 =>
            F_Final))
    with
     Pre =>
       Structural_Valid (Ctx, Fld)
          and Valid_Predecessor (Ctx, Fld);

   function Valid_Predecessor (Ctx : Context; Fld : Virtual_Field) return Boolean is
     ((case Fld is
         when F_Initial =>
            True,
         when F_Guarantee =>
            Ctx.Cursors (Fld).Predecessor = F_Initial,
         when F_Typ =>
            (Valid (Ctx.Cursors (F_Guarantee))
                 and Ctx.Cursors (Fld).Predecessor = F_Guarantee),
         when F_Binder_Tag1 =>
            (Valid (Ctx.Cursors (F_Typ))
                 and Ctx.Cursors (Fld).Predecessor = F_Typ),
         when F_Binder_Tag2 =>
            (Valid (Ctx.Cursors (F_Binder_Tag1))
                 and Ctx.Cursors (Fld).Predecessor = F_Binder_Tag1),
         when F_Final =>
            (Valid (Ctx.Cursors (F_Binder_Tag2))
                 and Ctx.Cursors (Fld).Predecessor = F_Binder_Tag2)));

   function Invalid_Successor (Ctx : Context; Fld : Field) return Boolean is
     ((case Fld is
         when F_Guarantee =>
            Invalid (Ctx.Cursors (F_Typ)),
         when F_Typ =>
            Invalid (Ctx.Cursors (F_Binder_Tag1)),
         when F_Binder_Tag1 =>
            Invalid (Ctx.Cursors (F_Binder_Tag2)),
         when F_Binder_Tag2 =>
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
               when F_Guarantee =>
                  Invalid (Ctx, F_Guarantee)
                     and Invalid (Ctx, F_Typ)
                     and Invalid (Ctx, F_Binder_Tag1)
                     and Invalid (Ctx, F_Binder_Tag2),
               when F_Typ =>
                  Ctx.Cursors (F_Guarantee) = Ctx.Cursors (F_Guarantee)'Old
                     and Invalid (Ctx, F_Typ)
                     and Invalid (Ctx, F_Binder_Tag1)
                     and Invalid (Ctx, F_Binder_Tag2),
               when F_Binder_Tag1 =>
                  Ctx.Cursors (F_Guarantee) = Ctx.Cursors (F_Guarantee)'Old
                     and Ctx.Cursors (F_Typ) = Ctx.Cursors (F_Typ)'Old
                     and Invalid (Ctx, F_Binder_Tag1)
                     and Invalid (Ctx, F_Binder_Tag2),
               when F_Binder_Tag2 =>
                  Ctx.Cursors (F_Guarantee) = Ctx.Cursors (F_Guarantee)'Old
                     and Ctx.Cursors (F_Typ) = Ctx.Cursors (F_Typ)'Old
                     and Ctx.Cursors (F_Binder_Tag1) = Ctx.Cursors (F_Binder_Tag1)'Old
                     and Invalid (Ctx, F_Binder_Tag2))
   is
      First : constant Types.Bit_Length := Field_First (Ctx, Fld) with
        Ghost;
      Length : constant Types.Bit_Length := Field_Length (Ctx, Fld) with
        Ghost;
   begin
      pragma Assert (Field_First (Ctx, Fld) = First
         and Field_Length (Ctx, Fld) = Length);
      case Fld is
         when F_Guarantee =>
            Ctx.Cursors (F_Binder_Tag2) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Binder_Tag1) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Typ) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Guarantee) := (S_Invalid, Ctx.Cursors (F_Guarantee).Predecessor);
            pragma Assert (Field_First (Ctx, Fld) = First
               and Field_Length (Ctx, Fld) = Length);
         when F_Typ =>
            Ctx.Cursors (F_Binder_Tag2) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Binder_Tag1) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Typ) := (S_Invalid, Ctx.Cursors (F_Typ).Predecessor);
            pragma Assert (Field_First (Ctx, Fld) = First
               and Field_Length (Ctx, Fld) = Length);
         when F_Binder_Tag1 =>
            Ctx.Cursors (F_Binder_Tag2) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Binder_Tag1) := (S_Invalid, Ctx.Cursors (F_Binder_Tag1).Predecessor);
            pragma Assert (Field_First (Ctx, Fld) = First
               and Field_Length (Ctx, Fld) = Length);
         when F_Binder_Tag2 =>
            Ctx.Cursors (F_Binder_Tag2) := (S_Invalid, Ctx.Cursors (F_Binder_Tag2).Predecessor);
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
         when F_Guarantee | F_Typ | F_Binder_Tag1 | F_Binder_Tag2 =>
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
      function Extract is new Types.Extract (Protocol.Binder_Guarantee_Base);
      function Extract is new Types.Extract (Protocol.Binder_Type_Base);
      function Extract is new Types.Extract (Protocol.Binder_Tag1_Base);
      function Extract is new Types.Extract (Protocol.Binder_Tag2_Base);
   begin
      return ((case Fld is
            when F_Guarantee =>
               (Fld => F_Guarantee, Guarantee_Value => Extract (Ctx.Buffer.all (Buffer_First .. Buffer_Last), Offset)),
            when F_Typ =>
               (Fld => F_Typ, Typ_Value => Extract (Ctx.Buffer.all (Buffer_First .. Buffer_Last), Offset)),
            when F_Binder_Tag1 =>
               (Fld => F_Binder_Tag1, Binder_Tag1_Value => Extract (Ctx.Buffer.all (Buffer_First .. Buffer_Last), Offset)),
            when F_Binder_Tag2 =>
               (Fld => F_Binder_Tag2, Binder_Tag2_Value => Extract (Ctx.Buffer.all (Buffer_First .. Buffer_Last), Offset))));
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
               pragma Assert ((if Structural_Valid (Ctx.Cursors (F_Guarantee)) then
                   (Ctx.Cursors (F_Guarantee).Last - Ctx.Cursors (F_Guarantee).First + 1) = Protocol.Binder_Guarantee_Base'Size
                     and then Ctx.Cursors (F_Guarantee).Predecessor = F_Initial
                     and then Ctx.Cursors (F_Guarantee).First = Ctx.First
                     and then (if Structural_Valid (Ctx.Cursors (F_Typ)) then
                        (Ctx.Cursors (F_Typ).Last - Ctx.Cursors (F_Typ).First + 1) = Protocol.Binder_Type_Base'Size
                          and then Ctx.Cursors (F_Typ).Predecessor = F_Guarantee
                          and then Ctx.Cursors (F_Typ).First = (Ctx.Cursors (F_Guarantee).Last + 1)
                          and then (if Structural_Valid (Ctx.Cursors (F_Binder_Tag1)) then
                             (Ctx.Cursors (F_Binder_Tag1).Last - Ctx.Cursors (F_Binder_Tag1).First + 1) = Protocol.Binder_Tag1_Base'Size
                               and then Ctx.Cursors (F_Binder_Tag1).Predecessor = F_Typ
                               and then Ctx.Cursors (F_Binder_Tag1).First = (Ctx.Cursors (F_Typ).Last + 1)
                               and then (if Structural_Valid (Ctx.Cursors (F_Binder_Tag2)) then
                                  (Ctx.Cursors (F_Binder_Tag2).Last - Ctx.Cursors (F_Binder_Tag2).First + 1) = Protocol.Binder_Tag2_Base'Size
                                    and then Ctx.Cursors (F_Binder_Tag2).Predecessor = F_Binder_Tag1
                                    and then Ctx.Cursors (F_Binder_Tag2).First = (Ctx.Cursors (F_Binder_Tag1).Last + 1))))));
               if Fld = F_Guarantee then
                  Ctx.Cursors (Successor (Ctx, Fld)) := (State => S_Invalid, Predecessor => Fld);
               elsif Fld = F_Typ then
                  Ctx.Cursors (Successor (Ctx, Fld)) := (State => S_Invalid, Predecessor => Fld);
               elsif Fld = F_Binder_Tag1 then
                  Ctx.Cursors (Successor (Ctx, Fld)) := (State => S_Invalid, Predecessor => Fld);
               elsif Fld = F_Binder_Tag2 then
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
      Verify (Ctx, F_Guarantee);
      Verify (Ctx, F_Typ);
      Verify (Ctx, F_Binder_Tag1);
      Verify (Ctx, F_Binder_Tag2);
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
     (Valid (Ctx, F_Guarantee)
      and then Valid (Ctx, F_Typ)
      and then Valid (Ctx, F_Binder_Tag1)
      and then Valid (Ctx, F_Binder_Tag2));

   function Valid_Message (Ctx : Context) return Boolean is
     (Valid (Ctx, F_Guarantee)
      and then Valid (Ctx, F_Typ)
      and then Valid (Ctx, F_Binder_Tag1)
      and then Valid (Ctx, F_Binder_Tag2));

   function Incomplete_Message (Ctx : Context) return Boolean is
     (Incomplete (Ctx, F_Guarantee)
      or Incomplete (Ctx, F_Typ)
      or Incomplete (Ctx, F_Binder_Tag1)
      or Incomplete (Ctx, F_Binder_Tag2));

   function Get_Guarantee (Ctx : Context) return Protocol.Binder_Guarantee is
     (Convert (Ctx.Cursors (F_Guarantee).Value.Guarantee_Value));

   function Get_Typ (Ctx : Context) return Protocol.Binder_Type is
     (Convert (Ctx.Cursors (F_Typ).Value.Typ_Value));

   function Get_Binder_Tag1 (Ctx : Context) return Protocol.Binder_Tag1 is
     (Ctx.Cursors (F_Binder_Tag1).Value.Binder_Tag1_Value);

   function Get_Binder_Tag2 (Ctx : Context) return Protocol.Binder_Tag2 is
     (Ctx.Cursors (F_Binder_Tag2).Value.Binder_Tag2_Value);

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
      procedure Insert is new Types.Insert (Protocol.Binder_Guarantee_Base);
      procedure Insert is new Types.Insert (Protocol.Binder_Type_Base);
      procedure Insert is new Types.Insert (Protocol.Binder_Tag1_Base);
      procedure Insert is new Types.Insert (Protocol.Binder_Tag2_Base);
   begin
      Fst := First;
      Lst := Last;
      case Val.Fld is
         when F_Initial =>
            null;
         when F_Guarantee =>
            Insert (Val.Guarantee_Value, Ctx.Buffer.all (Buffer_First .. Buffer_Last), Offset);
         when F_Typ =>
            Insert (Val.Typ_Value, Ctx.Buffer.all (Buffer_First .. Buffer_Last), Offset);
         when F_Binder_Tag1 =>
            Insert (Val.Binder_Tag1_Value, Ctx.Buffer.all (Buffer_First .. Buffer_Last), Offset);
         when F_Binder_Tag2 =>
            Insert (Val.Binder_Tag2_Value, Ctx.Buffer.all (Buffer_First .. Buffer_Last), Offset);
         when F_Final =>
            null;
      end case;
   end Set_Field_Value;

   procedure Set_Guarantee (Ctx : in out Context; Val : Protocol.Binder_Guarantee) is
      Field_Value : constant Field_Dependent_Value := (F_Guarantee, Convert (Val));
      First, Last : Types.Bit_Index;
   begin
      Reset_Dependent_Fields (Ctx, F_Guarantee);
      Set_Field_Value (Ctx, Field_Value, First, Last);
      Ctx := (Ctx.Buffer_First, Ctx.Buffer_Last, Ctx.First, Last, Ctx.Buffer, Ctx.Cursors);
      Ctx.Cursors (F_Guarantee) := (State => S_Valid, First => First, Last => Last, Value => Field_Value, Predecessor => Ctx.Cursors (F_Guarantee).Predecessor);
      Ctx.Cursors (Successor (Ctx, F_Guarantee)) := (State => S_Invalid, Predecessor => F_Guarantee);
   end Set_Guarantee;

   procedure Set_Typ (Ctx : in out Context; Val : Protocol.Binder_Type) is
      Field_Value : constant Field_Dependent_Value := (F_Typ, Convert (Val));
      First, Last : Types.Bit_Index;
   begin
      Reset_Dependent_Fields (Ctx, F_Typ);
      Set_Field_Value (Ctx, Field_Value, First, Last);
      Ctx := (Ctx.Buffer_First, Ctx.Buffer_Last, Ctx.First, Last, Ctx.Buffer, Ctx.Cursors);
      Ctx.Cursors (F_Typ) := (State => S_Valid, First => First, Last => Last, Value => Field_Value, Predecessor => Ctx.Cursors (F_Typ).Predecessor);
      Ctx.Cursors (Successor (Ctx, F_Typ)) := (State => S_Invalid, Predecessor => F_Typ);
   end Set_Typ;

   procedure Set_Binder_Tag1 (Ctx : in out Context; Val : Protocol.Binder_Tag1) is
      Field_Value : constant Field_Dependent_Value := (F_Binder_Tag1, Val);
      First, Last : Types.Bit_Index;
   begin
      Reset_Dependent_Fields (Ctx, F_Binder_Tag1);
      Set_Field_Value (Ctx, Field_Value, First, Last);
      Ctx := (Ctx.Buffer_First, Ctx.Buffer_Last, Ctx.First, Last, Ctx.Buffer, Ctx.Cursors);
      Ctx.Cursors (F_Binder_Tag1) := (State => S_Valid, First => First, Last => Last, Value => Field_Value, Predecessor => Ctx.Cursors (F_Binder_Tag1).Predecessor);
      Ctx.Cursors (Successor (Ctx, F_Binder_Tag1)) := (State => S_Invalid, Predecessor => F_Binder_Tag1);
   end Set_Binder_Tag1;

   procedure Set_Binder_Tag2 (Ctx : in out Context; Val : Protocol.Binder_Tag2) is
      Field_Value : constant Field_Dependent_Value := (F_Binder_Tag2, Val);
      First, Last : Types.Bit_Index;
   begin
      Reset_Dependent_Fields (Ctx, F_Binder_Tag2);
      Set_Field_Value (Ctx, Field_Value, First, Last);
      Ctx := (Ctx.Buffer_First, Ctx.Buffer_Last, Ctx.First, Last, Ctx.Buffer, Ctx.Cursors);
      Ctx.Cursors (F_Binder_Tag2) := (State => S_Valid, First => First, Last => Last, Value => Field_Value, Predecessor => Ctx.Cursors (F_Binder_Tag2).Predecessor);
      Ctx.Cursors (Successor (Ctx, F_Binder_Tag2)) := (State => S_Invalid, Predecessor => F_Binder_Tag2);
   end Set_Binder_Tag2;

end Parpen.Protocol.Generic_Binder;
