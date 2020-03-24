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
      and then Invalid (Ctx, F_Server)
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
                  when F_Server =>
                     True,
                  when others =>
                     False),
         when F_Server =>
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
         when F_Initial | F_Len | F_Name | F_Server | F_Padding | F_Allow_Isolated | F_Dump_Flags =>
            True,
         when F_Final =>
            False));

   function Field_Length (Ctx : Context; Fld : Field) return Types.Bit_Length is
     ((case Ctx.Cursors (Fld).Predecessor is
         when F_Initial =>
            (case Fld is
                  when F_Len =>
                     Service_Manager.Len'Size,
                  when others =>
                     Types.Unreachable_Bit_Length),
         when F_Len =>
            (case Fld is
                  when F_Name =>
                     Types.Bit_Length (Ctx.Cursors (F_Len).Value.Len_Value) * 8,
                  when others =>
                     Types.Unreachable_Bit_Length),
         when F_Name =>
            (case Fld is
                  when F_Server =>
                     192,
                  when others =>
                     Types.Unreachable_Bit_Length),
         when F_Server =>
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
         when F_Server =>
            (if Ctx.Cursors (Fld).Predecessor = F_Name then
                (Ctx.Cursors (Ctx.Cursors (Fld).Predecessor).Last + 1)
             else
                Types.Unreachable_Bit_Length),
         when F_Padding =>
            (if Ctx.Cursors (Fld).Predecessor = F_Server then
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
            F_Server,
         when F_Server =>
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
         when F_Server =>
            (Structural_Valid (Ctx.Cursors (F_Name))
                 and Ctx.Cursors (Fld).Predecessor = F_Name),
         when F_Padding =>
            (Structural_Valid (Ctx.Cursors (F_Server))
                 and Ctx.Cursors (Fld).Predecessor = F_Server),
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
            Invalid (Ctx.Cursors (F_Server)),
         when F_Server =>
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
                     and Invalid (Ctx, F_Server)
                     and Invalid (Ctx, F_Padding)
                     and Invalid (Ctx, F_Allow_Isolated)
                     and Invalid (Ctx, F_Dump_Flags),
               when F_Name =>
                  Ctx.Cursors (F_Len) = Ctx.Cursors (F_Len)'Old
                     and Invalid (Ctx, F_Name)
                     and Invalid (Ctx, F_Server)
                     and Invalid (Ctx, F_Padding)
                     and Invalid (Ctx, F_Allow_Isolated)
                     and Invalid (Ctx, F_Dump_Flags),
               when F_Server =>
                  Ctx.Cursors (F_Len) = Ctx.Cursors (F_Len)'Old
                     and Ctx.Cursors (F_Name) = Ctx.Cursors (F_Name)'Old
                     and Invalid (Ctx, F_Server)
                     and Invalid (Ctx, F_Padding)
                     and Invalid (Ctx, F_Allow_Isolated)
                     and Invalid (Ctx, F_Dump_Flags),
               when F_Padding =>
                  Ctx.Cursors (F_Len) = Ctx.Cursors (F_Len)'Old
                     and Ctx.Cursors (F_Name) = Ctx.Cursors (F_Name)'Old
                     and Ctx.Cursors (F_Server) = Ctx.Cursors (F_Server)'Old
                     and Invalid (Ctx, F_Padding)
                     and Invalid (Ctx, F_Allow_Isolated)
                     and Invalid (Ctx, F_Dump_Flags),
               when F_Allow_Isolated =>
                  Ctx.Cursors (F_Len) = Ctx.Cursors (F_Len)'Old
                     and Ctx.Cursors (F_Name) = Ctx.Cursors (F_Name)'Old
                     and Ctx.Cursors (F_Server) = Ctx.Cursors (F_Server)'Old
                     and Ctx.Cursors (F_Padding) = Ctx.Cursors (F_Padding)'Old
                     and Invalid (Ctx, F_Allow_Isolated)
                     and Invalid (Ctx, F_Dump_Flags),
               when F_Dump_Flags =>
                  Ctx.Cursors (F_Len) = Ctx.Cursors (F_Len)'Old
                     and Ctx.Cursors (F_Name) = Ctx.Cursors (F_Name)'Old
                     and Ctx.Cursors (F_Server) = Ctx.Cursors (F_Server)'Old
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
            Ctx.Cursors (F_Server) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Name) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Len) := (S_Invalid, Ctx.Cursors (F_Len).Predecessor);
            pragma Assert (Field_First (Ctx, Fld) = First
               and Field_Length (Ctx, Fld) = Length);
         when F_Name =>
            Ctx.Cursors (F_Dump_Flags) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Allow_Isolated) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Padding) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Server) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Name) := (S_Invalid, Ctx.Cursors (F_Name).Predecessor);
            pragma Assert (Field_First (Ctx, Fld) = First
               and Field_Length (Ctx, Fld) = Length);
         when F_Server =>
            Ctx.Cursors (F_Dump_Flags) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Allow_Isolated) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Padding) := (S_Invalid, F_Final);
            Ctx.Cursors (F_Server) := (S_Invalid, Ctx.Cursors (F_Server).Predecessor);
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
         when F_Name | F_Server =>
            True,
         when F_Padding | F_Allow_Isolated | F_Dump_Flags =>
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
      function Extract is new Types.Extract (Service_Manager.Len);
      function Extract is new Types.Extract (Service_Manager.MBZ_7_Base);
      function Extract is new Types.Extract (Builtin_Types.Boolean_Base);
      function Extract is new Types.Extract (Service_Manager.Integer_Base);
   begin
      return ((case Fld is
            when F_Len =>
               (Fld => F_Len, Len_Value => Extract (Ctx.Buffer.all (Buffer_First .. Buffer_Last), Offset)),
            when F_Name =>
               (Fld => F_Name),
            when F_Server =>
               (Fld => F_Server),
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
                   (Ctx.Cursors (F_Len).Last - Ctx.Cursors (F_Len).First + 1) = Service_Manager.Len'Size
                     and then Ctx.Cursors (F_Len).Predecessor = F_Initial
                     and then Ctx.Cursors (F_Len).First = Ctx.First
                     and then (if Structural_Valid (Ctx.Cursors (F_Name)) then
                        (Ctx.Cursors (F_Name).Last - Ctx.Cursors (F_Name).First + 1) = Types.Bit_Length (Ctx.Cursors (F_Len).Value.Len_Value) * 8
                          and then Ctx.Cursors (F_Name).Predecessor = F_Len
                          and then Ctx.Cursors (F_Name).First = (Ctx.Cursors (F_Len).Last + 1)
                          and then (if Structural_Valid (Ctx.Cursors (F_Server)) then
                             (Ctx.Cursors (F_Server).Last - Ctx.Cursors (F_Server).First + 1) = 192
                               and then Ctx.Cursors (F_Server).Predecessor = F_Name
                               and then Ctx.Cursors (F_Server).First = (Ctx.Cursors (F_Name).Last + 1)
                               and then (if Structural_Valid (Ctx.Cursors (F_Padding)) then
                                  (Ctx.Cursors (F_Padding).Last - Ctx.Cursors (F_Padding).First + 1) = Service_Manager.MBZ_7_Base'Size
                                    and then Ctx.Cursors (F_Padding).Predecessor = F_Server
                                    and then Ctx.Cursors (F_Padding).First = (Ctx.Cursors (F_Server).Last + 1)
                                    and then (if Structural_Valid (Ctx.Cursors (F_Allow_Isolated)) then
                                       (Ctx.Cursors (F_Allow_Isolated).Last - Ctx.Cursors (F_Allow_Isolated).First + 1) = Builtin_Types.Boolean_Base'Size
                                         and then Ctx.Cursors (F_Allow_Isolated).Predecessor = F_Padding
                                         and then Ctx.Cursors (F_Allow_Isolated).First = (Ctx.Cursors (F_Padding).Last + 1)
                                         and then (if Structural_Valid (Ctx.Cursors (F_Dump_Flags)) then
                                            (Ctx.Cursors (F_Dump_Flags).Last - Ctx.Cursors (F_Dump_Flags).First + 1) = Service_Manager.Integer_Base'Size
                                              and then Ctx.Cursors (F_Dump_Flags).Predecessor = F_Allow_Isolated
                                              and then Ctx.Cursors (F_Dump_Flags).First = (Ctx.Cursors (F_Allow_Isolated).Last + 1))))))));
               if Fld = F_Len then
                  Ctx.Cursors (Successor (Ctx, Fld)) := (State => S_Invalid, Predecessor => Fld);
               elsif Fld = F_Name then
                  Ctx.Cursors (Successor (Ctx, Fld)) := (State => S_Invalid, Predecessor => Fld);
               elsif Fld = F_Server then
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
      Verify (Ctx, F_Server);
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
      and then Structural_Valid (Ctx, F_Server)
      and then Valid (Ctx, F_Padding)
      and then Valid (Ctx, F_Allow_Isolated)
      and then Valid (Ctx, F_Dump_Flags));

   function Valid_Message (Ctx : Context) return Boolean is
     (Valid (Ctx, F_Len)
      and then Valid (Ctx, F_Name)
      and then Valid (Ctx, F_Server)
      and then Valid (Ctx, F_Padding)
      and then Valid (Ctx, F_Allow_Isolated)
      and then Valid (Ctx, F_Dump_Flags));

   function Incomplete_Message (Ctx : Context) return Boolean is
     (Incomplete (Ctx, F_Len)
      or Incomplete (Ctx, F_Name)
      or Incomplete (Ctx, F_Server)
      or Incomplete (Ctx, F_Padding)
      or Incomplete (Ctx, F_Allow_Isolated)
      or Incomplete (Ctx, F_Dump_Flags));

   function Get_Len (Ctx : Context) return Service_Manager.Len is
     (Ctx.Cursors (F_Len).Value.Len_Value);

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

   procedure Get_Server (Ctx : Context) is
      First : constant Types.Index := Types.Byte_Index (Ctx.Cursors (F_Server).First);
      Last : constant Types.Index := Types.Byte_Index (Ctx.Cursors (F_Server).Last);
   begin
      Process_Server (Ctx.Buffer.all (First .. Last));
   end Get_Server;

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
      procedure Insert is new Types.Insert (Service_Manager.Len);
      procedure Insert is new Types.Insert (Service_Manager.MBZ_7_Base);
      procedure Insert is new Types.Insert (Builtin_Types.Boolean_Base);
      procedure Insert is new Types.Insert (Service_Manager.Integer_Base);
   begin
      Fst := First;
      Lst := Last;
      case Val.Fld is
         when F_Initial =>
            null;
         when F_Len =>
            Insert (Val.Len_Value, Ctx.Buffer.all (Buffer_First .. Buffer_Last), Offset);
         when F_Name | F_Server =>
            null;
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

   procedure Set_Server (Ctx : in out Context) is
      First : constant Types.Bit_Index := Field_First (Ctx, F_Server);
      Last : constant Types.Bit_Index := Field_Last (Ctx, F_Server);
      function Buffer_First return Types.Index is
        (Types.Byte_Index (First));
      function Buffer_Last return Types.Index is
        (Types.Byte_Index (Last));
   begin
      Initialize_Server (Ctx);
      Process_Server (Ctx.Buffer.all (Buffer_First .. Buffer_Last));
   end Set_Server;

   procedure Initialize_Name (Ctx : in out Context) is
      First : constant Types.Bit_Index := Field_First (Ctx, F_Name);
      Last : constant Types.Bit_Index := Field_Last (Ctx, F_Name);
   begin
      Reset_Dependent_Fields (Ctx, F_Name);
      Ctx := (Ctx.Buffer_First, Ctx.Buffer_Last, Ctx.First, Last, Ctx.Buffer, Ctx.Cursors);
      pragma Assert ((if Structural_Valid (Ctx.Cursors (F_Len)) then
          (Ctx.Cursors (F_Len).Last - Ctx.Cursors (F_Len).First + 1) = Service_Manager.Len'Size
            and then Ctx.Cursors (F_Len).Predecessor = F_Initial
            and then Ctx.Cursors (F_Len).First = Ctx.First
            and then (if Structural_Valid (Ctx.Cursors (F_Name)) then
               (Ctx.Cursors (F_Name).Last - Ctx.Cursors (F_Name).First + 1) = Types.Bit_Length (Ctx.Cursors (F_Len).Value.Len_Value) * 8
                 and then Ctx.Cursors (F_Name).Predecessor = F_Len
                 and then Ctx.Cursors (F_Name).First = (Ctx.Cursors (F_Len).Last + 1)
                 and then (if Structural_Valid (Ctx.Cursors (F_Server)) then
                    (Ctx.Cursors (F_Server).Last - Ctx.Cursors (F_Server).First + 1) = 192
                      and then Ctx.Cursors (F_Server).Predecessor = F_Name
                      and then Ctx.Cursors (F_Server).First = (Ctx.Cursors (F_Name).Last + 1)
                      and then (if Structural_Valid (Ctx.Cursors (F_Padding)) then
                         (Ctx.Cursors (F_Padding).Last - Ctx.Cursors (F_Padding).First + 1) = Service_Manager.MBZ_7_Base'Size
                           and then Ctx.Cursors (F_Padding).Predecessor = F_Server
                           and then Ctx.Cursors (F_Padding).First = (Ctx.Cursors (F_Server).Last + 1)
                           and then (if Structural_Valid (Ctx.Cursors (F_Allow_Isolated)) then
                              (Ctx.Cursors (F_Allow_Isolated).Last - Ctx.Cursors (F_Allow_Isolated).First + 1) = Builtin_Types.Boolean_Base'Size
                                and then Ctx.Cursors (F_Allow_Isolated).Predecessor = F_Padding
                                and then Ctx.Cursors (F_Allow_Isolated).First = (Ctx.Cursors (F_Padding).Last + 1)
                                and then (if Structural_Valid (Ctx.Cursors (F_Dump_Flags)) then
                                   (Ctx.Cursors (F_Dump_Flags).Last - Ctx.Cursors (F_Dump_Flags).First + 1) = Service_Manager.Integer_Base'Size
                                     and then Ctx.Cursors (F_Dump_Flags).Predecessor = F_Allow_Isolated
                                     and then Ctx.Cursors (F_Dump_Flags).First = (Ctx.Cursors (F_Allow_Isolated).Last + 1))))))));
      Ctx.Cursors (F_Name) := (State => S_Structural_Valid, First => First, Last => Last, Value => (Fld => F_Name), Predecessor => Ctx.Cursors (F_Name).Predecessor);
      Ctx.Cursors (Successor (Ctx, F_Name)) := (State => S_Invalid, Predecessor => F_Name);
   end Initialize_Name;

   procedure Initialize_Server (Ctx : in out Context) is
      First : constant Types.Bit_Index := Field_First (Ctx, F_Server);
      Last : constant Types.Bit_Index := Field_Last (Ctx, F_Server);
   begin
      Reset_Dependent_Fields (Ctx, F_Server);
      Ctx := (Ctx.Buffer_First, Ctx.Buffer_Last, Ctx.First, Last, Ctx.Buffer, Ctx.Cursors);
      pragma Assert ((if Structural_Valid (Ctx.Cursors (F_Len)) then
          (Ctx.Cursors (F_Len).Last - Ctx.Cursors (F_Len).First + 1) = Service_Manager.Len'Size
            and then Ctx.Cursors (F_Len).Predecessor = F_Initial
            and then Ctx.Cursors (F_Len).First = Ctx.First
            and then (if Structural_Valid (Ctx.Cursors (F_Name)) then
               (Ctx.Cursors (F_Name).Last - Ctx.Cursors (F_Name).First + 1) = Types.Bit_Length (Ctx.Cursors (F_Len).Value.Len_Value) * 8
                 and then Ctx.Cursors (F_Name).Predecessor = F_Len
                 and then Ctx.Cursors (F_Name).First = (Ctx.Cursors (F_Len).Last + 1)
                 and then (if Structural_Valid (Ctx.Cursors (F_Server)) then
                    (Ctx.Cursors (F_Server).Last - Ctx.Cursors (F_Server).First + 1) = 192
                      and then Ctx.Cursors (F_Server).Predecessor = F_Name
                      and then Ctx.Cursors (F_Server).First = (Ctx.Cursors (F_Name).Last + 1)
                      and then (if Structural_Valid (Ctx.Cursors (F_Padding)) then
                         (Ctx.Cursors (F_Padding).Last - Ctx.Cursors (F_Padding).First + 1) = Service_Manager.MBZ_7_Base'Size
                           and then Ctx.Cursors (F_Padding).Predecessor = F_Server
                           and then Ctx.Cursors (F_Padding).First = (Ctx.Cursors (F_Server).Last + 1)
                           and then (if Structural_Valid (Ctx.Cursors (F_Allow_Isolated)) then
                              (Ctx.Cursors (F_Allow_Isolated).Last - Ctx.Cursors (F_Allow_Isolated).First + 1) = Builtin_Types.Boolean_Base'Size
                                and then Ctx.Cursors (F_Allow_Isolated).Predecessor = F_Padding
                                and then Ctx.Cursors (F_Allow_Isolated).First = (Ctx.Cursors (F_Padding).Last + 1)
                                and then (if Structural_Valid (Ctx.Cursors (F_Dump_Flags)) then
                                   (Ctx.Cursors (F_Dump_Flags).Last - Ctx.Cursors (F_Dump_Flags).First + 1) = Service_Manager.Integer_Base'Size
                                     and then Ctx.Cursors (F_Dump_Flags).Predecessor = F_Allow_Isolated
                                     and then Ctx.Cursors (F_Dump_Flags).First = (Ctx.Cursors (F_Allow_Isolated).Last + 1))))))));
      Ctx.Cursors (F_Server) := (State => S_Structural_Valid, First => First, Last => Last, Value => (Fld => F_Server), Predecessor => Ctx.Cursors (F_Server).Predecessor);
      Ctx.Cursors (Successor (Ctx, F_Server)) := (State => S_Invalid, Predecessor => F_Server);
   end Initialize_Server;

end Parpen.Service_Manager.Generic_Request_Add_Service;
