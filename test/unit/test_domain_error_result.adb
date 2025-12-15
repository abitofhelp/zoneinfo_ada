pragma Ada_2022;
--  ======================================================================
--  Test_Domain_Error_Result
--  ======================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--  Purpose:
--    Unit tests for Domain.Error.Result monad functionality.
--    Tests Ok/Error constructors, Is_Ok/Is_Error queries, and then value
--    extraction.
--  ======================================================================

with Domain;
with Ada.Text_IO;
with Domain.Error;
with Domain.Error.Result;
with Test_Framework;

procedure Test_Domain_Error_Result is

   use Ada.Text_IO;
   use Domain.Error;

   --  Test statistics
   Total_Tests  : Natural := 0;
   Passed_Tests : Natural := 0;

   --  Helper procedure to run a test
   pragma Style_Checks (Off);
   procedure Run_Test (Name : String; Passed : Boolean) is
   begin
      Total_Tests := Total_Tests + 1;
      if Passed then
         Passed_Tests := Passed_Tests + 1;
         Put_Line ("[PASS] " & Name);
      else
         Put_Line ("[FAIL] " & Name);
      end if;
   end Run_Test;
   pragma Style_Checks (On);

   --  Instantiate Result for Integer (for testing)
   package Int_Result is new Domain.Error.Result.Generic_Result (T => Integer);

   --  Instantiate Result for Boolean (for testing)
   package Bool_Result is new
     Domain.Error.Result.Generic_Result (T => Boolean);

begin
   Put_Line ("========================================");
   Put_Line ("Testing: Domain.Error.Result");
   Put_Line ("========================================");
   New_Line;

   --  ========================================================================
   --  Test: Ok construction and then Is_Ok query
   --  ========================================================================

   declare
      R : constant Int_Result.Result := Int_Result.Ok (42);
   begin
      Run_Test
        ("Ok construction - Is_Ok returns true", Int_Result.Is_Ok (R));
      Run_Test
        ("Ok construction - Is_Error returns false",
         not Int_Result.Is_Error (R));
   end;

   --  ========================================================================
   --  Test: Ok value extraction
   --  ========================================================================

   declare
      R   : constant Int_Result.Result := Int_Result.Ok (123);
      Val : Integer;
   begin
      if Int_Result.Is_Ok (R) then
         Val := Int_Result.Value (R);
         Run_Test ("Ok value extraction - correct value", Val = 123);
      else
         Run_Test ("Ok value extraction - Result should be Ok", False);
      end if;
   end;

   --  ========================================================================
   --  Test: Error construction and then Is_Error query
   --  ========================================================================

   declare
      R : constant Int_Result.Result :=
        Int_Result.Error
          (Kind => Validation_Error, Message => "Test validation error");
   begin
      Run_Test
        ("Error construction - Is_Error returns true",
         Int_Result.Is_Error (R));
      Run_Test
        ("Error construction - Is_Ok returns false", not Int_Result.Is_Ok (R));
   end;

   --  ========================================================================
   --  Test: Error info extraction
   --  ========================================================================

   declare
      R    : constant Int_Result.Result :=
        Int_Result.Error
          (Kind => IO_Error, Message => "Test IO error");
      Info : Error_Type;
   begin
      if Int_Result.Is_Error (R) then
         Info := Int_Result.Error_Info (R);
         Run_Test
           ("Error info - correct kind", Info.Kind = IO_Error);
         Run_Test
           ("Error info - correct message",
            Error_Strings.To_String (Info.Message) = "Test IO error");
      else
         Run_Test ("Error info extraction - Result should be Error", False);
      end if;
   end;

   --  ========================================================================
   --  Test: Result with Boolean type
   --  ========================================================================

   declare
      R : constant Bool_Result.Result := Bool_Result.Ok (True);
   begin
      Run_Test
        ("Boolean Result - Is_Ok returns true", Bool_Result.Is_Ok (R));
      if Bool_Result.Is_Ok (R) then
         Run_Test
           ("Boolean Result - correct value",
            Bool_Result.Value (R) = True);
      end if;
   end;

   --  ========================================================================
   --  Test: Error with empty message
   --  ========================================================================

   declare
      R    : constant Int_Result.Result :=
        Int_Result.Error (Kind => Validation_Error, Message => "");
      Info : Error_Type;
   begin
      Run_Test
        ("Error with empty message - Is_Error", Int_Result.Is_Error (R));
      if Int_Result.Is_Error (R) then
         Info := Int_Result.Error_Info (R);
         Run_Test
           ("Error with empty message - message is empty",
            Error_Strings.Length (Info.Message) = 0);
      end if;
   end;

   --  ========================================================================
   --  Test: Multiple Ok values don't interfere
   --  ========================================================================

   declare
      R1 : constant Int_Result.Result := Int_Result.Ok (100);
      R2 : constant Int_Result.Result := Int_Result.Ok (200);
   begin
      Run_Test
        ("Multiple Ok values - R1 has correct value",
         Int_Result.Is_Ok (R1) and then Int_Result.Value (R1) = 100);
      Run_Test
        ("Multiple Ok values - R2 has correct value",
         Int_Result.Is_Ok (R2) and then Int_Result.Value (R2) = 200);
   end;

   --  ========================================================================
   --  Test: Multiple Error values don't interfere
   --  ========================================================================

   declare
      R1   : constant Int_Result.Result :=
        Int_Result.Error (Kind => Validation_Error, Message => "Error 1");
      R2   : constant Int_Result.Result :=
        Int_Result.Error (Kind => IO_Error, Message => "Error 2");
      Info1 : Error_Type;
      Info2 : Error_Type;
   begin
      if Int_Result.Is_Error (R1) and then Int_Result.Is_Error (R2) then
         Info1 := Int_Result.Error_Info (R1);
         Info2 := Int_Result.Error_Info (R2);
         Run_Test
           ("Multiple errors - R1 has correct kind",
            Info1.Kind = Validation_Error);
         Run_Test
           ("Multiple errors - R1 has correct message",
            Error_Strings.To_String (Info1.Message) = "Error 1");
         Run_Test
           ("Multiple errors - R2 has correct kind",
            Info2.Kind = IO_Error);
         Run_Test
           ("Multiple errors - R2 has correct message",
            Error_Strings.To_String (Info2.Message) = "Error 2");
      else
         Run_Test ("Multiple errors test failed", False);
      end if;
   end;

   --  ========================================================================
   --  Test: Long error message (boundary test)
   --  ========================================================================

   declare
      Long_Message : constant String (1 .. 500) := [others => 'X'];
      R            : constant Int_Result.Result :=
        Int_Result.Error (Kind => Validation_Error, Message => Long_Message);
      Info : Error_Type;
   begin
      Run_Test
        ("Long error message - Is_Error", Int_Result.Is_Error (R));
      if Int_Result.Is_Error (R) then
         Info := Int_Result.Error_Info (R);
         --  Message should be truncated to max length (512)
         Run_Test
           ("Long error message - message stored",
            Error_Strings.Length (Info.Message) > 0);
      end if;
   end;

   --  ========================================================================
   --  Test: Unwrap_Or with Ok returns value
   --  ========================================================================

   declare
      R   : constant Int_Result.Result := Int_Result.Ok (42);
      Val : constant Integer           := Int_Result.Unwrap_Or (R, 0);
   begin
      Run_Test ("Unwrap_Or with Ok returns value", Val = 42);
   end;

   --  ========================================================================
   --  Test: Unwrap_Or with Error returns default
   --  ========================================================================

   declare
      R   : constant Int_Result.Result :=
        Int_Result.Error (Kind => Validation_Error, Message => "Error");
      Val : constant Integer := Int_Result.Unwrap_Or (R, 99);
   begin
      Run_Test ("Unwrap_Or with Error returns default", Val = 99);
   end;

   --  ========================================================================
   --  Test: Unwrap_Or_With with Ok returns value
   --  ========================================================================

   declare
      Default_Called : Boolean := False;

      function Get_Default return Integer is
      begin
         Default_Called := True;
         return 77;
      end Get_Default;

      function Unwrap_With_Default is new Int_Result.Unwrap_Or_With
        (F => Get_Default);

      R   : constant Int_Result.Result := Int_Result.Ok (42);
      Val : constant Integer           := Unwrap_With_Default (R);
   begin
      Run_Test
        ("Unwrap_Or_With with Ok returns value and doesn't call F",
         Val = 42 and then not Default_Called);
   end;

   --  ========================================================================
   --  Test: Unwrap_Or_With with Error calls F
   --  ========================================================================

   declare
      Default_Called : Boolean := False;

      function Get_Default return Integer is
      begin
         Default_Called := True;
         return 77;
      end Get_Default;

      function Unwrap_With_Default is new Int_Result.Unwrap_Or_With
        (F => Get_Default);

      R   : constant Int_Result.Result :=
        Int_Result.Error (Kind => Validation_Error, Message => "Error");
      Val : constant Integer := Unwrap_With_Default (R);
   begin
      Run_Test
        ("Unwrap_Or_With with Error calls F and returns its value",
         Val = 77 and then Default_Called);
   end;

   --  ========================================================================
   --  Test: Map with Ok transforms value
   --  ========================================================================

   declare
      function Double (X : Integer) return Integer is (X * 2);
      function Map_Double is new Int_Result.Map (F => Double);

      R      : constant Int_Result.Result := Int_Result.Ok (21);
      Mapped : constant Int_Result.Result := Map_Double (R);
   begin
      Run_Test
        ("Map with Ok transforms value",
         Int_Result.Is_Ok (Mapped) and then Int_Result.Value (Mapped) = 42);
   end;

   --  ========================================================================
   --  Test: Map with Error propagates error
   --  ========================================================================

   declare
      function Double (X : Integer) return Integer is (X * 2);
      function Map_Double is new Int_Result.Map (F => Double);

      R      : constant Int_Result.Result :=
        Int_Result.Error (Kind => IO_Error, Message => "IO failure");
      Mapped : constant Int_Result.Result := Map_Double (R);
      Info   : Error_Type;
   begin
      if Int_Result.Is_Error (Mapped) then
         Info := Int_Result.Error_Info (Mapped);
         Run_Test
           ("Map with Error propagates error",
            Info.Kind = IO_Error and then
            Error_Strings.To_String (Info.Message) = "IO failure");
      else
         Run_Test ("Map with Error should propagate", False);
      end if;
   end;

   --  ========================================================================
   --  Test: And_Then with Ok chains to another Ok
   --  ========================================================================

   declare
      function Half_If_Even (X : Integer) return Int_Result.Result is
      begin
         if X mod 2 = 0 then
            return Int_Result.Ok (X / 2);
         else
            return Int_Result.Error
              (Kind    => Validation_Error,
               Message => "Number is odd");
         end if;
      end Half_If_Even;

      function And_Then_Half is new Int_Result.And_Then (F => Half_If_Even);

      R      : constant Int_Result.Result := Int_Result.Ok (42);
      Result : constant Int_Result.Result := And_Then_Half (R);
   begin
      Run_Test
        ("And_Then with Ok chains to another Ok",
         Int_Result.Is_Ok (Result) and then Int_Result.Value (Result) = 21);
   end;

   --  ========================================================================
   --  Test: And_Then with Ok chains to Error
   --  ========================================================================

   declare
      function Half_If_Even (X : Integer) return Int_Result.Result is
      begin
         if X mod 2 = 0 then
            return Int_Result.Ok (X / 2);
         else
            return Int_Result.Error
              (Kind    => Validation_Error,
               Message => "Number is odd");
         end if;
      end Half_If_Even;

      function And_Then_Half is new Int_Result.And_Then (F => Half_If_Even);

      R      : constant Int_Result.Result := Int_Result.Ok (43);
      Result : constant Int_Result.Result := And_Then_Half (R);
      Info   : Error_Type;
   begin
      if Int_Result.Is_Error (Result) then
         Info := Int_Result.Error_Info (Result);
         Run_Test
           ("And_Then with Ok chains to Error",
            Info.Kind = Validation_Error and then
            Error_Strings.To_String (Info.Message) = "Number is odd");
      else
         Run_Test ("And_Then should return Error for odd number", False);
      end if;
   end;

   --  ========================================================================
   --  Test: And_Then with Error propagates without calling F
   --  ========================================================================

   declare
      F_Called : Boolean := False;

      function Half_If_Even (X : Integer) return Int_Result.Result is
      begin
         F_Called := True;
         return Int_Result.Ok (X / 2);
      end Half_If_Even;

      function And_Then_Half is new Int_Result.And_Then (F => Half_If_Even);

      R      : constant Int_Result.Result :=
        Int_Result.Error (Kind => IO_Error, Message => "IO failure");
      Result : constant Int_Result.Result := And_Then_Half (R);
      Info   : Error_Type;
   begin
      if Int_Result.Is_Error (Result) then
         Info := Int_Result.Error_Info (Result);
         Run_Test
           ("And_Then with Error propagates without calling F",
            not F_Called and then Info.Kind = IO_Error);
      else
         Run_Test ("And_Then should propagate Error", False);
      end if;
   end;

   --  ========================================================================
   --  Test: Map_Error with Error transforms error
   --  ========================================================================

   declare
      function Add_Context (E : Error_Type) return Error_Type is
         use Error_Strings;
      begin
         return
           (Kind    => E.Kind,
            Message => To_Bounded_String
              ("Context: " & To_String (E.Message)));
      end Add_Context;

      function Map_Error_With_Context is new Int_Result.Map_Error
        (F => Add_Context);

      R      : constant Int_Result.Result :=
        Int_Result.Error (Kind => IO_Error, Message => "File not found");
      Mapped : constant Int_Result.Result := Map_Error_With_Context (R);
      Info   : Error_Type;
   begin
      if Int_Result.Is_Error (Mapped) then
         Info := Int_Result.Error_Info (Mapped);
         Run_Test
           ("Map_Error with Error transforms error",
            Info.Kind = IO_Error and then
            Error_Strings.To_String (Info.Message) =
               "Context: File not found");
      else
         Run_Test ("Map_Error should return Error", False);
      end if;
   end;

   --  ========================================================================
   --  Test: Map_Error with Ok propagates Ok
   --  ========================================================================

   declare
      function Add_Context (E : Error_Type) return Error_Type is
         use Error_Strings;
      begin
         return
           (Kind    => E.Kind,
            Message => To_Bounded_String
              ("Context: " & To_String (E.Message)));
      end Add_Context;

      function Map_Error_With_Context is new Int_Result.Map_Error
        (F => Add_Context);

      R      : constant Int_Result.Result := Int_Result.Ok (42);
      Mapped : constant Int_Result.Result := Map_Error_With_Context (R);
   begin
      Run_Test
        ("Map_Error with Ok propagates Ok",
         Int_Result.Is_Ok (Mapped) and then Int_Result.Value (Mapped) = 42);
   end;

   --  ========================================================================
   --  Test: Fallback with Ok primary returns primary
   --  ========================================================================

   declare
      Primary     : constant Int_Result.Result := Int_Result.Ok (42);
      Alternative : constant Int_Result.Result := Int_Result.Ok (99);
      Result      : constant Int_Result.Result :=
        Int_Result.Fallback (Primary, Alternative);
   begin
      Run_Test
        ("Fallback with Ok primary returns primary",
         Int_Result.Is_Ok (Result) and then Int_Result.Value (Result) = 42);
   end;

   --  ========================================================================
   --  Test: Fallback with Error primary returns alternative
   --  ========================================================================

   declare
      Primary     : constant Int_Result.Result :=
        Int_Result.Error (Kind => IO_Error, Message => "Error");
      Alternative : constant Int_Result.Result := Int_Result.Ok (99);
      Result      : constant Int_Result.Result :=
        Int_Result.Fallback (Primary, Alternative);
   begin
      Run_Test
        ("Fallback with Error primary returns alternative",
         Int_Result.Is_Ok (Result) and then Int_Result.Value (Result) = 99);
   end;

   --  ========================================================================
   --  Test: Fallback_With with Ok doesn't call F
   --  ========================================================================

   declare
      F_Called : Boolean := False;

      function Get_Alternative return Int_Result.Result is
      begin
         F_Called := True;
         return Int_Result.Ok (99);
      end Get_Alternative;

      function Fallback_With_Alt is new Int_Result.Fallback_With
        (F => Get_Alternative);

      R      : constant Int_Result.Result := Int_Result.Ok (42);
      Result : constant Int_Result.Result := Fallback_With_Alt (R);
   begin
      Run_Test
        ("Fallback_With with Ok doesn't call F",
         Int_Result.Is_Ok (Result) and then Int_Result.Value (Result) = 42
           and then not F_Called);
   end;

   --  ========================================================================
   --  Test: Fallback_With with Error calls F
   --  ========================================================================

   declare
      F_Called : Boolean := False;

      function Get_Alternative return Int_Result.Result is
      begin
         F_Called := True;
         return Int_Result.Ok (99);
      end Get_Alternative;

      function Fallback_With_Alt is new Int_Result.Fallback_With
        (F => Get_Alternative);

      R      : constant Int_Result.Result :=
        Int_Result.Error (Kind => IO_Error, Message => "Error");
      Result : constant Int_Result.Result := Fallback_With_Alt (R);
   begin
      Run_Test
        ("Fallback_With with Error calls F",
         Int_Result.Is_Ok (Result) and then Int_Result.Value (Result) = 99
           and then F_Called);
   end;

   --  ========================================================================
   --  Test: Recover with Ok returns value
   --  ========================================================================

   declare
      function Handle_Error (E : Error_Type) return Integer is
         pragma Unreferenced (E);
      begin
         return -1;
      end Handle_Error;

      function Recover_From_Error is new Int_Result.Recover
        (Handle => Handle_Error);

      R   : constant Int_Result.Result := Int_Result.Ok (42);
      Val : constant Integer           := Recover_From_Error (R);
   begin
      Run_Test ("Recover with Ok returns value", Val = 42);
   end;

   --  ========================================================================
   --  Test: Recover with Error calls handler
   --  ========================================================================

   declare
      Handler_Called : Boolean := False;

      function Handle_Error (E : Error_Type) return Integer is
         pragma Unreferenced (E);
      begin
         Handler_Called := True;
         return -1;
      end Handle_Error;

      function Recover_From_Error is new Int_Result.Recover
        (Handle => Handle_Error);

      R   : constant Int_Result.Result :=
        Int_Result.Error (Kind => Validation_Error, Message => "Error");
      Val : constant Integer := Recover_From_Error (R);
   begin
      Run_Test
        ("Recover with Error calls handler",
         Val = -1 and then Handler_Called);
   end;

   --  ========================================================================
   --  Test: Recover_With with Ok returns Ok
   --  ========================================================================

   declare
      function Handle_Error (E : Error_Type) return Int_Result.Result is
         pragma Unreferenced (E);
      begin
         return Int_Result.Ok (-1);
      end Handle_Error;

      function Recover_With_Handler is new Int_Result.Recover_With
        (Handle => Handle_Error);

      R      : constant Int_Result.Result := Int_Result.Ok (42);
      Result : constant Int_Result.Result := Recover_With_Handler (R);
   begin
      Run_Test
        ("Recover_With with Ok returns Ok",
         Int_Result.Is_Ok (Result) and then Int_Result.Value (Result) = 42);
   end;

   --  ========================================================================
   --  Test: Recover_With with Error calls handler returning Ok
   --  ========================================================================

   declare
      Handler_Called : Boolean := False;

      function Handle_Error (E : Error_Type) return Int_Result.Result is
         pragma Unreferenced (E);
      begin
         Handler_Called := True;
         return Int_Result.Ok (-1);
      end Handle_Error;

      function Recover_With_Handler is new Int_Result.Recover_With
        (Handle => Handle_Error);

      R      : constant Int_Result.Result :=
        Int_Result.Error (Kind => Validation_Error, Message => "Error");
      Result : constant Int_Result.Result := Recover_With_Handler (R);
   begin
      Run_Test
        ("Recover_With with Error calls handler returning Ok",
         Int_Result.Is_Ok (Result) and then Int_Result.Value (Result) = -1
           and then Handler_Called);
   end;

   --  ========================================================================
   --  Test: Recover_With with Error calls handler returning Error
   --  ========================================================================

   declare
      Handler_Called : Boolean := False;

      function Handle_Error (E : Error_Type) return Int_Result.Result is
         pragma Unreferenced (E);
      begin
         Handler_Called := True;
         return Int_Result.Error
           (Kind    => IO_Error,
            Message => "Recovered error");
      end Handle_Error;

      function Recover_With_Handler is new Int_Result.Recover_With
        (Handle => Handle_Error);

      R      : constant Int_Result.Result :=
        Int_Result.Error (Kind => Validation_Error, Message => "Error");
      Result : constant Int_Result.Result := Recover_With_Handler (R);
      Info   : Error_Type;
   begin
      if Int_Result.Is_Error (Result) then
         Info := Int_Result.Error_Info (Result);
         Run_Test
           ("Recover_With with Error calls handler returning Error",
            Handler_Called and then Info.Kind = IO_Error and then
            Error_Strings.To_String (Info.Message) = "Recovered error");
      else
         Run_Test ("Recover_With should return Error from handler", False);
      end if;
   end;

   --  ========================================================================
   --  Test: From_Error constructor
   --  ========================================================================

   declare
      Err : constant Error_Type :=
        (Kind    => Validation_Error,
         Message => Error_Strings.To_Bounded_String ("validation failed"));
      R   : constant Int_Result.Result := Int_Result.From_Error (Err);
   begin
      Run_Test ("From_Error - Is_Error returns true", Int_Result.Is_Error (R));
      if Int_Result.Is_Error (R) then
         Run_Test
           ("From_Error - preserves error kind",
            Int_Result.Error_Info (R).Kind = Validation_Error);
         Run_Test
           ("From_Error - preserves error message",
            Error_Strings.To_String (Int_Result.Error_Info (R).Message) =
              "validation failed");
      end if;
   end;

   --  ========================================================================
   --  Test: Bimap with Ok transforms value
   --  ========================================================================

   declare
      function Double (X : Integer) return Integer is (X * 2);

      function Change_Kind (E : Error_Type) return Error_Type is
        ((Kind => IO_Error, Message => E.Message));

      function Transform is new Int_Result.Bimap
        (Map_Ok  => Double,
         Map_Err => Change_Kind);

      R      : constant Int_Result.Result := Int_Result.Ok (21);
      Mapped : constant Int_Result.Result := Transform (R);
   begin
      Run_Test
        ("Bimap with Ok transforms value",
         Int_Result.Is_Ok (Mapped) and then Int_Result.Value (Mapped) = 42);
   end;

   --  ========================================================================
   --  Test: Bimap with Error transforms error
   --  ========================================================================

   declare
      function Double (X : Integer) return Integer is (X * 2);

      function Change_Kind (E : Error_Type) return Error_Type is
        ((Kind => IO_Error, Message => E.Message));

      function Transform is new Int_Result.Bimap
        (Map_Ok  => Double,
         Map_Err => Change_Kind);

      R      : constant Int_Result.Result :=
        Int_Result.Error (Kind => Validation_Error, Message => "test error");
      Mapped : constant Int_Result.Result := Transform (R);
      Info   : Error_Type;
   begin
      if Int_Result.Is_Error (Mapped) then
         Info := Int_Result.Error_Info (Mapped);
         Run_Test
           ("Bimap with Error transforms error kind", Info.Kind = IO_Error);
      else
         Run_Test ("Bimap with Error should return Error", False);
      end if;
   end;

   --  ========================================================================
   --  Test: Ensure with Ok and predicate passes
   --  ========================================================================

   declare
      function Is_Positive (X : Integer) return Boolean is (X > 0);

      function To_Validation_Error (X : Integer) return Error_Type is
         pragma Unreferenced (X);
      begin
         return
           (Kind    => Validation_Error,
            Message => Error_Strings.To_Bounded_String ("Not positive"));
      end To_Validation_Error;

      function Validate is new Int_Result.Ensure
        (Pred     => Is_Positive,
         To_Error => To_Validation_Error);

      R      : constant Int_Result.Result := Int_Result.Ok (10);
      Result : constant Int_Result.Result := Validate (R);
   begin
      Run_Test
        ("Ensure with Ok keeps Ok if predicate holds",
         Int_Result.Is_Ok (Result) and then Int_Result.Value (Result) = 10);
   end;

   --  ========================================================================
   --  Test: Ensure with Ok and predicate fails
   --  ========================================================================

   declare
      function Is_Positive (X : Integer) return Boolean is (X > 0);

      function To_Validation_Error (X : Integer) return Error_Type is
         pragma Unreferenced (X);
      begin
         return
           (Kind    => Validation_Error,
            Message => Error_Strings.To_Bounded_String ("Not positive"));
      end To_Validation_Error;

      function Validate is new Int_Result.Ensure
        (Pred     => Is_Positive,
         To_Error => To_Validation_Error);

      R      : constant Int_Result.Result := Int_Result.Ok (-5);
      Result : constant Int_Result.Result := Validate (R);
   begin
      Run_Test
        ("Ensure with Ok converts to Error if predicate fails",
         Int_Result.Is_Error (Result));
   end;

   --  ========================================================================
   --  Test: Ensure with Error leaves Error unchanged
   --  ========================================================================

   declare
      function Is_Positive (X : Integer) return Boolean is (X > 0);

      function To_Validation_Error (X : Integer) return Error_Type is
         pragma Unreferenced (X);
      begin
         return
           (Kind    => Validation_Error,
            Message => Error_Strings.To_Bounded_String ("Not positive"));
      end To_Validation_Error;

      function Validate is new Int_Result.Ensure
        (Pred     => Is_Positive,
         To_Error => To_Validation_Error);

      R      : constant Int_Result.Result :=
        Int_Result.Error (Kind => IO_Error, Message => "Original error");
      Result : constant Int_Result.Result := Validate (R);
      Info   : Error_Type;
   begin
      if Int_Result.Is_Error (Result) then
         Info := Int_Result.Error_Info (Result);
         Run_Test
           ("Ensure with Error leaves Error unchanged",
            Info.Kind = IO_Error and then
            Error_Strings.To_String (Info.Message) = "Original error");
      else
         Run_Test ("Ensure should leave Error unchanged", False);
      end if;
   end;

   --  ========================================================================
   --  Test: With_Context on Ok returns Ok unchanged
   --  ========================================================================

   declare
      function Add_Location (E : Error_Type; Where : String) return Error_Type
      is
         use Error_Strings;
         New_Msg : constant String := To_String (E.Message) & " at " & Where;
      begin
         return (Kind => E.Kind, Message => To_Bounded_String (New_Msg));
      end Add_Location;

      function Add_Context is new Int_Result.With_Context
        (Add => Add_Location);

      R      : constant Int_Result.Result := Int_Result.Ok (42);
      Result : constant Int_Result.Result := Add_Context (R, "test_location");
   begin
      Run_Test
        ("With_Context on Ok returns Ok unchanged",
         Int_Result.Is_Ok (Result) and then Int_Result.Value (Result) = 42);
   end;

   --  ========================================================================
   --  Test: With_Context on Error adds context
   --  ========================================================================

   declare
      function Add_Location (E : Error_Type; Where : String) return Error_Type
      is
         use Error_Strings;
         New_Msg : constant String := To_String (E.Message) & " at " & Where;
      begin
         return (Kind => E.Kind, Message => To_Bounded_String (New_Msg));
      end Add_Location;

      function Add_Context is new Int_Result.With_Context
        (Add => Add_Location);

      R      : constant Int_Result.Result :=
        Int_Result.Error (Kind => IO_Error, Message => "file not found");
      Result : constant Int_Result.Result := Add_Context (R, "read_config");
      Info   : Error_Type;
   begin
      if Int_Result.Is_Error (Result) then
         Info := Int_Result.Error_Info (Result);
         Run_Test
           ("With_Context on Error adds context",
            Info.Kind = IO_Error and then
            Error_Strings.To_String (Info.Message) =
               "file not found at read_config");
      else
         Run_Test ("With_Context should return Error", False);
      end if;
   end;

   --  ========================================================================
   --  Test: Tap with Ok calls On_Ok
   --  ========================================================================

   declare
      On_Ok_Called    : Boolean := False;
      On_Error_Called : Boolean := False;
      Captured_Value  : Integer := 0;

      procedure On_Ok (V : Integer) is
      begin
         On_Ok_Called   := True;
         Captured_Value := V;
      end On_Ok;

      procedure On_Error (E : Error_Type) is
         pragma Unreferenced (E);
      begin
         On_Error_Called := True;
      end On_Error;

      function Tap_Side_Effects is new Int_Result.Tap
        (On_Ok    => On_Ok,
         On_Error => On_Error);

      R      : constant Int_Result.Result := Int_Result.Ok (42);
      Result : constant Int_Result.Result := Tap_Side_Effects (R);
   begin
      Run_Test
        ("Tap with Ok calls On_Ok and returns same Result",
         On_Ok_Called and then not On_Error_Called and then
         Captured_Value = 42 and then Int_Result.Is_Ok (Result) and then
         Int_Result.Value (Result) = 42);
   end;

   --  ========================================================================
   --  Test: Tap with Error calls On_Error
   --  ========================================================================

   declare
      On_Ok_Called    : Boolean := False;
      On_Error_Called : Boolean := False;
      Captured_Kind   : Error_Kind;

      procedure On_Ok (V : Integer) is
         pragma Unreferenced (V);
      begin
         On_Ok_Called := True;
      end On_Ok;

      procedure On_Error (E : Error_Type) is
      begin
         On_Error_Called := True;
         Captured_Kind   := E.Kind;
      end On_Error;

      function Tap_Side_Effects is new Int_Result.Tap
        (On_Ok    => On_Ok,
         On_Error => On_Error);

      R      : constant Int_Result.Result :=
        Int_Result.Error (Kind => IO_Error, Message => "IO failure");
      Result : constant Int_Result.Result := Tap_Side_Effects (R);
      Info   : Error_Type;
   begin
      if Int_Result.Is_Error (Result) then
         Info := Int_Result.Error_Info (Result);
         Run_Test
           ("Tap with Error calls On_Error and returns same Result",
            not On_Ok_Called and then On_Error_Called and then
            Captured_Kind = IO_Error and then Info.Kind = IO_Error);
      else
         Run_Test ("Tap should return Error", False);
      end if;
   end;

   --  ========================================================================
   --  Test: And_Then_Into with Ok chains to different type
   --  ========================================================================

   declare
      --  Float Result for cross-type testing
      package Float_Result is new Domain.Error.Result.Generic_Result
        (T => Float);

      function Int_To_Float (X : Integer) return Float_Result.Result is
      begin
         return Float_Result.Ok (Float (X) * 1.5);
      end Int_To_Float;

      function Chain_To_Float is new Domain.Error.Result.And_Then_Into
        (T             => Integer,
         U             => Float,
         Source_Result => Int_Result,
         Target_Result => Float_Result,
         F             => Int_To_Float);

      R      : constant Int_Result.Result := Int_Result.Ok (10);
      Result : constant Float_Result.Result := Chain_To_Float (R);
   begin
      Run_Test
        ("And_Then_Into with Ok chains to different type",
         Float_Result.Is_Ok (Result) and then
         Float_Result.Value (Result) = 15.0);
   end;

   --  ========================================================================
   --  Test: And_Then_Into with Error propagates to target type
   --  ========================================================================

   declare
      package Float_Result is new Domain.Error.Result.Generic_Result
        (T => Float);

      function Int_To_Float (X : Integer) return Float_Result.Result is
      begin
         return Float_Result.Ok (Float (X) * 1.5);
      end Int_To_Float;

      function Chain_To_Float is new Domain.Error.Result.And_Then_Into
        (T             => Integer,
         U             => Float,
         Source_Result => Int_Result,
         Target_Result => Float_Result,
         F             => Int_To_Float);

      R      : constant Int_Result.Result :=
        Int_Result.Error (Kind => IO_Error, Message => "Source error");
      Result : constant Float_Result.Result := Chain_To_Float (R);
      Info   : Error_Type;
   begin
      if Float_Result.Is_Error (Result) then
         Info := Float_Result.Error_Info (Result);
         Run_Test
           ("And_Then_Into with Error propagates to target type",
            Info.Kind = IO_Error and then
            Error_Strings.To_String (Info.Message) = "Source error");
      else
         Run_Test ("And_Then_Into should propagate Error", False);
      end if;
   end;

   --  ========================================================================
   --  Test: And_Then_Into with Ok chains to Error in target
   --  ========================================================================

   declare
      package Float_Result is new Domain.Error.Result.Generic_Result
        (T => Float);

      function Fail_If_Negative (X : Integer) return Float_Result.Result is
      begin
         if X < 0 then
            return Float_Result.Error
              (Kind    => Validation_Error,
               Message => "Negative value");
         else
            return Float_Result.Ok (Float (X));
         end if;
      end Fail_If_Negative;

      function Chain_To_Float is new Domain.Error.Result.And_Then_Into
        (T             => Integer,
         U             => Float,
         Source_Result => Int_Result,
         Target_Result => Float_Result,
         F             => Fail_If_Negative);

      R      : constant Int_Result.Result := Int_Result.Ok (-5);
      Result : constant Float_Result.Result := Chain_To_Float (R);
      Info   : Error_Type;
   begin
      if Float_Result.Is_Error (Result) then
         Info := Float_Result.Error_Info (Result);
         Run_Test
           ("And_Then_Into with Ok chains to Error in target",
            Info.Kind = Validation_Error and then
            Error_Strings.To_String (Info.Message) = "Negative value");
      else
         Run_Test ("And_Then_Into should return Error from F", False);
      end if;
   end;

   --  Print summary
   New_Line;
   Put_Line ("========================================");
   Put_Line ("Test Summary: Domain.Error.Result");
   Put_Line ("========================================");
   Put_Line ("Total tests: " & Total_Tests'Image);
   Put_Line ("Passed:      " & Passed_Tests'Image);
   Put_Line ("Failed:      " & Natural'Image (Total_Tests - Passed_Tests));
   New_Line;

   --  Register results with test framework
   Test_Framework.Register_Results (Total_Tests, Passed_Tests);

end Test_Domain_Error_Result;
