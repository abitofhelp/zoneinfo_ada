pragma Ada_2022;
with Domain;
--  ======================================================================
--  Test_Domain_Option
--  ======================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--
--  Purpose:
--    Unit tests for Domain.Value_Object.Option monad functionality.
--    Tests Of_Value/None constructors, Is_Some/Is_None queries, and all
--    monadic combinators (Map, And_Then, Filter, Or_Else, etc.).
--  ======================================================================

with Ada.Text_IO;
with Domain.Value_Object.Option;
with Test_Framework;

procedure Test_Domain_Option is

   use Ada.Text_IO;

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

   --  Instantiate Option for Integer (for testing)
   package Int_Option is new Domain.Value_Object.Option.Generic_Option
     (T => Integer);

   --  Instantiate Option for Boolean (for testing)
   package Bool_Option is new Domain.Value_Object.Option.Generic_Option
     (T => Boolean);

begin
   Put_Line ("========================================");
   Put_Line ("Testing: Domain.Value_Object.Option");
   Put_Line ("========================================");
   New_Line;

   --  ========================================================================
   --  Test: Of_Value construction and Is_Some query
   --  ========================================================================

   declare
      Opt : constant Int_Option.Option := Int_Option.Of_Value (42);
   begin
      Run_Test
        ("Of_Value construction - Is_Some returns true",
         Int_Option.Is_Some (Opt));
      Run_Test
        ("Of_Value construction - Is_None returns false",
         not Int_Option.Is_None (Opt));
   end;

   --  ========================================================================
   --  Test: Of_Value value extraction
   --  ========================================================================

   declare
      Opt : constant Int_Option.Option := Int_Option.Of_Value (123);
      Val : Integer;
   begin
      if Int_Option.Is_Some (Opt) then
         Val := Int_Option.Value (Opt);
         Run_Test ("Of_Value value extraction - correct value", Val = 123);
      else
         Run_Test ("Of_Value value extraction - Option should be Some", False);
      end if;
   end;

   --  ========================================================================
   --  Test: None construction and Is_None query
   --  ========================================================================

   declare
      Opt : constant Int_Option.Option := Int_Option.None;
   begin
      Run_Test
        ("None construction - Is_None returns true",
         Int_Option.Is_None (Opt));
      Run_Test
        ("None construction - Is_Some returns false",
         not Int_Option.Is_Some (Opt));
   end;

   --  ========================================================================
   --  Test: Option with Boolean type
   --  ========================================================================

   declare
      Opt : constant Bool_Option.Option := Bool_Option.Of_Value (True);
   begin
      Run_Test
        ("Boolean Option - Is_Some returns true",
         Bool_Option.Is_Some (Opt));
      if Bool_Option.Is_Some (Opt) then
         Run_Test
           ("Boolean Option - correct value",
            Bool_Option.Value (Opt) = True);
      end if;
   end;

   --  ========================================================================
   --  Test: Multiple Of_Value options don't interfere
   --  ========================================================================

   declare
      Opt1 : constant Int_Option.Option := Int_Option.Of_Value (100);
      Opt2 : constant Int_Option.Option := Int_Option.Of_Value (200);
   begin
      Run_Test
        ("Multiple Of_Value options - Opt1 has correct value",
         Int_Option.Is_Some (Opt1) and then Int_Option.Value (Opt1) = 100);
      Run_Test
        ("Multiple Of_Value options - Opt2 has correct value",
         Int_Option.Is_Some (Opt2) and then Int_Option.Value (Opt2) = 200);
   end;

   --  ========================================================================
   --  Test: Unwrap_Or with Some returns value
   --  ========================================================================

   declare
      Opt : constant Int_Option.Option := Int_Option.Of_Value (42);
      Val : constant Integer           := Int_Option.Unwrap_Or (Opt, 0);
   begin
      Run_Test ("Unwrap_Or with Some returns value", Val = 42);
   end;

   --  ========================================================================
   --  Test: Unwrap_Or with None returns default
   --  ========================================================================

   declare
      Opt : constant Int_Option.Option := Int_Option.None;
      Val : constant Integer           := Int_Option.Unwrap_Or (Opt, 99);
   begin
      Run_Test ("Unwrap_Or with None returns default", Val = 99);
   end;

   --  ========================================================================
   --  Test: Unwrap_Or_With with Some returns value
   --  ========================================================================

   declare
      Default_Called : Boolean := False;

      function Get_Default return Integer is
      begin
         Default_Called := True;
         return 77;
      end Get_Default;

      function Unwrap_With_Default is new Int_Option.Unwrap_Or_With
        (F => Get_Default);

      Opt : constant Int_Option.Option := Int_Option.Of_Value (42);
      Val : constant Integer           := Unwrap_With_Default (Opt);
   begin
      Run_Test
        ("Unwrap_Or_With with Some returns value and doesn't call F",
         Val = 42 and then not Default_Called);
   end;

   --  ========================================================================
   --  Test: Unwrap_Or_With with None calls F
   --  ========================================================================

   declare
      Default_Called : Boolean := False;

      function Get_Default return Integer is
      begin
         Default_Called := True;
         return 77;
      end Get_Default;

      function Unwrap_With_Default is new Int_Option.Unwrap_Or_With
        (F => Get_Default);

      Opt : constant Int_Option.Option := Int_Option.None;
      Val : constant Integer           := Unwrap_With_Default (Opt);
   begin
      Run_Test
        ("Unwrap_Or_With with None calls F and returns its value",
         Val = 77 and then Default_Called);
   end;

   --  ========================================================================
   --  Test: Map with Some transforms value
   --  ========================================================================

   declare
      function Double (X : Integer) return Integer is (X * 2);
      function Map_Double is new Int_Option.Map (F => Double);

      Opt    : constant Int_Option.Option := Int_Option.Of_Value (21);
      Mapped : constant Int_Option.Option := Map_Double (Opt);
   begin
      Run_Test
        ("Map with Some transforms value",
         Int_Option.Is_Some (Mapped) and then
         Int_Option.Value (Mapped) = 42);
   end;

   --  ========================================================================
   --  Test: Map with None propagates None
   --  ========================================================================

   declare
      function Double (X : Integer) return Integer is (X * 2);
      function Map_Double is new Int_Option.Map (F => Double);

      Opt    : constant Int_Option.Option := Int_Option.None;
      Mapped : constant Int_Option.Option := Map_Double (Opt);
   begin
      Run_Test
        ("Map with None propagates None",
         Int_Option.Is_None (Mapped));
   end;

   --  ========================================================================
   --  Test: And_Then with Some chains to another Some
   --  ========================================================================

   declare
      function Half_If_Even (X : Integer) return Int_Option.Option is
      begin
         if X mod 2 = 0 then
            return Int_Option.Of_Value (X / 2);
         else
            return Int_Option.None;
         end if;
      end Half_If_Even;

      function And_Then_Half is new Int_Option.And_Then (F => Half_If_Even);

      Opt    : constant Int_Option.Option := Int_Option.Of_Value (42);
      Result : constant Int_Option.Option := And_Then_Half (Opt);
   begin
      Run_Test
        ("And_Then with Some chains to another Some",
         Int_Option.Is_Some (Result) and then
         Int_Option.Value (Result) = 21);
   end;

   --  ========================================================================
   --  Test: And_Then with Some chains to None
   --  ========================================================================

   declare
      function Half_If_Even (X : Integer) return Int_Option.Option is
      begin
         if X mod 2 = 0 then
            return Int_Option.Of_Value (X / 2);
         else
            return Int_Option.None;
         end if;
      end Half_If_Even;

      function And_Then_Half is new Int_Option.And_Then (F => Half_If_Even);

      Opt    : constant Int_Option.Option := Int_Option.Of_Value (43);
      Result : constant Int_Option.Option := And_Then_Half (Opt);
   begin
      Run_Test
        ("And_Then with Some chains to None",
         Int_Option.Is_None (Result));
   end;

   --  ========================================================================
   --  Test: And_Then with None propagates without calling F
   --  ========================================================================

   declare
      F_Called : Boolean := False;

      function Half_If_Even (X : Integer) return Int_Option.Option is
      begin
         F_Called := True;
         return Int_Option.Of_Value (X / 2);
      end Half_If_Even;

      function And_Then_Half is new Int_Option.And_Then (F => Half_If_Even);

      Opt    : constant Int_Option.Option := Int_Option.None;
      Result : constant Int_Option.Option := And_Then_Half (Opt);
   begin
      Run_Test
        ("And_Then with None propagates without calling F",
         not F_Called and then Int_Option.Is_None (Result));
   end;

   --  ========================================================================
   --  Test: Filter with Some and true predicate keeps value
   --  ========================================================================

   declare
      function Is_Even (X : Integer) return Boolean is (X mod 2 = 0);
      function Filter_Even is new Int_Option.Filter (Pred => Is_Even);

      Opt      : constant Int_Option.Option := Int_Option.Of_Value (42);
      Filtered : constant Int_Option.Option := Filter_Even (Opt);
   begin
      Run_Test
        ("Filter with Some and true predicate keeps value",
         Int_Option.Is_Some (Filtered) and then
         Int_Option.Value (Filtered) = 42);
   end;

   --  ========================================================================
   --  Test: Filter with Some and false predicate returns None
   --  ========================================================================

   declare
      function Is_Even (X : Integer) return Boolean is (X mod 2 = 0);
      function Filter_Even is new Int_Option.Filter (Pred => Is_Even);

      Opt      : constant Int_Option.Option := Int_Option.Of_Value (43);
      Filtered : constant Int_Option.Option := Filter_Even (Opt);
   begin
      Run_Test
        ("Filter with Some and false predicate returns None",
         Int_Option.Is_None (Filtered));
   end;

   --  ========================================================================
   --  Test: Filter with None propagates None
   --  ========================================================================

   declare
      F_Called : Boolean := False;

      function Is_Even (X : Integer) return Boolean is
      begin
         F_Called := True;
         return X mod 2 = 0;
      end Is_Even;

      function Filter_Even is new Int_Option.Filter (Pred => Is_Even);

      Opt      : constant Int_Option.Option := Int_Option.None;
      Filtered : constant Int_Option.Option := Filter_Even (Opt);
   begin
      Run_Test
        ("Filter with None propagates None without calling predicate",
         not F_Called and then Int_Option.Is_None (Filtered));
   end;

   --  ========================================================================
   --  Test: Or_Else with Some primary returns primary
   --  ========================================================================

   declare
      Primary     : constant Int_Option.Option := Int_Option.Of_Value (42);
      Alternative : constant Int_Option.Option := Int_Option.Of_Value (99);
      Result      : constant Int_Option.Option :=
        Int_Option.Or_Else (Primary, Alternative);
   begin
      Run_Test
        ("Or_Else with Some primary returns primary",
         Int_Option.Is_Some (Result) and then
         Int_Option.Value (Result) = 42);
   end;

   --  ========================================================================
   --  Test: Or_Else with None primary returns alternative
   --  ========================================================================

   declare
      Primary     : constant Int_Option.Option := Int_Option.None;
      Alternative : constant Int_Option.Option := Int_Option.Of_Value (99);
      Result      : constant Int_Option.Option :=
        Int_Option.Or_Else (Primary, Alternative);
   begin
      Run_Test
        ("Or_Else with None primary returns alternative",
         Int_Option.Is_Some (Result) and then
         Int_Option.Value (Result) = 99);
   end;

   --  ========================================================================
   --  Test: Or_Else_With with Some doesn't call F
   --  ========================================================================

   declare
      F_Called : Boolean := False;

      function Get_Alternative return Int_Option.Option is
      begin
         F_Called := True;
         return Int_Option.Of_Value (99);
      end Get_Alternative;

      function Or_Else_With_Alt is new Int_Option.Or_Else_With
        (F => Get_Alternative);

      Opt    : constant Int_Option.Option := Int_Option.Of_Value (42);
      Result : constant Int_Option.Option := Or_Else_With_Alt (Opt);
   begin
      Run_Test
        ("Or_Else_With with Some doesn't call F",
         Int_Option.Is_Some (Result) and then
         Int_Option.Value (Result) = 42 and then not F_Called);
   end;

   --  ========================================================================
   --  Test: Or_Else_With with None calls F
   --  ========================================================================

   declare
      F_Called : Boolean := False;

      function Get_Alternative return Int_Option.Option is
      begin
         F_Called := True;
         return Int_Option.Of_Value (99);
      end Get_Alternative;

      function Or_Else_With_Alt is new Int_Option.Or_Else_With
        (F => Get_Alternative);

      Opt    : constant Int_Option.Option := Int_Option.None;
      Result : constant Int_Option.Option := Or_Else_With_Alt (Opt);
   begin
      Run_Test
        ("Or_Else_With with None calls F",
         Int_Option.Is_Some (Result) and then
         Int_Option.Value (Result) = 99 and then F_Called);
   end;

   --  ========================================================================
   --  Test: Chaining multiple operations (Map then Filter)
   --  ========================================================================

   declare
      function Double (X : Integer) return Integer is (X * 2);
      function Map_Double is new Int_Option.Map (F => Double);

      function Greater_Than_50 (X : Integer) return Boolean is (X > 50);
      function Filter_GT_50 is new Int_Option.Filter (Pred => Greater_Than_50);

      Opt    : constant Int_Option.Option := Int_Option.Of_Value (30);
      Step1  : constant Int_Option.Option := Map_Double (Opt);
      Result : constant Int_Option.Option := Filter_GT_50 (Step1);
   begin
      Run_Test
        ("Chaining Map then Filter - 30 * 2 = 60 > 50",
         Int_Option.Is_Some (Result) and then
         Int_Option.Value (Result) = 60);
   end;

   --  ========================================================================
   --  Test: Chaining with None in the middle (Map then None then Filter)
   --  ========================================================================

   declare
      function Double (X : Integer) return Integer is (X * 2);
      function Map_Double is new Int_Option.Map (F => Double);

      function Less_Than_50 (X : Integer) return Boolean is (X < 50);
      function Filter_LT_50 is new Int_Option.Filter (Pred => Less_Than_50);

      Opt    : constant Int_Option.Option := Int_Option.Of_Value (30);
      Step1  : constant Int_Option.Option := Map_Double (Opt);
      Result : constant Int_Option.Option := Filter_LT_50 (Step1);
   begin
      Run_Test
        ("Chaining with filter that fails - 30 * 2 = 60 not < 50",
         Int_Option.Is_None (Result));
   end;

   --  ========================================================================
   --  Test: Option with zero value (boundary test)
   --  ========================================================================

   declare
      Opt : constant Int_Option.Option := Int_Option.Of_Value (0);
   begin
      Run_Test
        ("Option with zero value - Is_Some",
         Int_Option.Is_Some (Opt));
      if Int_Option.Is_Some (Opt) then
         Run_Test
           ("Option with zero value - correct value",
            Int_Option.Value (Opt) = 0);
      end if;
   end;

   --  ========================================================================
   --  Test: Option with negative value
   --  ========================================================================

   declare
      Opt : constant Int_Option.Option := Int_Option.Of_Value (-42);
   begin
      Run_Test
        ("Option with negative value - Is_Some",
         Int_Option.Is_Some (Opt));
      if Int_Option.Is_Some (Opt) then
         Run_Test
           ("Option with negative value - correct value",
            Int_Option.Value (Opt) = -42);
      end if;
   end;

   --  ========================================================================
   --  Test: Complex chain with And_Then and Map
   --  ========================================================================

   declare
      function Half_If_Even (X : Integer) return Int_Option.Option is
      begin
         if X mod 2 = 0 then
            return Int_Option.Of_Value (X / 2);
         else
            return Int_Option.None;
         end if;
      end Half_If_Even;

      function And_Then_Half is new Int_Option.And_Then (F => Half_If_Even);

      function Add_Ten (X : Integer) return Integer is (X + 10);
      function Map_Add_Ten is new Int_Option.Map (F => Add_Ten);

      Opt    : constant Int_Option.Option := Int_Option.Of_Value (100);
      Step1  : constant Int_Option.Option := And_Then_Half (Opt);
      Result : constant Int_Option.Option := Map_Add_Ten (Step1);
   begin
      Run_Test
        ("Complex chain - 100 / 2 = 50, then 50 + 10 = 60",
         Int_Option.Is_Some (Result) and then
         Int_Option.Value (Result) = 60);
   end;

   --  Print summary
   New_Line;
   Put_Line ("========================================");
   Put_Line ("Test Summary: Domain.Value_Object.Option");
   Put_Line ("========================================");
   Put_Line ("Total tests: " & Total_Tests'Image);
   Put_Line ("Passed:      " & Passed_Tests'Image);
   Put_Line ("Failed:      " & Natural'Image (Total_Tests - Passed_Tests));
   New_Line;

   --  Register results with test framework
   Test_Framework.Register_Results (Total_Tests, Passed_Tests);

end Test_Domain_Option;
