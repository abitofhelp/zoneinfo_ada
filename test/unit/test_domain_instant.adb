pragma Ada_2022;
--  ======================================================================
--  Test_Domain_Instant
--  ======================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--
--  Purpose:
--    Unit tests for Domain.Value_Object.Instant functionality.
--    Tests creation, epoch nanosecond conversion, and comparisons.
--  ======================================================================

with Ada.Text_IO;
with Interfaces;
with Domain.Value_Object.Instant;
with Domain.Value_Object.Duration_Type;
with Test_Framework;

procedure Test_Domain_Instant is

   use Ada.Text_IO;
   use Interfaces;
   use Domain.Value_Object.Instant;

   package Duration_Pkg renames Domain.Value_Object.Duration_Type;

   --  Test statistics
   Total_Tests  : Natural := 0;
   Passed_Tests : Natural := 0;

   procedure Run_Test (Name : String; Passed : Boolean) is
   begin
      Total_Tests := Total_Tests + 1;
      if Passed then
         Passed_Tests := Passed_Tests + 1;
         Put_Line ("  [PASS] " & Name);
      else
         Put_Line ("  [FAIL] " & Name);
      end if;
   end Run_Test;

begin
   Put_Line ("========================================");
   Put_Line ("Testing: Domain.Value_Object.Instant");
   Put_Line ("========================================");
   New_Line;

   --  ========================================================================
   --  Test: Create Instant from epoch nanoseconds
   --  ========================================================================

   Put_Line ("Test: Instant Creation");

   declare
      I : constant Instant := From_Epoch_Nanos (0);
   begin
      Run_Test
        ("From_Epoch_Nanos(0) creates epoch instant",
         Get_Epoch_Nanos (I) = 0);
   end;

   declare
      I : constant Instant := From_Epoch_Nanos (1_000_000_000);
   begin
      Run_Test
        ("From_Epoch_Nanos(1e9) creates 1-second instant",
         Get_Epoch_Nanos (I) = 1_000_000_000);
   end;

   declare
      --  Test negative epoch (before Unix epoch)
      I : constant Instant := From_Epoch_Nanos (-1_000_000_000);
   begin
      Run_Test
        ("Negative epoch nanos (before 1970)",
         Get_Epoch_Nanos (I) = -1_000_000_000);
   end;

   --  ========================================================================
   --  Test: Instant equality
   --  ========================================================================

   Put_Line ("Test: Instant Equality");

   declare
      I1 : constant Instant := From_Epoch_Nanos (12345);
      I2 : constant Instant := From_Epoch_Nanos (12345);
      I3 : constant Instant := From_Epoch_Nanos (54321);
   begin
      Run_Test ("Equal instants compare equal", I1 = I2);
      Run_Test ("Different instants not equal", not (I1 = I3));
   end;

   --  ========================================================================
   --  Test: Instant ordering
   --  ========================================================================

   Put_Line ("Test: Instant Ordering");

   declare
      Earlier : constant Instant := From_Epoch_Nanos (100);
      Later   : constant Instant := From_Epoch_Nanos (200);
   begin
      Run_Test ("Earlier < Later", Earlier < Later);
      Run_Test ("Not (Later < Earlier)", not (Later < Earlier));
      Run_Test ("Not (Earlier < Earlier)", not (Earlier < Earlier));
   end;

   --  ========================================================================
   --  Test: Instant Result type
   --  ========================================================================

   Put_Line ("Test: Instant_Result");

   declare
      I : constant Instant := From_Epoch_Nanos (999);
      R : constant Instant_Result.Result := Instant_Result.Ok (I);
   begin
      Run_Test ("Instant_Result Ok", Instant_Result.Is_Ok (R));
      Run_Test
        ("Instant_Result value correct",
         Get_Epoch_Nanos (Instant_Result.Value (R)) = 999);
   end;

   --  ========================================================================
   --  Test: Large epoch values
   --  ========================================================================

   Put_Line ("Test: Large Epoch Values");

   declare
      --  Year 2100 approximately (in nanoseconds)
      Large_Nanos : constant := 4_102_444_800_000_000_000;
      I           : constant Instant := From_Epoch_Nanos (Large_Nanos);
   begin
      Run_Test
        ("Large epoch value (year 2100)",
         Get_Epoch_Nanos (I) = Large_Nanos);
   end;

   --  ========================================================================
   --  Test: To_Unix_Epoch conversion
   --  ========================================================================

   Put_Line ("Test: To_Unix_Epoch");

   declare
      I     : constant Instant := From_Epoch_Nanos (1_500_000_000);
      Epoch : constant Unix_Epoch_Type := To_Unix_Epoch (I);
   begin
      Run_Test ("To_Unix_Epoch(1.5s).Seconds = 1", Epoch.Seconds = 1);
      Run_Test
        ("To_Unix_Epoch(1.5s).Nanos = 500000000",
         Epoch.Nanoseconds = 500_000_000);
   end;

   declare
      I     : constant Instant := From_Epoch_Nanos (-1_500_000_000);
      Epoch : constant Unix_Epoch_Type := To_Unix_Epoch (I);
   begin
      Run_Test ("To_Unix_Epoch(-1.5s).Seconds = -2", Epoch.Seconds = -2);
      Run_Test
        ("To_Unix_Epoch(-1.5s).Nanos = 500000000",
         Epoch.Nanoseconds = 500_000_000);
   end;

   --  ========================================================================
   --  Test: Add_Duration operation
   --  ========================================================================

   Put_Line ("Test: Add Duration to Instant");

   declare
      I      : constant Instant := From_Epoch_Nanos (1000);
      D      : constant Duration_Pkg.Duration_Type :=
        Duration_Pkg.From_Seconds (5);
      Result : constant Instant_Result.Result := Add (I, D);
   begin
      Run_Test
        ("Add duration to instant - Is_Ok", Instant_Result.Is_Ok (Result));
      if Instant_Result.Is_Ok (Result) then
         Run_Test
           ("Add(1000ns, 5s) = 5000000001000ns",
            Get_Epoch_Nanos (Instant_Result.Value (Result)) = 5_000_001_000);
      end if;
   end;

   declare
      I      : constant Instant := From_Epoch_Nanos (5_000_000_000);
      D      : constant Duration_Pkg.Duration_Type :=
        Duration_Pkg.From_Nanos (1_500_000_000);
      Result : constant Instant_Result.Result := Add (I, D);
   begin
      Run_Test ("Add(5s, 1.5s) - Is_Ok", Instant_Result.Is_Ok (Result));
      if Instant_Result.Is_Ok (Result) then
         Run_Test
           ("Add(5s, 1.5s) = 6.5s",
            Get_Epoch_Nanos (Instant_Result.Value (Result)) = 6_500_000_000);
      end if;
   end;

   --  ========================================================================
   --  Test: Subtract_Duration operation
   --  ========================================================================

   Put_Line ("Test: Subtract Duration from Instant");

   declare
      I      : constant Instant := From_Epoch_Nanos (10_000_000_000);
      D      : constant Duration_Pkg.Duration_Type :=
        Duration_Pkg.From_Seconds (5);
      Result : constant Instant_Result.Result := Subtract (I, D);
   begin
      Run_Test ("Subtract duration - Is_Ok", Instant_Result.Is_Ok (Result));
      if Instant_Result.Is_Ok (Result) then
         Run_Test
           ("Subtract(10s, 5s) = 5s",
            Get_Epoch_Nanos (Instant_Result.Value (Result)) = 5_000_000_000);
      end if;
   end;

   declare
      I      : constant Instant := From_Epoch_Nanos (6_500_000_000);
      D      : constant Duration_Pkg.Duration_Type :=
        Duration_Pkg.From_Nanos (1_500_000_000);
      Result : constant Instant_Result.Result := Subtract (I, D);
   begin
      Run_Test ("Subtract(6.5s, 1.5s) - Is_Ok", Instant_Result.Is_Ok (Result));
      if Instant_Result.Is_Ok (Result) then
         Run_Test
           ("Subtract(6.5s, 1.5s) = 5s",
            Get_Epoch_Nanos (Instant_Result.Value (Result)) = 5_000_000_000);
      end if;
   end;

   --  ========================================================================
   --  Test: Diff (instant from instant)
   --  ========================================================================

   Put_Line ("Test: Diff (Instant - Instant)");

   declare
      I1       : constant Instant := From_Epoch_Nanos (1_000_000_000);
      I2       : constant Instant := From_Epoch_Nanos (6_000_000_000);
      Duration : constant Duration_Pkg.Duration_Type := Diff (I1, I2);
   begin
      Run_Test
        ("Diff(1s, 6s) = 5s",
         Duration_Pkg.To_Nanos (Duration) = 5_000_000_000);
   end;

   declare
      I1       : constant Instant := From_Epoch_Nanos (6_000_000_000);
      I2       : constant Instant := From_Epoch_Nanos (1_000_000_000);
      Duration : constant Duration_Pkg.Duration_Type := Diff (I1, I2);
   begin
      Run_Test
        ("Diff(6s, 1s) = -5s",
         Duration_Pkg.To_Nanos (Duration) = -5_000_000_000);
   end;

   --  ========================================================================
   --  Test: Arithmetic Operators (+ and -)
   --  ========================================================================

   Put_Line ("Test: Arithmetic Operators");

   --  Test: Instant + Duration
   declare
      I      : constant Instant := From_Epoch_Nanos (1000);
      D      : constant Duration_Pkg.Duration_Type :=
        Duration_Pkg.From_Seconds (5);
      Result : constant Instant_Result.Result := I + D;
   begin
      Run_Test ("Instant + Duration - Is_Ok", Instant_Result.Is_Ok (Result));
      if Instant_Result.Is_Ok (Result) then
         Run_Test
           ("(1000ns) + 5s = 5000001000ns",
            Get_Epoch_Nanos (Instant_Result.Value (Result)) = 5_000_001_000);
      end if;
   end;

   --  Test: Instant - Duration
   declare
      I      : constant Instant := From_Epoch_Nanos (10_000_000_000);
      D      : constant Duration_Pkg.Duration_Type :=
        Duration_Pkg.From_Seconds (5);
      Result : constant Instant_Result.Result := I - D;
   begin
      Run_Test ("Instant - Duration - Is_Ok", Instant_Result.Is_Ok (Result));
      if Instant_Result.Is_Ok (Result) then
         Run_Test
           ("(10s) - 5s = 5s",
            Get_Epoch_Nanos (Instant_Result.Value (Result)) = 5_000_000_000);
      end if;
   end;

   --  Test: Instant - Instant -> Duration
   declare
      I1       : constant Instant := From_Epoch_Nanos (1_000_000_000);
      I2       : constant Instant := From_Epoch_Nanos (6_000_000_000);
      Duration : constant Duration_Pkg.Duration_Type := I2 - I1;
   begin
      Run_Test
        ("(6s) - (1s) = 5s duration",
         Duration_Pkg.To_Nanos (Duration) = 5_000_000_000);
   end;

   declare
      I1       : constant Instant := From_Epoch_Nanos (6_000_000_000);
      I2       : constant Instant := From_Epoch_Nanos (1_000_000_000);
      Duration : constant Duration_Pkg.Duration_Type := I2 - I1;
   begin
      Run_Test
        ("(1s) - (6s) = -5s duration",
         Duration_Pkg.To_Nanos (Duration) = -5_000_000_000);
   end;

   --  ========================================================================
   --  Test: "<" comparison when epoch nanos are equal
   --  ========================================================================

   Put_Line ("Test: Instant Comparison Edge Cases");

   declare
      I1 : constant Instant := From_Epoch_Nanos (12345);
      I2 : constant Instant := From_Epoch_Nanos (12345);
   begin
      Run_Test ("Equal instants not < each other", not (I1 < I2));
   end;

   --  ========================================================================
   --  Test: "<=", ">", ">=" comparisons
   --  ========================================================================

   Put_Line ("Test: Additional Comparison Operators");

   declare
      Earlier : constant Instant := From_Epoch_Nanos (100);
      Later   : constant Instant := From_Epoch_Nanos (200);
      Same    : constant Instant := From_Epoch_Nanos (100);
   begin
      Run_Test ("Earlier <= Later", Earlier <= Later);
      Run_Test ("Earlier <= Same", Earlier <= Same);
      Run_Test ("Not (Later <= Earlier)", not (Later <= Earlier));

      Run_Test ("Later > Earlier", Later > Earlier);
      Run_Test ("Not (Earlier > Later)", not (Earlier > Later));
      Run_Test ("Not (Earlier > Same)", not (Earlier > Same));

      Run_Test ("Later >= Earlier", Later >= Earlier);
      Run_Test ("Earlier >= Same", Earlier >= Same);
      Run_Test ("Not (Earlier >= Later)", not (Earlier >= Later));
   end;

   --  Print summary
   New_Line;
   Put_Line ("========================================");
   Put_Line ("Test Summary: Domain.Value_Object.Instant");
   Put_Line ("========================================");
   Put_Line ("Total tests: " & Total_Tests'Image);
   Put_Line ("Passed:      " & Passed_Tests'Image);
   Put_Line ("Failed:      " & Natural'Image (Total_Tests - Passed_Tests));
   New_Line;

   --  Register results with test framework
   Test_Framework.Register_Results (Total_Tests, Passed_Tests);

end Test_Domain_Instant;
