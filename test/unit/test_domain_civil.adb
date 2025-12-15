pragma Ada_2022;
--  ======================================================================
--  Test_Domain_Civil
--  ======================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--
--  Purpose:
--    Unit tests for Domain.Value_Object.Civil functionality.
--    Tests creation, validation, accessors, and comparisons.
--  ======================================================================

with Ada.Text_IO;
with Interfaces;
with Domain.Value_Object.Civil;
with Test_Framework;

procedure Test_Domain_Civil is

   use Ada.Text_IO;
   use Interfaces;
   use Domain.Value_Object.Civil;

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
   Put_Line ("Testing: Domain.Value_Object.Civil");
   Put_Line ("========================================");
   New_Line;

   --  ========================================================================
   --  Test: Valid Civil creation
   --  ========================================================================

   Put_Line ("Test: Valid Civil Creation");

   declare
      R : constant Civil_Result.Result := Create
        (Year       => 2025,
         Month      => 6,
         Day        => 15,
         Hour       => 14,
         Minute     => 30,
         Second     => 45,
         Nanosecond => 123_456_789);
   begin
      Run_Test ("Create valid Civil - Is_Ok", Civil_Result.Is_Ok (R));
      if Civil_Result.Is_Ok (R) then
         declare
            C : constant Civil := Civil_Result.Value (R);
         begin
            Run_Test ("Year accessor", Get_Year (C) = 2025);
            Run_Test ("Month accessor", Get_Month (C) = 6);
            Run_Test ("Day accessor", Get_Day (C) = 15);
            Run_Test ("Hour accessor", Get_Hour (C) = 14);
            Run_Test ("Minute accessor", Get_Minute (C) = 30);
            Run_Test ("Second accessor", Get_Second (C) = 45);
            Run_Test ("Nanosecond accessor", Get_Nanosecond (C) = 123_456_789);
         end;
      end if;
   end;

   --  ========================================================================
   --  Test: Epoch Civil (1970-01-01 00:00:00)
   --  ========================================================================

   Put_Line ("Test: Epoch Civil");

   declare
      R : constant Civil_Result.Result := Create
        (Year       => 1970,
         Month      => 1,
         Day        => 1,
         Hour       => 0,
         Minute     => 0,
         Second     => 0,
         Nanosecond => 0);
   begin
      Run_Test ("Epoch Civil creation", Civil_Result.Is_Ok (R));
   end;

   --  ========================================================================
   --  Test: Component ranges enforced by subtype constraints
   --  ========================================================================
   --
   --  Note: Month (1..12), Hour (0..23), Minute (0..59), Second (0..59)
   --  are enforced by Ada's subtype constraints. Passing values outside
   --  these ranges causes CONSTRAINT_ERROR at compile/runtime before
   --  Create is called. This is desirable behavior.
   --
   --  Tests for calendar validity (Feb 30, Apr 31, etc.) are below since
   --  those values are within subtype ranges but invalid for the calendar.

   --  ========================================================================
   --  Test: Invalid Civil (bad day)
   --  ========================================================================

   Put_Line ("Test: Invalid Civil - Bad Day");

   declare
      R : constant Civil_Result.Result := Create
        (Year       => 2025,
         Month      => 2,
         Day        => 30,  --  February doesn't have 30 days
         Hour       => 0,
         Minute     => 0,
         Second     => 0,
         Nanosecond => 0);
   begin
      Run_Test ("Feb 30 - Is_Error", Civil_Result.Is_Error (R));
   end;

   declare
      R : constant Civil_Result.Result := Create
        (Year       => 2025,
         Month      => 4,
         Day        => 31,  --  April has 30 days
         Hour       => 0,
         Minute     => 0,
         Second     => 0,
         Nanosecond => 0);
   begin
      Run_Test ("Apr 31 - Is_Error", Civil_Result.Is_Error (R));
   end;

   --  ========================================================================
   --  Test: Leap year handling
   --  ========================================================================

   Put_Line ("Test: Leap Year Handling");

   declare
      --  2024 is a leap year
      R : constant Civil_Result.Result := Create
        (Year       => 2024,
         Month      => 2,
         Day        => 29,  --  Valid in leap year
         Hour       => 0,
         Minute     => 0,
         Second     => 0,
         Nanosecond => 0);
   begin
      Run_Test ("Feb 29 in leap year (2024) - Is_Ok", Civil_Result.Is_Ok (R));
   end;

   declare
      --  2025 is not a leap year
      R : constant Civil_Result.Result := Create
        (Year       => 2025,
         Month      => 2,
         Day        => 29,  --  Invalid in non-leap year
         Hour       => 0,
         Minute     => 0,
         Second     => 0,
         Nanosecond => 0);
   begin
      Run_Test
        ("Feb 29 in non-leap year (2025) - Is_Error",
         Civil_Result.Is_Error (R));
   end;

   declare
      --  2000 is a leap year (divisible by 400)
      R : constant Civil_Result.Result := Create
        (Year       => 2000,
         Month      => 2,
         Day        => 29,
         Hour       => 0,
         Minute     => 0,
         Second     => 0,
         Nanosecond => 0);
   begin
      Run_Test
        ("Feb 29 in century leap year (2000) - Is_Ok",
         Civil_Result.Is_Ok (R));
   end;

   --  ========================================================================
   --  Test: Time component subtype constraints
   --  ========================================================================
   --
   --  Hour (0..23), Minute (0..59), Second (0..59) are enforced by Ada
   --  subtype constraints. Testing values outside these ranges would
   --  cause CONSTRAINT_ERROR, which is correct Ada behavior.

   --  ========================================================================
   --  Test: Is_Leap_Year function
   --  ========================================================================

   Put_Line ("Test: Is_Leap_Year");

   declare
   begin
      Run_Test ("2024 is a leap year", Is_Leap_Year (2024));
      Run_Test ("2025 is not a leap year", not Is_Leap_Year (2025));
      Run_Test ("2000 is a leap year (div by 400)", Is_Leap_Year (2000));
      Run_Test
        ("1900 is not a leap year (div by 100)", not Is_Leap_Year (1900));
      Run_Test
        ("2100 is not a leap year (div by 100)", not Is_Leap_Year (2100));
      Run_Test ("2400 is a leap year (div by 400)", Is_Leap_Year (2400));
   end;

   --  ========================================================================
   --  Test: Days_In_Month function
   --  ========================================================================

   Put_Line ("Test: Days_In_Month");

   declare
   begin
      --  31-day months
      Run_Test ("January has 31 days", Days_In_Month (2025, 1) = 31);
      Run_Test ("March has 31 days", Days_In_Month (2025, 3) = 31);
      Run_Test ("May has 31 days", Days_In_Month (2025, 5) = 31);
      Run_Test ("July has 31 days", Days_In_Month (2025, 7) = 31);
      Run_Test ("August has 31 days", Days_In_Month (2025, 8) = 31);
      Run_Test ("October has 31 days", Days_In_Month (2025, 10) = 31);
      Run_Test ("December has 31 days", Days_In_Month (2025, 12) = 31);

      --  30-day months
      Run_Test ("April has 30 days", Days_In_Month (2025, 4) = 30);
      Run_Test ("June has 30 days", Days_In_Month (2025, 6) = 30);
      Run_Test ("September has 30 days", Days_In_Month (2025, 9) = 30);
      Run_Test ("November has 30 days", Days_In_Month (2025, 11) = 30);

      --  February (leap and non-leap)
      Run_Test
        ("Feb 2025 (non-leap) has 28 days", Days_In_Month (2025, 2) = 28);
      Run_Test ("Feb 2024 (leap) has 29 days", Days_In_Month (2024, 2) = 29);
      Run_Test ("Feb 2000 (leap) has 29 days", Days_In_Month (2000, 2) = 29);
      Run_Test
        ("Feb 1900 (non-leap) has 28 days", Days_In_Month (1900, 2) = 28);
   end;

   --  ========================================================================
   --  Test: Civil equality
   --  ========================================================================

   Put_Line ("Test: Civil Equality");

   declare
      R1 : constant Civil_Result.Result := Create
        (2025, 6, 15, 10, 30, 0, 0);
      R2 : constant Civil_Result.Result := Create
        (2025, 6, 15, 10, 30, 0, 0);
      R3 : constant Civil_Result.Result := Create
        (2025, 6, 15, 10, 30, 0, 1);  --  Different nanosecond
   begin
      if Civil_Result.Is_Ok (R1) and then Civil_Result.Is_Ok (R2) and then
         Civil_Result.Is_Ok (R3)
      then
         Run_Test
           ("Equal Civil values",
            Civil_Result.Value (R1) = Civil_Result.Value (R2));
         Run_Test
           ("Different Civil values",
            not (Civil_Result.Value (R1) = Civil_Result.Value (R3)));
      else
         Run_Test ("Civil equality test setup", False);
      end if;
   end;

   --  ========================================================================
   --  Test: Civil comparison operators
   --  ========================================================================

   Put_Line ("Test: Civil Comparison Operators");

   declare
      R1 : constant Civil_Result.Result := Create
        (2025, 6, 15, 10, 30, 0, 0);
      R2 : constant Civil_Result.Result := Create
        (2025, 6, 15, 10, 30, 1, 0);  --  1 second later
      R3 : constant Civil_Result.Result := Create
        (2025, 6, 16, 10, 30, 0, 0);  --  1 day later
   begin
      if Civil_Result.Is_Ok (R1) and then Civil_Result.Is_Ok (R2) and then
         Civil_Result.Is_Ok (R3)
      then
         declare
            C1 : constant Civil := Civil_Result.Value (R1);
            C2 : constant Civil := Civil_Result.Value (R2);
            C3 : constant Civil := Civil_Result.Value (R3);
         begin
            --  Test "<" operator
            Run_Test ("C1 < C2 (1 second difference)", C1 < C2);
            Run_Test ("C1 < C3 (1 day difference)", C1 < C3);
            Run_Test ("Not (C2 < C1)", not (C2 < C1));

            --  Test "<=" operator
            Run_Test ("C1 <= C2", C1 <= C2);
            Run_Test ("C1 <= C1", C1 <= C1);
            Run_Test ("Not (C2 <= C1)", not (C2 <= C1));

            --  Test ">" operator
            Run_Test ("C2 > C1", C2 > C1);
            Run_Test ("C3 > C1", C3 > C1);
            Run_Test ("Not (C1 > C2)", not (C1 > C2));

            --  Test ">=" operator
            Run_Test ("C2 >= C1", C2 >= C1);
            Run_Test ("C1 >= C1", C1 >= C1);
            Run_Test ("Not (C1 >= C2)", not (C1 >= C2));
         end;
      else
         Run_Test ("Civil comparison test setup", False);
      end if;
   end;

   --  Test comparison with different years
   declare
      R1 : constant Civil_Result.Result := Create
        (2024, 12, 31, 23, 59, 59, 999_999_999);
      R2 : constant Civil_Result.Result := Create
        (2025, 1, 1, 0, 0, 0, 0);
   begin
      if Civil_Result.Is_Ok (R1) and then Civil_Result.Is_Ok (R2) then
         declare
            C1 : constant Civil := Civil_Result.Value (R1);
            C2 : constant Civil := Civil_Result.Value (R2);
         begin
            Run_Test ("2024-12-31 23:59:59 < 2025-01-01 00:00:00", C1 < C2);
         end;
      else
         Run_Test ("Year comparison test setup", False);
      end if;
   end;

   --  Test comparison with nanosecond precision
   declare
      R1 : constant Civil_Result.Result := Create
        (2025, 6, 15, 10, 30, 0, 100);
      R2 : constant Civil_Result.Result := Create
        (2025, 6, 15, 10, 30, 0, 200);
   begin
      if Civil_Result.Is_Ok (R1) and then Civil_Result.Is_Ok (R2) then
         declare
            C1 : constant Civil := Civil_Result.Value (R1);
            C2 : constant Civil := Civil_Result.Value (R2);
         begin
            Run_Test
              ("Nanosecond comparison (100ns < 200ns)",
               C1 < C2);
         end;
      else
         Run_Test ("Nanosecond comparison test setup", False);
      end if;
   end;

   --  Print summary
   New_Line;
   Put_Line ("========================================");
   Put_Line ("Test Summary: Domain.Value_Object.Civil");
   Put_Line ("========================================");
   Put_Line ("Total tests: " & Total_Tests'Image);
   Put_Line ("Passed:      " & Passed_Tests'Image);
   Put_Line ("Failed:      " & Natural'Image (Total_Tests - Passed_Tests));
   New_Line;

   --  Register results with test framework
   Test_Framework.Register_Results (Total_Tests, Passed_Tests);

end Test_Domain_Civil;
