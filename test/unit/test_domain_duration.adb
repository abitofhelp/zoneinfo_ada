pragma Ada_2022;
--  ======================================================================
--  Test_Domain_Duration
--  ======================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--
--  Purpose:
--    Unit tests for Domain.Value_Object.Duration_Type functionality.
--    Tests creation, arithmetic, and accessors.
--  ======================================================================

with Ada.Text_IO;
with Domain.Value_Object.Duration_Type;
with Test_Framework;

procedure Test_Domain_Duration is

   use Ada.Text_IO;
   use Domain.Value_Object.Duration_Type;
   use type Integer_64;

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
   Put_Line ("Testing: Domain.Value_Object.Duration_Type");
   Put_Line ("========================================");
   New_Line;

   --  ========================================================================
   --  Test: Zero duration
   --  ========================================================================

   Put_Line ("Test: Zero Duration");

   declare
      Z : constant Duration_Type := Zero;
   begin
      Run_Test ("Zero.Get_Seconds = 0", Get_Seconds (Z) = 0);
      Run_Test ("Zero.Get_Nanoseconds = 0", Get_Nanoseconds (Z) = 0);
      Run_Test ("Zero.Is_Negative = False", not Is_Negative (Z));
   end;

   --  ========================================================================
   --  Test: Duration from seconds
   --  ========================================================================

   Put_Line ("Test: From_Seconds");

   declare
      D : constant Duration_Type := From_Seconds (3600);  --  1 hour
   begin
      Run_Test
        ("From_Seconds(3600).Get_Seconds = 3600", Get_Seconds (D) = 3600);
      Run_Test
        ("From_Seconds(3600).Get_Nanoseconds = 0", Get_Nanoseconds (D) = 0);
   end;

   declare
      D : constant Duration_Type := From_Seconds (-3600);  --  -1 hour
   begin
      Run_Test
        ("From_Seconds(-3600).Get_Seconds = -3600",
         Get_Seconds (D) = -3600);
      Run_Test ("From_Seconds(-3600).Is_Negative", Is_Negative (D));
   end;

   --  ========================================================================
   --  Test: Duration from total nanoseconds
   --  ========================================================================

   Put_Line ("Test: From_Nanos");

   declare
      D : constant Duration_Type := From_Nanos (1_500_000_000);  --  1.5 sec
   begin
      Run_Test ("From_Nanos(1.5e9).Get_Seconds = 1", Get_Seconds (D) = 1);
      Run_Test
        ("From_Nanos(1.5e9).Get_Nanoseconds = 500000000",
         Get_Nanoseconds (D) = 500_000_000);
   end;

   declare
      D : constant Duration_Type := From_Nanos (500_000_000);  --  0.5 sec
   begin
      Run_Test ("From_Nanos(0.5e9).Get_Seconds = 0", Get_Seconds (D) = 0);
      Run_Test
        ("From_Nanos(0.5e9).Get_Nanoseconds = 500000000",
         Get_Nanoseconds (D) = 500_000_000);
   end;

   --  ========================================================================
   --  Test: Duration from components
   --  ========================================================================

   Put_Line ("Test: Create");

   declare
      D : constant Duration_Type := Create (10, 123_456_789);
   begin
      Run_Test ("Create(10, 123456789).Seconds = 10", Get_Seconds (D) = 10);
      Run_Test
        ("Create(10, 123456789).Nanos = 123456789",
         Get_Nanoseconds (D) = 123_456_789);
   end;

   --  ========================================================================
   --  Test: Total nanoseconds accessor
   --  ========================================================================

   Put_Line ("Test: To_Nanos");

   declare
      D     : constant Duration_Type := Create (2, 500_000_000);
      Total : constant Integer_64 := To_Nanos (D);
   begin
      Run_Test
        ("To_Nanos(2.5s) = 2500000000",
         Total = 2_500_000_000);
   end;

   --  ========================================================================
   --  Test: Duration equality
   --  ========================================================================

   Put_Line ("Test: Duration Equality");

   declare
      D1 : constant Duration_Type := From_Seconds (100);
      D2 : constant Duration_Type := From_Seconds (100);
      D3 : constant Duration_Type := From_Seconds (200);
   begin
      Run_Test ("Equal durations", D1 = D2);
      Run_Test ("Different durations", not (D1 = D3));
   end;

   --  ========================================================================
   --  Test: UTC offset durations
   --  ========================================================================

   Put_Line ("Test: UTC Offset Durations");

   declare
      Plus_5 : constant Duration_Type := From_Seconds (5 * 3600);  --  UTC+5
      Minus_8 : constant Duration_Type := From_Seconds (-8 * 3600);  --  UTC-8
   begin
      Run_Test ("UTC+5 = 18000 seconds", Get_Seconds (Plus_5) = 18000);
      Run_Test ("UTC-8 = -28800 seconds", Get_Seconds (Minus_8) = -28800);
      Run_Test ("UTC+5 not negative", not Is_Negative (Plus_5));
      Run_Test ("UTC-8 is negative", Is_Negative (Minus_8));
   end;

   --  ========================================================================
   --  Test: From_Millis with positive and negative values
   --  ========================================================================

   Put_Line ("Test: From_Millis");

   declare
      D : constant Duration_Type := From_Millis (1500);  --  1.5 seconds
   begin
      Run_Test ("From_Millis(1500).Seconds = 1", Get_Seconds (D) = 1);
      Run_Test
        ("From_Millis(1500).Nanos = 500000000",
         Get_Nanoseconds (D) = 500_000_000);
   end;

   declare
      D : constant Duration_Type := From_Millis (-1500);  --  -1.5 seconds
   begin
      Run_Test ("From_Millis(-1500).Seconds = -2", Get_Seconds (D) = -2);
      Run_Test
        ("From_Millis(-1500).Nanos = 500000000",
         Get_Nanoseconds (D) = 500_000_000);
   end;

   --  ========================================================================
   --  Test: From_Micros with positive and negative values
   --  ========================================================================

   Put_Line ("Test: From_Micros");

   declare
      D : constant Duration_Type := From_Micros (1_500_000);  --  1.5 seconds
   begin
      Run_Test ("From_Micros(1500000).Seconds = 1", Get_Seconds (D) = 1);
      Run_Test
        ("From_Micros(1500000).Nanos = 500000000",
         Get_Nanoseconds (D) = 500_000_000);
   end;

   declare
      D : constant Duration_Type := From_Micros (-1_500_000);  --  -1.5 seconds
   begin
      Run_Test ("From_Micros(-1500000).Seconds = -2", Get_Seconds (D) = -2);
      Run_Test
        ("From_Micros(-1500000).Nanos = 500000000",
         Get_Nanoseconds (D) = 500_000_000);
   end;

   --  ========================================================================
   --  Test: From_Nanos with negative values
   --  ========================================================================

   Put_Line ("Test: From_Nanos Negative");

   declare
      D : constant Duration_Type := From_Nanos (-1_500_000_000);  --  -1.5 sec
   begin
      Run_Test ("From_Nanos(-1.5e9).Seconds = -2", Get_Seconds (D) = -2);
      Run_Test
        ("From_Nanos(-1.5e9).Nanos = 500000000",
         Get_Nanoseconds (D) = 500_000_000);
   end;

   --  ========================================================================
   --  Test: To_Seconds conversion
   --  ========================================================================

   Put_Line ("Test: To_Seconds");

   declare
      D : constant Duration_Type := Create (10, 123_456_789);
      S : constant Integer_64 := To_Seconds (D);
   begin
      Run_Test ("To_Seconds(10.123s) = 10", S = 10);
   end;

   --  ========================================================================
   --  Test: To_Millis conversion
   --  ========================================================================

   Put_Line ("Test: To_Millis");

   declare
      D  : constant Duration_Type := Create (2, 500_000_000);
      Ms : constant Integer_64 := To_Millis (D);
   begin
      Run_Test ("To_Millis(2.5s) = 2500", Ms = 2500);
   end;

   declare
      D  : constant Duration_Type := Create (1, 234_000_000);
      Ms : constant Integer_64 := To_Millis (D);
   begin
      Run_Test ("To_Millis(1.234s) = 1234", Ms = 1234);
   end;

   --  ========================================================================
   --  Test: Add (two durations)
   --  ========================================================================

   Put_Line ("Test: Add Durations");

   declare
      D1     : constant Duration_Type := From_Seconds (100);
      D2     : constant Duration_Type := From_Seconds (50);
      Result : constant Duration_Type := Add (D1, D2);
   begin
      Run_Test ("Add(100s, 50s) = 150s", Get_Seconds (Result) = 150);
   end;

   declare
      D1     : constant Duration_Type := Create (1, 600_000_000);
      D2     : constant Duration_Type := Create (2, 500_000_000);
      Result : constant Duration_Type := Add (D1, D2);
   begin
      Run_Test ("Add(1.6s, 2.5s).Seconds = 4", Get_Seconds (Result) = 4);
      Run_Test
        ("Add(1.6s, 2.5s).Nanos = 100000000",
         Get_Nanoseconds (Result) = 100_000_000);
   end;

   --  ========================================================================
   --  Test: Subtract (two durations)
   --  ========================================================================

   Put_Line ("Test: Subtract Durations");

   declare
      D1     : constant Duration_Type := From_Seconds (200);
      D2     : constant Duration_Type := From_Seconds (50);
      Result : constant Duration_Type := Subtract (D1, D2);
   begin
      Run_Test ("Subtract(200s, 50s) = 150s", Get_Seconds (Result) = 150);
   end;

   declare
      D1     : constant Duration_Type := Create (5, 300_000_000);
      D2     : constant Duration_Type := Create (2, 100_000_000);
      Result : constant Duration_Type := Subtract (D1, D2);
   begin
      Run_Test ("Subtract(5.3s, 2.1s).Seconds = 3", Get_Seconds (Result) = 3);
      Run_Test
        ("Subtract(5.3s, 2.1s).Nanos = 200000000",
         Get_Nanoseconds (Result) = 200_000_000);
   end;

   --  ========================================================================
   --  Test: Negate
   --  ========================================================================

   Put_Line ("Test: Negate");

   declare
      D      : constant Duration_Type := From_Seconds (100);
      Neg    : constant Duration_Type := Negate (D);
   begin
      Run_Test ("Negate(100s) = -100s", Get_Seconds (Neg) = -100);
      Run_Test ("Negate(100s) is negative", Is_Negative (Neg));
   end;

   declare
      D      : constant Duration_Type := Create (1, 500_000_000);
      Neg    : constant Duration_Type := Negate (D);
   begin
      Run_Test ("Negate(1.5s).Seconds = -2", Get_Seconds (Neg) = -2);
      Run_Test
        ("Negate(1.5s).Nanos = 500000000",
         Get_Nanoseconds (Neg) = 500_000_000);
   end;

   --  ========================================================================
   --  Test: Is_Zero
   --  ========================================================================

   Put_Line ("Test: Is_Zero");

   declare
      Z : constant Duration_Type := Zero;
      D : constant Duration_Type := From_Seconds (1);
   begin
      Run_Test ("Zero.Is_Zero = True", Is_Zero (Z));
      Run_Test ("From_Seconds(1).Is_Zero = False", not Is_Zero (D));
   end;

   declare
      D : constant Duration_Type := Create (0, 0);
   begin
      Run_Test ("Create(0, 0).Is_Zero = True", Is_Zero (D));
   end;

   --  ========================================================================
   --  Test: Duration ordering
   --  ========================================================================

   Put_Line ("Test: Duration Ordering");

   declare
      D1 : constant Duration_Type := From_Seconds (100);
      D2 : constant Duration_Type := From_Seconds (200);
   begin
      Run_Test ("100s < 200s", D1 < D2);
      Run_Test ("Not (200s < 100s)", not (D2 < D1));
   end;

   --  Test < when seconds are equal (tests nanoseconds comparison)
   declare
      D1 : constant Duration_Type := Create (10, 100_000_000);
      D2 : constant Duration_Type := Create (10, 200_000_000);
   begin
      Run_Test
        ("10.1s < 10.2s (equal seconds, different nanos)",
         D1 < D2);
   end;

   --  Test <= comparison
   declare
      D1 : constant Duration_Type := From_Seconds (100);
      D2 : constant Duration_Type := From_Seconds (200);
      D3 : constant Duration_Type := From_Seconds (100);
   begin
      Run_Test ("100s <= 200s", D1 <= D2);
      Run_Test ("100s <= 100s", D1 <= D3);
      Run_Test ("Not (200s <= 100s)", not (D2 <= D1));
   end;

   --  Test > comparison
   declare
      D1 : constant Duration_Type := From_Seconds (200);
      D2 : constant Duration_Type := From_Seconds (100);
   begin
      Run_Test ("200s > 100s", D1 > D2);
      Run_Test ("Not (100s > 200s)", not (D2 > D1));
   end;

   --  Test >= comparison
   declare
      D1 : constant Duration_Type := From_Seconds (200);
      D2 : constant Duration_Type := From_Seconds (100);
      D3 : constant Duration_Type := From_Seconds (200);
   begin
      Run_Test ("200s >= 100s", D1 >= D2);
      Run_Test ("200s >= 200s", D1 >= D3);
      Run_Test ("Not (100s >= 200s)", not (D2 >= D1));
   end;

   --  ========================================================================
   --  Test: Arithmetic Operators (+, -, unary -)
   --  ========================================================================

   Put_Line ("Test: Arithmetic Operators");

   --  Test: Duration + Duration
   declare
      D1     : constant Duration_Type := From_Seconds (100);
      D2     : constant Duration_Type := From_Seconds (50);
      Result : constant Duration_Type := D1 + D2;
   begin
      Run_Test ("(100s) + (50s) = 150s", Get_Seconds (Result) = 150);
   end;

   declare
      D1     : constant Duration_Type := Create (1, 600_000_000);
      D2     : constant Duration_Type := Create (2, 500_000_000);
      Result : constant Duration_Type := D1 + D2;
   begin
      Run_Test ("(1.6s) + (2.5s).Seconds = 4", Get_Seconds (Result) = 4);
      Run_Test
        ("(1.6s) + (2.5s).Nanos = 100000000",
         Get_Nanoseconds (Result) = 100_000_000);
   end;

   --  Test: Duration - Duration
   declare
      D1     : constant Duration_Type := From_Seconds (200);
      D2     : constant Duration_Type := From_Seconds (50);
      Result : constant Duration_Type := D1 - D2;
   begin
      Run_Test ("(200s) - (50s) = 150s", Get_Seconds (Result) = 150);
   end;

   declare
      D1     : constant Duration_Type := Create (5, 300_000_000);
      D2     : constant Duration_Type := Create (2, 100_000_000);
      Result : constant Duration_Type := D1 - D2;
   begin
      Run_Test ("(5.3s) - (2.1s).Seconds = 3", Get_Seconds (Result) = 3);
      Run_Test
        ("(5.3s) - (2.1s).Nanos = 200000000",
         Get_Nanoseconds (Result) = 200_000_000);
   end;

   --  Test: Unary negation (-Duration)
   declare
      D      : constant Duration_Type := From_Seconds (100);
      Neg    : constant Duration_Type := -D;
   begin
      Run_Test ("-(100s) = -100s", Get_Seconds (Neg) = -100);
      Run_Test ("-(100s) is negative", Is_Negative (Neg));
   end;

   declare
      D      : constant Duration_Type := Create (1, 500_000_000);
      Neg    : constant Duration_Type := -D;
   begin
      Run_Test ("-(1.5s).Seconds = -2", Get_Seconds (Neg) = -2);
      Run_Test
        ("-(1.5s).Nanos = 500000000",
         Get_Nanoseconds (Neg) = 500_000_000);
   end;

   --  Print summary
   New_Line;
   Put_Line ("========================================");
   Put_Line ("Test Summary: Domain.Value_Object.Duration_Type");
   Put_Line ("========================================");
   Put_Line ("Total tests: " & Total_Tests'Image);
   Put_Line ("Passed:      " & Passed_Tests'Image);
   Put_Line ("Failed:      " & Natural'Image (Total_Tests - Passed_Tests));
   New_Line;

   --  Register results with test framework
   Test_Framework.Register_Results (Total_Tests, Passed_Tests);

end Test_Domain_Duration;
