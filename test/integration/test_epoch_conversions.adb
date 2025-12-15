pragma Ada_2022;
--  ======================================================================
--  Test_Epoch_Conversions
--  ======================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--
--  Purpose:
--    Integration tests for epoch/civil time conversions using known values.
--    Tests specific dates with verified epoch timestamps.
--    Pattern inspired by tzif test_get_transition_at_epoch.adb.
--
--  Test Data Sources:
--    Epoch values calculated from: https://www.epochconverter.com/
--    All tests use UTC timezone (stub limitation).
--  ======================================================================

with Ada.Text_IO;
with Interfaces;
with Zoneinfo.API.Desktop;
with Domain.Value_Object.Instant;
with Domain.Value_Object.Civil;
with Domain.Value_Object.Zone_ID;
with Test_Framework;

procedure Test_Epoch_Conversions is

   use Ada.Text_IO;
   use Interfaces;
   use Zoneinfo.API.Desktop;

   package Instant_Pkg renames Domain.Value_Object.Instant;
   package Civil_Pkg renames Domain.Value_Object.Civil;
   package Zone_Pkg renames Domain.Value_Object.Zone_ID;

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

   --  Helper: Get UTC zone (should never fail)
   function UTC_Zone return Zone_ID is
      R : constant Zone_Pkg.Zone_ID_Result.Result :=
        Zone_Pkg.From_String ("UTC");
   begin
      return Zone_Pkg.Zone_ID_Result.Value (R);
   end UTC_Zone;

   --  ========================================================================
   --  Known Epoch Test Cases (Table-Driven Pattern)
   --  ========================================================================
   --
   --  Each test case verifies:
   --    1. Instant -> Civil conversion (To_Civil)
   --    2. Civil -> Instant conversion (To_Instant)
   --    3. Round-trip consistency
   --  ========================================================================

   procedure Test_Known_Epoch
     (Name           : String;
      Epoch_Seconds  : Integer_64;
      Expected_Year  : Integer;
      Expected_Month : Integer;
      Expected_Day   : Integer;
      Expected_Hour  : Integer;
      Expected_Min   : Integer;
      Expected_Sec   : Integer)
   is
      Epoch_Nanos : constant Integer_64 := Epoch_Seconds * 1_000_000_000;
      I           : constant Instant :=
        Instant_Pkg.From_Epoch_Nanos (Epoch_Nanos);
      Zone        : constant Zone_ID := UTC_Zone;
      C           : constant Civil := To_Civil (I, Zone);
   begin
      Put_Line ("Test: " & Name);

      --  Verify Civil components
      Run_Test
        (Name & " - Year = " & Expected_Year'Image,
         Civil_Pkg.Get_Year (C) = Expected_Year);
      Run_Test
        (Name & " - Month = " & Expected_Month'Image,
         Civil_Pkg.Get_Month (C) = Expected_Month);
      Run_Test
        (Name & " - Day = " & Expected_Day'Image,
         Civil_Pkg.Get_Day (C) = Expected_Day);
      Run_Test
        (Name & " - Hour = " & Expected_Hour'Image,
         Civil_Pkg.Get_Hour (C) = Expected_Hour);
      Run_Test
        (Name & " - Minute = " & Expected_Min'Image,
         Civil_Pkg.Get_Minute (C) = Expected_Min);
      Run_Test
        (Name & " - Second = " & Expected_Sec'Image,
         Civil_Pkg.Get_Second (C) = Expected_Sec);

      --  Verify round-trip: Civil -> Instant
      declare
         Civil_R : constant Civil_Pkg.Civil_Result.Result :=
           Civil_Pkg.Create
             (Year       => Expected_Year,
              Month      => Expected_Month,
              Day        => Expected_Day,
              Hour       => Expected_Hour,
              Minute     => Expected_Min,
              Second     => Expected_Sec,
              Nanosecond => 0);
      begin
         if Civil_Pkg.Civil_Result.Is_Ok (Civil_R) then
            declare
               C2       : constant Civil :=
                 Civil_Pkg.Civil_Result.Value (Civil_R);
               I_Result : constant Instant_Result.Result :=
                 To_Instant (C2, Zone);
            begin
               if Instant_Result.Is_Ok (I_Result) then
                  declare
                     I2        : constant Instant :=
                       Instant_Result.Value (I_Result);
                     Got_Nanos : constant Integer_64 :=
                       Instant_Pkg.Get_Epoch_Nanos (I2);
                  begin
                     Run_Test
                       (Name & " - Round-trip epoch matches",
                        Got_Nanos = Epoch_Nanos);
                  end;
               else
                  Run_Test
                    (Name & " - Round-trip To_Instant ok", False);
               end if;
            end;
         else
            Run_Test (Name & " - Round-trip Civil creation", False);
         end if;
      end;
   end Test_Known_Epoch;

begin
   Put_Line ("========================================");
   Put_Line ("Testing: Epoch Conversions (Known Values)");
   Put_Line ("========================================");
   New_Line;

   --  ========================================================================
   --  Unix Epoch: 1970-01-01 00:00:00 UTC
   --  ========================================================================
   Test_Known_Epoch
     (Name           => "Unix Epoch (1970-01-01)",
      Epoch_Seconds  => 0,
      Expected_Year  => 1970,
      Expected_Month => 1,
      Expected_Day   => 1,
      Expected_Hour  => 0,
      Expected_Min   => 0,
      Expected_Sec   => 0);

   --  ========================================================================
   --  Y2K: 2000-01-01 00:00:00 UTC
   --  Epoch: 946684800
   --  ========================================================================
   Test_Known_Epoch
     (Name           => "Y2K (2000-01-01)",
      Epoch_Seconds  => 946_684_800,
      Expected_Year  => 2000,
      Expected_Month => 1,
      Expected_Day   => 1,
      Expected_Hour  => 0,
      Expected_Min   => 0,
      Expected_Sec   => 0);

   --  ========================================================================
   --  Leap Year Day: 2000-02-29 12:00:00 UTC
   --  (2000 is a leap year - divisible by 400)
   --  Epoch: 951825600
   --  ========================================================================
   Test_Known_Epoch
     (Name           => "Leap Day 2000 (2000-02-29)",
      Epoch_Seconds  => 951_825_600,
      Expected_Year  => 2000,
      Expected_Month => 2,
      Expected_Day   => 29,
      Expected_Hour  => 12,
      Expected_Min   => 0,
      Expected_Sec   => 0);

   --  ========================================================================
   --  2024 Leap Year: 2024-02-29 23:59:59 UTC
   --  Epoch: 1709251199
   --  ========================================================================
   Test_Known_Epoch
     (Name           => "Leap Day 2024 (2024-02-29 23:59:59)",
      Epoch_Seconds  => 1_709_251_199,
      Expected_Year  => 2024,
      Expected_Month => 2,
      Expected_Day   => 29,
      Expected_Hour  => 23,
      Expected_Min   => 59,
      Expected_Sec   => 59);

   --  ========================================================================
   --  2025-01-01 00:00:00 UTC
   --  Epoch: 1735689600
   --  ========================================================================
   Test_Known_Epoch
     (Name           => "New Year 2025 (2025-01-01)",
      Epoch_Seconds  => 1_735_689_600,
      Expected_Year  => 2025,
      Expected_Month => 1,
      Expected_Day   => 1,
      Expected_Hour  => 0,
      Expected_Min   => 0,
      Expected_Sec   => 0);

   --  ========================================================================
   --  Mid-Year: 2025-07-04 12:00:00 UTC (US Independence Day noon)
   --  Epoch: 1751630400
   --  ========================================================================
   Test_Known_Epoch
     (Name           => "July 4th 2025 Noon (2025-07-04 12:00)",
      Epoch_Seconds  => 1_751_630_400,
      Expected_Year  => 2025,
      Expected_Month => 7,
      Expected_Day   => 4,
      Expected_Hour  => 12,
      Expected_Min   => 0,
      Expected_Sec   => 0);

   --  ========================================================================
   --  End of Year: 2025-12-31 23:59:59 UTC
   --  Epoch: 1767225599
   --  ========================================================================
   Test_Known_Epoch
     (Name           => "NYE 2025 (2025-12-31 23:59:59)",
      Epoch_Seconds  => 1_767_225_599,
      Expected_Year  => 2025,
      Expected_Month => 12,
      Expected_Day   => 31,
      Expected_Hour  => 23,
      Expected_Min   => 59,
      Expected_Sec   => 59);

   --  ========================================================================
   --  Far Future: 2099-12-31 23:59:59 UTC
   --  Epoch: 4102444799
   --  ========================================================================
   Test_Known_Epoch
     (Name           => "End of 2099 (2099-12-31 23:59:59)",
      Epoch_Seconds  => 4_102_444_799,
      Expected_Year  => 2099,
      Expected_Month => 12,
      Expected_Day   => 31,
      Expected_Hour  => 23,
      Expected_Min   => 59,
      Expected_Sec   => 59);

   --  ========================================================================
   --  Negative Epoch Tests (Before Unix Epoch)
   --  ========================================================================

   Put_Line ("");
   Put_Line ("Test: Negative Epoch (Before 1970)");

   --  1969-12-31 23:59:59 UTC = epoch -1
   Test_Known_Epoch
     (Name           => "Just before epoch (1969-12-31 23:59:59)",
      Epoch_Seconds  => -1,
      Expected_Year  => 1969,
      Expected_Month => 12,
      Expected_Day   => 31,
      Expected_Hour  => 23,
      Expected_Min   => 59,
      Expected_Sec   => 59);

   --  1960-01-01 00:00:00 UTC = epoch -315619200
   Test_Known_Epoch
     (Name           => "1960-01-01 00:00:00",
      Epoch_Seconds  => -315_619_200,
      Expected_Year  => 1960,
      Expected_Month => 1,
      Expected_Day   => 1,
      Expected_Hour  => 0,
      Expected_Min   => 0,
      Expected_Sec   => 0);

   --  ========================================================================
   --  Month Boundary Tests
   --  ========================================================================

   Put_Line ("");
   Put_Line ("Test: Month Boundaries");

   --  End of January 2025: 2025-01-31 23:59:59 UTC
   --  Epoch: 1738367999
   Test_Known_Epoch
     (Name           => "End of Jan 2025 (2025-01-31 23:59:59)",
      Epoch_Seconds  => 1_738_367_999,
      Expected_Year  => 2025,
      Expected_Month => 1,
      Expected_Day   => 31,
      Expected_Hour  => 23,
      Expected_Min   => 59,
      Expected_Sec   => 59);

   --  Start of February 2025: 2025-02-01 00:00:00 UTC
   --  Epoch: 1738368000
   Test_Known_Epoch
     (Name           => "Start of Feb 2025 (2025-02-01 00:00:00)",
      Epoch_Seconds  => 1_738_368_000,
      Expected_Year  => 2025,
      Expected_Month => 2,
      Expected_Day   => 1,
      Expected_Hour  => 0,
      Expected_Min   => 0,
      Expected_Sec   => 0);

   --  Print summary
   New_Line;
   Put_Line ("========================================");
   Put_Line ("Test Summary: Epoch Conversions");
   Put_Line ("========================================");
   Put_Line ("Total tests: " & Total_Tests'Image);
   Put_Line ("Passed:      " & Passed_Tests'Image);
   Put_Line ("Failed:      " & Natural'Image (Total_Tests - Passed_Tests));
   New_Line;

   --  Register results with test framework
   Test_Framework.Register_Results (Total_Tests, Passed_Tests);

end Test_Epoch_Conversions;
