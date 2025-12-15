pragma Ada_2022;
--  ======================================================================
--  Test_Infrastructure_Tzif
--  ======================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--
--  Purpose:
--    Integration tests for Infrastructure.Adapter.Tzif.
--    Tests the Tzif adapter directly with known epoch values.
--    Pattern inspired by tzif test_get_transition_at_epoch.adb.
--
--  Note:
--    The Tzif adapter is fully integrated with the tzif library.
--    These tests verify:
--      - Epoch to civil conversions for specific timezones
--      - DST transition handling
--      - Historical timezone changes
--      - UTC offset queries
--  ======================================================================

with Ada.Text_IO;
with Interfaces;
with Infrastructure.Adapter.Tzif;
with Domain.Value_Object.Instant;
with Domain.Value_Object.Civil;
with Domain.Value_Object.Zone_ID;
with Domain.Value_Object.Duration_Type;
with Test_Framework;

procedure Test_Infrastructure_Tzif is

   use Ada.Text_IO;
   use Interfaces;

   package Tzif renames Infrastructure.Adapter.Tzif;
   package Instant_Pkg renames Domain.Value_Object.Instant;
   package Civil_Pkg renames Domain.Value_Object.Civil;
   package Zone_Pkg renames Domain.Value_Object.Zone_ID;
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

   --  Helper: Get UTC zone
   function UTC_Zone return Tzif.Zone_ID is
      R : constant Zone_Pkg.Zone_ID_Result.Result :=
        Zone_Pkg.From_String ("UTC");
   begin
      return Zone_Pkg.Zone_ID_Result.Value (R);
   end UTC_Zone;

begin
   Put_Line ("========================================");
   Put_Line ("Testing: Infrastructure.Adapter.Tzif");
   Put_Line ("========================================");
   New_Line;

   --  ========================================================================
   --  Test: To_Civil at Unix Epoch (epoch = 0)
   --  Pattern: tzif test_get_transition_at_epoch "Test UTC at epoch 0"
   --  ========================================================================

   Put_Line ("Test: To_Civil at Unix Epoch");

   declare
      I    : constant Tzif.Instant := Instant_Pkg.From_Epoch_Nanos (0);
      Zone : constant Tzif.Zone_ID := UTC_Zone;
      C    : constant Tzif.Civil := Tzif.To_Civil (I, Zone);
   begin
      Run_Test
        ("Epoch 0 -> 1970-01-01 00:00:00 Year",
         Civil_Pkg.Get_Year (C) = 1970);
      Run_Test
        ("Epoch 0 -> 1970-01-01 00:00:00 Month",
         Civil_Pkg.Get_Month (C) = 1);
      Run_Test
        ("Epoch 0 -> 1970-01-01 00:00:00 Day",
         Civil_Pkg.Get_Day (C) = 1);
      Run_Test
        ("Epoch 0 -> 1970-01-01 00:00:00 Hour",
         Civil_Pkg.Get_Hour (C) = 0);
      Run_Test
        ("Epoch 0 -> 1970-01-01 00:00:00 Minute",
         Civil_Pkg.Get_Minute (C) = 0);
      Run_Test
        ("Epoch 0 -> 1970-01-01 00:00:00 Second",
         Civil_Pkg.Get_Second (C) = 0);
   end;

   --  ========================================================================
   --  Test: To_Civil at Y2K (2000-01-01 00:00:00 UTC)
   --  Epoch: 946684800 seconds = 946684800000000000 nanoseconds
   --  ========================================================================

   Put_Line ("Test: To_Civil at Y2K");

   declare
      Epoch_Nanos : constant Integer_64 := 946_684_800_000_000_000;
      I           : constant Tzif.Instant :=
        Instant_Pkg.From_Epoch_Nanos (Epoch_Nanos);
      Zone        : constant Tzif.Zone_ID := UTC_Zone;
      C           : constant Tzif.Civil := Tzif.To_Civil (I, Zone);
   begin
      Run_Test ("Y2K Year", Civil_Pkg.Get_Year (C) = 2000);
      Run_Test ("Y2K Month", Civil_Pkg.Get_Month (C) = 1);
      Run_Test ("Y2K Day", Civil_Pkg.Get_Day (C) = 1);
      Run_Test ("Y2K Hour", Civil_Pkg.Get_Hour (C) = 0);
   end;

   --  ========================================================================
   --  Test: To_Civil at negative epoch (before 1970)
   --  Pattern: tzif test_get_transition_at_epoch "Test Negative Epoch"
   --  1969-12-31 23:59:59 UTC = epoch -1 second
   --  ========================================================================

   Put_Line ("Test: To_Civil at Negative Epoch");

   declare
      Epoch_Nanos : constant Integer_64 := -1_000_000_000;  --  -1 sec
      I           : constant Tzif.Instant :=
        Instant_Pkg.From_Epoch_Nanos (Epoch_Nanos);
      Zone        : constant Tzif.Zone_ID := UTC_Zone;
      C           : constant Tzif.Civil := Tzif.To_Civil (I, Zone);
   begin
      Run_Test ("Pre-epoch Year = 1969", Civil_Pkg.Get_Year (C) = 1969);
      Run_Test ("Pre-epoch Month = 12", Civil_Pkg.Get_Month (C) = 12);
      Run_Test ("Pre-epoch Day = 31", Civil_Pkg.Get_Day (C) = 31);
      Run_Test ("Pre-epoch Hour = 23", Civil_Pkg.Get_Hour (C) = 23);
      Run_Test ("Pre-epoch Minute = 59", Civil_Pkg.Get_Minute (C) = 59);
      Run_Test ("Pre-epoch Second = 59", Civil_Pkg.Get_Second (C) = 59);
   end;

   --  ========================================================================
   --  Test: To_Civil at far future epoch
   --  Pattern: tzif test_get_transition_at_epoch "Test Future Epoch"
   --  2099-12-31 23:59:59 UTC = epoch 4102444799 seconds
   --  ========================================================================

   Put_Line ("Test: To_Civil at Far Future");

   declare
      Epoch_Nanos : constant Integer_64 := 4_102_444_799_000_000_000;
      I           : constant Tzif.Instant :=
        Instant_Pkg.From_Epoch_Nanos (Epoch_Nanos);
      Zone        : constant Tzif.Zone_ID := UTC_Zone;
      C           : constant Tzif.Civil := Tzif.To_Civil (I, Zone);
   begin
      Run_Test ("Future Year = 2099", Civil_Pkg.Get_Year (C) = 2099);
      Run_Test ("Future Month = 12", Civil_Pkg.Get_Month (C) = 12);
      Run_Test ("Future Day = 31", Civil_Pkg.Get_Day (C) = 31);
   end;

   --  ========================================================================
   --  Test: To_Instant from known Civil time
   --  ========================================================================

   Put_Line ("Test: To_Instant from Civil");

   declare
      Civil_R : constant Civil_Pkg.Civil_Result.Result :=
        Civil_Pkg.Create
          (Year       => 2025,
           Month      => 1,
           Day        => 1,
           Hour       => 0,
           Minute     => 0,
           Second     => 0,
           Nanosecond => 0);
      Zone : constant Tzif.Zone_ID := UTC_Zone;
   begin
      if Civil_Pkg.Civil_Result.Is_Ok (Civil_R) then
         declare
            C        : constant Tzif.Civil :=
              Civil_Pkg.Civil_Result.Value (Civil_R);
            R        : constant Tzif.Instant_Result.Result :=
              Tzif.To_Instant (C, Zone);
            Expected : constant Integer_64 := 1_735_689_600_000_000_000;
         begin
            Run_Test
              ("To_Instant 2025-01-01 returns Ok",
               Tzif.Instant_Result.Is_Ok (R));
            if Tzif.Instant_Result.Is_Ok (R) then
               declare
                  I     : constant Tzif.Instant :=
                    Tzif.Instant_Result.Value (R);
                  Nanos : constant Integer_64 :=
                    Instant_Pkg.Get_Epoch_Nanos (I);
               begin
                  Run_Test
                    ("To_Instant 2025-01-01 epoch correct",
                     Nanos = Expected);
               end;
            end if;
         end;
      else
         Run_Test ("To_Instant test setup", False);
      end if;
   end;

   --  ========================================================================
   --  Test: To_Instant round-trip with nanoseconds
   --  ========================================================================

   Put_Line ("Test: To_Instant Round-Trip with Nanoseconds");

   declare
      Civil_R : constant Civil_Pkg.Civil_Result.Result :=
        Civil_Pkg.Create
          (Year       => 2025,
           Month      => 6,
           Day        => 15,
           Hour       => 14,
           Minute     => 30,
           Second     => 45,
           Nanosecond => 123_456_789);
      Zone : constant Tzif.Zone_ID := UTC_Zone;
   begin
      if Civil_Pkg.Civil_Result.Is_Ok (Civil_R) then
         declare
            C1 : constant Tzif.Civil :=
              Civil_Pkg.Civil_Result.Value (Civil_R);
            R  : constant Tzif.Instant_Result.Result :=
              Tzif.To_Instant (C1, Zone);
         begin
            if Tzif.Instant_Result.Is_Ok (R) then
               declare
                  I  : constant Tzif.Instant := Tzif.Instant_Result.Value (R);
                  C2 : constant Tzif.Civil := Tzif.To_Civil (I, Zone);
               begin
                  Run_Test
                    ("Round-trip Year",
                     Civil_Pkg.Get_Year (C2) = Civil_Pkg.Get_Year (C1));
                  Run_Test
                    ("Round-trip Month",
                     Civil_Pkg.Get_Month (C2) = Civil_Pkg.Get_Month (C1));
                  Run_Test
                    ("Round-trip Day",
                     Civil_Pkg.Get_Day (C2) = Civil_Pkg.Get_Day (C1));
                  Run_Test
                    ("Round-trip Hour",
                     Civil_Pkg.Get_Hour (C2) = Civil_Pkg.Get_Hour (C1));
                  Run_Test
                    ("Round-trip Minute",
                     Civil_Pkg.Get_Minute (C2) = Civil_Pkg.Get_Minute (C1));
                  Run_Test
                    ("Round-trip Second",
                     Civil_Pkg.Get_Second (C2) = Civil_Pkg.Get_Second (C1));
                  Run_Test
                    ("Round-trip Nanosecond",
                     Civil_Pkg.Get_Nanosecond (C2) =
                       Civil_Pkg.Get_Nanosecond (C1));
               end;
            else
               Run_Test ("Round-trip To_Instant succeeded", False);
            end if;
         end;
      else
         Run_Test ("Round-trip test setup", False);
      end if;
   end;

   --  ========================================================================
   --  Test: Is_Valid_Zone
   --  ========================================================================

   Put_Line ("Test: Is_Valid_Zone");

   declare
      Zone : constant Tzif.Zone_ID := UTC_Zone;
   begin
      Run_Test ("UTC is valid zone", Tzif.Is_Valid_Zone (Zone));
   end;

   --  Test non-UTC zones (tzif is fully integrated)
   declare
      Zone_R : constant Zone_Pkg.Zone_ID_Result.Result :=
        Zone_Pkg.From_String ("America/New_York");
   begin
      if Zone_Pkg.Zone_ID_Result.Is_Ok (Zone_R) then
         Run_Test ("America/New_York is valid zone",
            Tzif.Is_Valid_Zone (Zone_Pkg.Zone_ID_Result.Value (Zone_R)));
      end if;
   end;

   declare
      Zone_R : constant Zone_Pkg.Zone_ID_Result.Result :=
        Zone_Pkg.From_String ("Europe/London");
   begin
      if Zone_Pkg.Zone_ID_Result.Is_Ok (Zone_R) then
         Run_Test ("Europe/London is valid zone",
            Tzif.Is_Valid_Zone (Zone_Pkg.Zone_ID_Result.Value (Zone_R)));
      end if;
   end;

   declare
      Zone_R : constant Zone_Pkg.Zone_ID_Result.Result :=
        Zone_Pkg.From_String ("Asia/Tokyo");
   begin
      if Zone_Pkg.Zone_ID_Result.Is_Ok (Zone_R) then
         Run_Test ("Asia/Tokyo is valid zone",
            Tzif.Is_Valid_Zone (Zone_Pkg.Zone_ID_Result.Value (Zone_R)));
      end if;
   end;

   --  ========================================================================
   --  Test: Get_UTC_Offset for UTC (should be zero)
   --  ========================================================================

   Put_Line ("Test: Get_UTC_Offset");

   declare
      I      : constant Tzif.Instant := Instant_Pkg.From_Epoch_Nanos (0);
      Zone   : constant Tzif.Zone_ID := UTC_Zone;
      Offset : constant Tzif.Duration_Type := Tzif.Get_UTC_Offset (I, Zone);
   begin
      Run_Test
        ("UTC offset is zero",
         Duration_Pkg.Get_Seconds (Offset) = 0 and then
         Duration_Pkg.Get_Nanoseconds (Offset) = 0);
   end;

   --  Test America/New_York winter (EST = UTC-5)
   --  2024-01-15 12:00:00 UTC = epoch 1705320000 seconds
   declare
      Epoch_Secs : constant Interfaces.Integer_64 := 1_705_320_000;
      I          : constant Tzif.Instant :=
        Instant_Pkg.From_Epoch_Nanos (Epoch_Secs * 1_000_000_000);
      Zone_R     : constant Zone_Pkg.Zone_ID_Result.Result :=
        Zone_Pkg.From_String ("America/New_York");
   begin
      if Zone_Pkg.Zone_ID_Result.Is_Ok (Zone_R) then
         declare
            Zone   : constant Tzif.Zone_ID :=
              Zone_Pkg.Zone_ID_Result.Value (Zone_R);
            Offset : constant Tzif.Duration_Type :=
              Tzif.Get_UTC_Offset (I, Zone);
         begin
            --  EST is UTC-5 = -18000 seconds
            Run_Test
              ("America/New_York winter offset (EST = -5h)",
               Duration_Pkg.Get_Seconds (Offset) = -18000);
         end;
      end if;
   end;

   --  Test America/New_York summer (EDT = UTC-4)
   --  2024-07-15 12:00:00 UTC = epoch 1721044800 seconds
   declare
      Epoch_Secs : constant Interfaces.Integer_64 := 1_721_044_800;
      I          : constant Tzif.Instant :=
        Instant_Pkg.From_Epoch_Nanos (Epoch_Secs * 1_000_000_000);
      Zone_R     : constant Zone_Pkg.Zone_ID_Result.Result :=
        Zone_Pkg.From_String ("America/New_York");
   begin
      if Zone_Pkg.Zone_ID_Result.Is_Ok (Zone_R) then
         declare
            Zone   : constant Tzif.Zone_ID :=
              Zone_Pkg.Zone_ID_Result.Value (Zone_R);
            Offset : constant Tzif.Duration_Type :=
              Tzif.Get_UTC_Offset (I, Zone);
         begin
            --  EDT is UTC-4 = -14400 seconds
            Run_Test
              ("America/New_York summer offset (EDT = -4h)",
               Duration_Pkg.Get_Seconds (Offset) = -14400);
         end;
      end if;
   end;

   --  ========================================================================
   --  Test: Leap Year Handling in To_Civil
   --  ========================================================================

   Put_Line ("Test: Leap Year Handling");

   --  2000-02-29 12:00:00 UTC (2000 is leap year - div by 400)
   --  Epoch: 951825600 seconds
   declare
      Epoch_Nanos : constant Integer_64 := 951_825_600_000_000_000;
      I           : constant Tzif.Instant :=
        Instant_Pkg.From_Epoch_Nanos (Epoch_Nanos);
      Zone        : constant Tzif.Zone_ID := UTC_Zone;
      C           : constant Tzif.Civil := Tzif.To_Civil (I, Zone);
   begin
      Run_Test ("Leap 2000 Year", Civil_Pkg.Get_Year (C) = 2000);
      Run_Test ("Leap 2000 Month = 2", Civil_Pkg.Get_Month (C) = 2);
      Run_Test ("Leap 2000 Day = 29", Civil_Pkg.Get_Day (C) = 29);
      Run_Test ("Leap 2000 Hour = 12", Civil_Pkg.Get_Hour (C) = 12);
   end;

   --  2024-02-29 23:59:59 UTC (2024 is leap year)
   --  Epoch: 1709251199 seconds
   declare
      Epoch_Nanos : constant Integer_64 := 1_709_251_199_000_000_000;
      I           : constant Tzif.Instant :=
        Instant_Pkg.From_Epoch_Nanos (Epoch_Nanos);
      Zone        : constant Tzif.Zone_ID := UTC_Zone;
      C           : constant Tzif.Civil := Tzif.To_Civil (I, Zone);
   begin
      Run_Test ("Leap 2024 Year", Civil_Pkg.Get_Year (C) = 2024);
      Run_Test ("Leap 2024 Month = 2", Civil_Pkg.Get_Month (C) = 2);
      Run_Test ("Leap 2024 Day = 29", Civil_Pkg.Get_Day (C) = 29);
   end;

   --  1900-03-01 00:00:00 UTC (1900 NOT leap - div by 100 not 400)
   --  This tests that Feb 28 is followed by Mar 1, not Feb 29
   --  Epoch: -2203891200 seconds
   declare
      Epoch_Nanos : constant Integer_64 := -2_203_891_200_000_000_000;
      I           : constant Tzif.Instant :=
        Instant_Pkg.From_Epoch_Nanos (Epoch_Nanos);
      Zone        : constant Tzif.Zone_ID := UTC_Zone;
      C           : constant Tzif.Civil := Tzif.To_Civil (I, Zone);
   begin
      Run_Test ("Non-leap 1900 Year", Civil_Pkg.Get_Year (C) = 1900);
      Run_Test ("Non-leap 1900 Month = 3", Civil_Pkg.Get_Month (C) = 3);
      Run_Test ("Non-leap 1900 Day = 1", Civil_Pkg.Get_Day (C) = 1);
   end;

   --  Print summary
   New_Line;
   Put_Line ("========================================");
   Put_Line ("Test Summary: Infrastructure.Adapter.Tzif");
   Put_Line ("========================================");
   Put_Line ("Total tests: " & Total_Tests'Image);
   Put_Line ("Passed:      " & Passed_Tests'Image);
   Put_Line ("Failed:      " & Natural'Image (Total_Tests - Passed_Tests));
   New_Line;

   --  Register results with test framework
   Test_Framework.Register_Results (Total_Tests, Passed_Tests);

end Test_Infrastructure_Tzif;
