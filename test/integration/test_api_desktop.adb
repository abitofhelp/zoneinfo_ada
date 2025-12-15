pragma Ada_2022;
--  ======================================================================
--  Test_API_Desktop
--  ======================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--
--  Purpose:
--    Integration tests for Zoneinfo.API.Desktop.
--    Tests the full stack: API -> Application -> Infrastructure.
--    Uses real system clock and timezone adapters.
--  ======================================================================

with Ada.Text_IO;
with Interfaces;
with Zoneinfo.API.Desktop;
with Domain.Value_Object.Instant;
with Domain.Value_Object.Civil;
with Domain.Value_Object.Zone_ID;
with Domain.Value_Object.Zoned;
with Domain.Value_Object.Duration_Type;
with Test_Framework;

procedure Test_API_Desktop is

   use Ada.Text_IO;
   use Interfaces;
   use Zoneinfo.API.Desktop;

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
   Put_Line ("Testing: Zoneinfo.API.Desktop");
   Put_Line ("========================================");
   New_Line;

   --  ========================================================================
   --  Test: Now - Get current time as Instant
   --  ========================================================================

   Put_Line ("Test: Now (Current Time as Instant)");

   declare
      R : constant Instant_Result.Result := Now;
   begin
      Run_Test ("Now returns Ok result", Instant_Result.Is_Ok (R));
      if Instant_Result.Is_Ok (R) then
         declare
            I     : constant Instant := Instant_Result.Value (R);
            Nanos : constant Interfaces.Integer_64 :=
              Domain.Value_Object.Instant.Get_Epoch_Nanos (I);
         begin
            --  Current time should be after 2024-01-01
            Run_Test
              ("Now returns reasonable epoch (after 2024)",
               Nanos > 1_704_067_200_000_000_000);
            --  And before year 2100
            Run_Test
              ("Now returns reasonable epoch (before 2100)",
               Nanos < 4_102_444_800_000_000_000);
         end;
      end if;
   end;

   --  ========================================================================
   --  Test: Now_UTC - Get current UTC time as Zoned
   --  ========================================================================

   Put_Line ("Test: Now_UTC (Current Time as Zoned UTC)");

   declare
      R : constant Zoned_Result.Result := Now_UTC;
   begin
      Run_Test ("Now_UTC returns Ok result", Zoned_Result.Is_Ok (R));
      if Zoned_Result.Is_Ok (R) then
         declare
            Z    : constant Zoned := Zoned_Result.Value (R);
            Zone : constant Zone_ID := Domain.Value_Object.Zoned.Get_Zone (Z);
         begin
            Run_Test
              ("Now_UTC zone is UTC",
               Domain.Value_Object.Zone_ID.Is_UTC (Zone));
         end;
      end if;
   end;

   --  ========================================================================
   --  Test: Now_Zoned with UTC zone
   --  ========================================================================

   Put_Line ("Test: Now_Zoned (Current Time in Specified Zone)");

   declare
      Zone_Result : constant
        Domain.Value_Object.Zone_ID.Zone_ID_Result.Result :=
          Domain.Value_Object.Zone_ID.From_String ("UTC");
   begin
      if Domain.Value_Object.Zone_ID.Zone_ID_Result.Is_Ok (Zone_Result) then
         declare
            Zone : constant Zone_ID :=
              Domain.Value_Object.Zone_ID.Zone_ID_Result.Value (Zone_Result);
            R : constant Zoned_Result.Result := Now_Zoned (Zone);
         begin
            Run_Test ("Now_Zoned(UTC) returns Ok", Zoned_Result.Is_Ok (R));
         end;
      else
         Run_Test ("Now_Zoned test setup", False);
      end if;
   end;

   --  ========================================================================
   --  Test: Is_Valid_Zone
   --  ========================================================================

   Put_Line ("Test: Is_Valid_Zone");

   declare
      Zone_Result : constant
        Domain.Value_Object.Zone_ID.Zone_ID_Result.Result :=
          Domain.Value_Object.Zone_ID.From_String ("UTC");
   begin
      if Domain.Value_Object.Zone_ID.Zone_ID_Result.Is_Ok (Zone_Result) then
         declare
            Zone : constant Zone_ID :=
              Domain.Value_Object.Zone_ID.Zone_ID_Result.Value (Zone_Result);
         begin
            Run_Test ("UTC is valid zone", Is_Valid_Zone (Zone));
         end;
      else
         Run_Test ("Is_Valid_Zone test setup", False);
      end if;
   end;

   --  ========================================================================
   --  Test: To_Civil from Zoned
   --  ========================================================================

   Put_Line ("Test: To_Civil (Zoned to Civil Conversion)");

   declare
      R : constant Zoned_Result.Result := Now_UTC;
   begin
      if Zoned_Result.Is_Ok (R) then
         declare
            Z : constant Zoned := Zoned_Result.Value (R);
            C : constant Civil := To_Civil (Z);
         begin
            --  Civil time should have valid components
            Run_Test
              ("To_Civil year >= 2024",
               Domain.Value_Object.Civil.Get_Year (C) >= 2024);
            Run_Test
              ("To_Civil month valid",
               Domain.Value_Object.Civil.Get_Month (C)'Valid);
            Run_Test
              ("To_Civil day valid",
               Domain.Value_Object.Civil.Get_Day (C)'Valid);
            Run_Test
              ("To_Civil hour valid",
               Domain.Value_Object.Civil.Get_Hour (C)'Valid);
            Run_Test
              ("To_Civil minute valid",
               Domain.Value_Object.Civil.Get_Minute (C)'Valid);
            Run_Test
              ("To_Civil second valid",
               Domain.Value_Object.Civil.Get_Second (C)'Valid);
         end;
      else
         Run_Test ("To_Civil test - Now_UTC succeeded", False);
      end if;
   end;

   --  ========================================================================
   --  Test: To_Civil from Instant + Zone
   --  ========================================================================

   Put_Line ("Test: To_Civil (Instant + Zone to Civil)");

   declare
      Instant_R : constant Instant_Result.Result := Now;
      Zone_R    : constant Domain.Value_Object.Zone_ID.Zone_ID_Result.Result :=
        Domain.Value_Object.Zone_ID.From_String ("UTC");
   begin
      if Instant_Result.Is_Ok (Instant_R) and then
         Domain.Value_Object.Zone_ID.Zone_ID_Result.Is_Ok (Zone_R)
      then
         declare
            I    : constant Instant := Instant_Result.Value (Instant_R);
            Zone : constant Zone_ID :=
              Domain.Value_Object.Zone_ID.Zone_ID_Result.Value (Zone_R);
            C : constant Civil := To_Civil (I, Zone);
         begin
            Run_Test
              ("To_Civil(Instant, Zone) - valid year",
               Domain.Value_Object.Civil.Get_Year (C) >= 2024);
         end;
      else
         Run_Test ("To_Civil(Instant, Zone) test setup", False);
      end if;
   end;

   --  ========================================================================
   --  Test: To_Zoned from Civil + Zone
   --  ========================================================================

   Put_Line ("Test: To_Zoned (Civil + Zone to Zoned)");

   declare
      Civil_R : constant Domain.Value_Object.Civil.Civil_Result.Result :=
        Domain.Value_Object.Civil.Create
          (Year       => 2025,
           Month      => 6,
           Day        => 15,
           Hour       => 12,
           Minute     => 0,
           Second     => 0,
           Nanosecond => 0);
      Zone_R : constant Domain.Value_Object.Zone_ID.Zone_ID_Result.Result :=
        Domain.Value_Object.Zone_ID.From_String ("UTC");
   begin
      if Domain.Value_Object.Civil.Civil_Result.Is_Ok (Civil_R) and then
         Domain.Value_Object.Zone_ID.Zone_ID_Result.Is_Ok (Zone_R)
      then
         declare
            C    : constant Civil :=
              Domain.Value_Object.Civil.Civil_Result.Value (Civil_R);
            Zone : constant Zone_ID :=
              Domain.Value_Object.Zone_ID.Zone_ID_Result.Value (Zone_R);
            R : constant Zoned_Result.Result := To_Zoned (C, Zone);
         begin
            Run_Test
              ("To_Zoned(2025-06-15 12:00 UTC) returns Ok",
               Zoned_Result.Is_Ok (R));
         end;
      else
         Run_Test ("To_Zoned test setup", False);
      end if;
   end;

   --  ========================================================================
   --  Test: To_Instant from Civil + Zone
   --  ========================================================================

   Put_Line ("Test: To_Instant (Civil + Zone to Instant)");

   declare
      --  2025-01-01 00:00:00 UTC
      Civil_R : constant Domain.Value_Object.Civil.Civil_Result.Result :=
        Domain.Value_Object.Civil.Create
          (Year       => 2025,
           Month      => 1,
           Day        => 1,
           Hour       => 0,
           Minute     => 0,
           Second     => 0,
           Nanosecond => 0);
      Zone_R : constant Domain.Value_Object.Zone_ID.Zone_ID_Result.Result :=
        Domain.Value_Object.Zone_ID.From_String ("UTC");
   begin
      if Domain.Value_Object.Civil.Civil_Result.Is_Ok (Civil_R) and then
         Domain.Value_Object.Zone_ID.Zone_ID_Result.Is_Ok (Zone_R)
      then
         declare
            C    : constant Civil :=
              Domain.Value_Object.Civil.Civil_Result.Value (Civil_R);
            Zone : constant Zone_ID :=
              Domain.Value_Object.Zone_ID.Zone_ID_Result.Value (Zone_R);
            R : constant Instant_Result.Result := To_Instant (C, Zone);
         begin
            Run_Test
              ("To_Instant(2025-01-01 UTC) returns Ok",
               Instant_Result.Is_Ok (R));
            if Instant_Result.Is_Ok (R) then
               declare
                  I     : constant Instant := Instant_Result.Value (R);
                  Nanos : constant Interfaces.Integer_64 :=
                    Domain.Value_Object.Instant.Get_Epoch_Nanos (I);
                  --  Expected: 2025-01-01 00:00:00 UTC = 1735689600 seconds
                  Expected : constant Interfaces.Integer_64 :=
                    1_735_689_600_000_000_000;
               begin
                  Run_Test
                    ("To_Instant epoch nanos correct",
                     Nanos = Expected);
               end;
            end if;
         end;
      else
         Run_Test ("To_Instant test setup", False);
      end if;
   end;

   --  ========================================================================
   --  Test: Get_Offset for UTC zone
   --  ========================================================================

   Put_Line ("Test: Get_Offset (UTC Offset Query)");

   declare
      R : constant Zoned_Result.Result := Now_UTC;
   begin
      if Zoned_Result.Is_Ok (R) then
         declare
            Z      : constant Zoned := Zoned_Result.Value (R);
            Offset : constant Duration_Type := Get_Offset (Z);
         begin
            --  UTC should have zero offset
            Run_Test
              ("UTC offset is zero",
               Domain.Value_Object.Duration_Type.Get_Seconds (Offset) = 0);
         end;
      else
         Run_Test ("Get_Offset test - Now_UTC succeeded", False);
      end if;
   end;

   --  ========================================================================
   --  Test: Round-trip Civil -> Instant -> Civil
   --  ========================================================================

   Put_Line ("Test: Round-trip Civil -> Instant -> Civil");

   declare
      Original_Civil_R :
        constant Domain.Value_Object.Civil.Civil_Result.Result :=
        Domain.Value_Object.Civil.Create
          (Year       => 2025,
           Month      => 7,
           Day        => 4,
           Hour       => 15,
           Minute     => 30,
           Second     => 45,
           Nanosecond => 123_456_789);
      Zone_R : constant Domain.Value_Object.Zone_ID.Zone_ID_Result.Result :=
        Domain.Value_Object.Zone_ID.From_String ("UTC");
   begin
      if Domain.Value_Object.Civil.Civil_Result.Is_Ok (Original_Civil_R)
         and then Domain.Value_Object.Zone_ID.Zone_ID_Result.Is_Ok (Zone_R)
      then
         declare
            Original : constant Civil :=
              Domain.Value_Object.Civil.Civil_Result.Value (Original_Civil_R);
            Zone : constant Zone_ID :=
              Domain.Value_Object.Zone_ID.Zone_ID_Result.Value (Zone_R);
            Instant_R : constant Instant_Result.Result :=
              To_Instant (Original, Zone);
         begin
            if Instant_Result.Is_Ok (Instant_R) then
               declare
                  I : constant Instant :=
                    Instant_Result.Value (Instant_R);
                  Round_Trip : constant Civil := To_Civil (I, Zone);
               begin
                  Run_Test
                    ("Round-trip year matches",
                     Domain.Value_Object.Civil.Get_Year (Round_Trip) =
                     Domain.Value_Object.Civil.Get_Year (Original));
                  Run_Test
                    ("Round-trip month matches",
                     Domain.Value_Object.Civil.Get_Month (Round_Trip) =
                     Domain.Value_Object.Civil.Get_Month (Original));
                  Run_Test
                    ("Round-trip day matches",
                     Domain.Value_Object.Civil.Get_Day (Round_Trip) =
                     Domain.Value_Object.Civil.Get_Day (Original));
                  Run_Test
                    ("Round-trip hour matches",
                     Domain.Value_Object.Civil.Get_Hour (Round_Trip) =
                     Domain.Value_Object.Civil.Get_Hour (Original));
                  Run_Test
                    ("Round-trip minute matches",
                     Domain.Value_Object.Civil.Get_Minute (Round_Trip) =
                     Domain.Value_Object.Civil.Get_Minute (Original));
                  Run_Test
                    ("Round-trip second matches",
                     Domain.Value_Object.Civil.Get_Second (Round_Trip) =
                     Domain.Value_Object.Civil.Get_Second (Original));
                  Run_Test
                    ("Round-trip nanosecond matches",
                     Domain.Value_Object.Civil.Get_Nanosecond (Round_Trip) =
                     Domain.Value_Object.Civil.Get_Nanosecond (Original));
               end;
            else
               Run_Test ("Round-trip - To_Instant succeeded", False);
            end if;
         end;
      else
         Run_Test ("Round-trip test setup", False);
      end if;
   end;

   --  Print summary
   New_Line;
   Put_Line ("========================================");
   Put_Line ("Test Summary: Zoneinfo.API.Desktop");
   Put_Line ("========================================");
   Put_Line ("Total tests: " & Total_Tests'Image);
   Put_Line ("Passed:      " & Passed_Tests'Image);
   Put_Line ("Failed:      " & Natural'Image (Total_Tests - Passed_Tests));
   New_Line;

   --  Register results with test framework
   Test_Framework.Register_Results (Total_Tests, Passed_Tests);

end Test_API_Desktop;
