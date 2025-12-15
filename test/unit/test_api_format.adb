pragma Ada_2022;
--  ======================================================================
--  Test_API_Format
--  ======================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--  Purpose:
--    Unit tests for Zoneinfo.API.Format - datetime string formatting.
--  ======================================================================
with Ada.Text_IO; use Ada.Text_IO;
with Interfaces; use Interfaces;
with Zoneinfo.API.Format;
with Domain.Value_Object.Civil;
with Domain.Value_Object.Duration_Type;
with Domain.Value_Object.Zone_ID;
with Domain.Value_Object.Instant;
with Test_Framework;

procedure Test_API_Format is

   use Zoneinfo.API.Format;

   --  Aliases to avoid ambiguity with API re-exports
   package VOC renames Domain.Value_Object.Civil;
   package VOD renames Domain.Value_Object.Duration_Type;

   use type VOC.Civil;
   use type VOD.Duration_Type;

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

   --  Helper to create Civil
   function Make_Civil
     (Y : VOC.Year_Number; Mo : VOC.Month_Number; D : VOC.Day_Number;
      H : VOC.Hour_Number := 0; Mi : VOC.Minute_Number := 0;
      S : VOC.Second_Number := 0; N : VOC.Nanosecond_Number := 0)
      return VOC.Civil
   is
      R : constant VOC.Civil_Result.Result :=
        VOC.Create (Y, Mo, D, H, Mi, S, N);
   begin
      return VOC.Civil_Result.Value (R);
   end Make_Civil;

begin
   Put_Line ("========================================");
   Put_Line ("Testing: Zoneinfo.API.Format");
   Put_Line ("========================================");
   New_Line;

   --  ========================================================================
   --  Test: To_ISO_8601 - Basic datetime formatting
   --  ========================================================================

   Put_Line ("Test: To_ISO_8601");

   declare
      C : constant VOC.Civil := Make_Civil (2025, 12, 4, 14, 30, 45, 0);
      Result : constant Datetime_String := To_ISO_8601 (C);
      Str    : constant String := Datetime_Strings.To_String (Result);
   begin
      Run_Test ("To_ISO_8601 format without nanos",
        Str = "2025-12-04T14:30:45");
   end;

   declare
      C : constant VOC.Civil :=
        Make_Civil (2025, 12, 4, 14, 30, 45, 123_456_789);
      Result : constant Datetime_String := To_ISO_8601 (C);
      Str    : constant String := Datetime_Strings.To_String (Result);
   begin
      Run_Test ("To_ISO_8601 format with nanos",
        Str = "2025-12-04T14:30:45.123456789");
   end;

   declare
      C : constant VOC.Civil := Make_Civil (2025, 1, 1, 0, 0, 0, 0);
      Result : constant Datetime_String := To_ISO_8601 (C);
      Str    : constant String := Datetime_Strings.To_String (Result);
   begin
      Run_Test ("To_ISO_8601 midnight",
        Str = "2025-01-01T00:00:00");
   end;

   declare
      C : constant VOC.Civil := Make_Civil (2025, 12, 4, 14, 30, 45, 0);
      Result : constant Datetime_String :=
        To_ISO_8601 (C, Include_Nanos => False);
      Str    : constant String := Datetime_Strings.To_String (Result);
   begin
      Run_Test ("To_ISO_8601 exclude nanos param",
        Str = "2025-12-04T14:30:45");
   end;

   --  ========================================================================
   --  Test: To_ISO_8601_With_Offset
   --  ========================================================================

   Put_Line ("Test: To_ISO_8601_With_Offset");

   declare
      C : constant VOC.Civil := Make_Civil (2025, 12, 4, 14, 30, 0, 0);
      Offset : constant VOD.Duration_Type := VOD.Zero;
      Result : constant Datetime_String :=
        To_ISO_8601_With_Offset (C, Offset);
      Str    : constant String := Datetime_Strings.To_String (Result);
   begin
      Run_Test ("To_ISO_8601_With_Offset UTC (Z)",
        Str = "2025-12-04T14:30:00Z");
   end;

   declare
      C : constant VOC.Civil := Make_Civil (2025, 12, 4, 14, 30, 0, 0);
      Offset : constant VOD.Duration_Type := VOD.From_Seconds (-5 * 3600);
      Result : constant Datetime_String :=
        To_ISO_8601_With_Offset (C, Offset);
      Str    : constant String := Datetime_Strings.To_String (Result);
   begin
      Run_Test ("To_ISO_8601_With_Offset -05:00",
        Str = "2025-12-04T14:30:00-05:00");
   end;

   declare
      C : constant VOC.Civil := Make_Civil (2025, 7, 15, 10, 0, 0, 0);
      Offset : constant VOD.Duration_Type :=
        VOD.From_Seconds (9 * 3600 + 30 * 60);
      Result : constant Datetime_String :=
        To_ISO_8601_With_Offset (C, Offset);
      Str    : constant String := Datetime_Strings.To_String (Result);
   begin
      Run_Test ("To_ISO_8601_With_Offset +09:30",
        Str = "2025-07-15T10:00:00+09:30");
   end;

   --  ========================================================================
   --  Test: To_ISO_8601_With_Zone
   --  ========================================================================

   Put_Line ("Test: To_ISO_8601_With_Zone");

   declare
      C : constant VOC.Civil := Make_Civil (2025, 12, 4, 14, 30, 0, 0);
      Zone_R : constant Domain.Value_Object.Zone_ID.Zone_ID_Result.Result :=
        Domain.Value_Object.Zone_ID.From_String ("America/New_York");
   begin
      if Domain.Value_Object.Zone_ID.Zone_ID_Result.Is_Ok (Zone_R) then
         declare
            Zone : constant Domain.Value_Object.Zone_ID.Zone_ID :=
              Domain.Value_Object.Zone_ID.Zone_ID_Result.Value (Zone_R);
            Result : constant Datetime_String :=
              To_ISO_8601_With_Zone (C, Zone);
            Str    : constant String := Datetime_Strings.To_String (Result);
         begin
            Run_Test ("To_ISO_8601_With_Zone America/New_York",
              Str = "2025-12-04T14:30:00[America/New_York]");
         end;
      else
         Run_Test ("To_ISO_8601_With_Zone setup", False);
      end if;
   end;

   --  ========================================================================
   --  Test: To_ISO_8601_Full
   --  ========================================================================

   Put_Line ("Test: To_ISO_8601_Full");

   declare
      C : constant VOC.Civil := Make_Civil (2025, 12, 4, 14, 30, 0, 0);
      Offset : constant VOD.Duration_Type := VOD.From_Seconds (-5 * 3600);
      Zone_R : constant Domain.Value_Object.Zone_ID.Zone_ID_Result.Result :=
        Domain.Value_Object.Zone_ID.From_String ("America/New_York");
   begin
      if Domain.Value_Object.Zone_ID.Zone_ID_Result.Is_Ok (Zone_R) then
         declare
            Zone : constant Domain.Value_Object.Zone_ID.Zone_ID :=
              Domain.Value_Object.Zone_ID.Zone_ID_Result.Value (Zone_R);
            Result : constant Datetime_String :=
              To_ISO_8601_Full (C, Offset, Zone);
            Str    : constant String := Datetime_Strings.To_String (Result);
         begin
            Run_Test ("To_ISO_8601_Full offset+zone",
              Str = "2025-12-04T14:30:00-05:00[America/New_York]");
         end;
      else
         Run_Test ("To_ISO_8601_Full setup", False);
      end if;
   end;

   --  ========================================================================
   --  Test: To_ISO_Date
   --  ========================================================================

   Put_Line ("Test: To_ISO_Date");

   declare
      C : constant VOC.Civil := Make_Civil (2025, 12, 4, 14, 30, 45, 0);
      Result : constant Datetime_String := To_ISO_Date (C);
      Str    : constant String := Datetime_Strings.To_String (Result);
   begin
      Run_Test ("To_ISO_Date format",
        Str = "2025-12-04");
   end;

   declare
      C : constant VOC.Civil := Make_Civil (2025, 1, 5, 0, 0, 0, 0);
      Result : constant Datetime_String := To_ISO_Date (C);
      Str    : constant String := Datetime_Strings.To_String (Result);
   begin
      Run_Test ("To_ISO_Date leading zeros",
        Str = "2025-01-05");
   end;

   --  ========================================================================
   --  Test: To_ISO_Time
   --  ========================================================================

   Put_Line ("Test: To_ISO_Time");

   declare
      C : constant VOC.Civil := Make_Civil (2025, 12, 4, 14, 30, 45, 0);
      Result : constant Datetime_String := To_ISO_Time (C);
      Str    : constant String := Datetime_Strings.To_String (Result);
   begin
      Run_Test ("To_ISO_Time format",
        Str = "14:30:45");
   end;

   declare
      C : constant VOC.Civil := Make_Civil (2025, 12, 4, 9, 5, 3, 0);
      Result : constant Datetime_String := To_ISO_Time (C);
      Str    : constant String := Datetime_Strings.To_String (Result);
   begin
      Run_Test ("To_ISO_Time leading zeros",
        Str = "09:05:03");
   end;

   declare
      C : constant VOC.Civil :=
        Make_Civil (2025, 12, 4, 14, 30, 45, 500_000_000);
      Result : constant Datetime_String := To_ISO_Time (C);
      Str    : constant String := Datetime_Strings.To_String (Result);
   begin
      Run_Test ("To_ISO_Time with nanos",
        Str = "14:30:45.5");
   end;

   --  ========================================================================
   --  Test: To_Epoch_String
   --  ========================================================================

   Put_Line ("Test: To_Epoch_String");

   declare
      --  Create instant for 2025-01-01 00:00:00 UTC = 1735689600
      package VOI renames Domain.Value_Object.Instant;
      I : constant Instant :=
        VOI.From_Epoch_Nanos (1_735_689_600_000_000_000);
      Result : constant Datetime_String := To_Epoch_String (I);
      Str    : constant String := Datetime_Strings.To_String (Result);
   begin
      Run_Test ("To_Epoch_String 2025-01-01",
        Str = "1735689600");
   end;

   declare
      package VOI renames Domain.Value_Object.Instant;
      I : constant Instant :=
        VOI.From_Epoch_Nanos (1_735_689_600_500_000_000);
      Result : constant Datetime_String := To_Epoch_String (I);
      Str    : constant String := Datetime_Strings.To_String (Result);
   begin
      Run_Test ("To_Epoch_String with nanos",
        Str = "1735689600.5");
   end;

   --  ========================================================================
   --  Test: To_ISO_Duration
   --  ========================================================================

   Put_Line ("Test: To_ISO_Duration");

   declare
      D : constant VOD.Duration_Type := VOD.From_Seconds (3661);  --  1h 1m 1s
      Result : constant Duration_String := To_ISO_Duration (D);
      Str    : constant String := Duration_Strings.To_String (Result);
   begin
      Run_Test ("To_ISO_Duration 1h 1m 1s",
        Str = "PT1H1M1S");
   end;

   declare
      D : constant VOD.Duration_Type :=
        VOD.From_Seconds (86400 + 3600);  --  1d 1h
      Result : constant Duration_String := To_ISO_Duration (D);
      Str    : constant String := Duration_Strings.To_String (Result);
   begin
      Run_Test ("To_ISO_Duration 1d 1h",
        Str = "P1DT1H");
   end;

   declare
      D : constant VOD.Duration_Type := VOD.Zero;
      Result : constant Duration_String := To_ISO_Duration (D);
      Str    : constant String := Duration_Strings.To_String (Result);
   begin
      Run_Test ("To_ISO_Duration zero",
        Str = "PT0S");
   end;

   declare
      D : constant VOD.Duration_Type :=
        VOD.Negate (VOD.From_Seconds (300));  --  -5m
      Result : constant Duration_String := To_ISO_Duration (D);
      Str    : constant String := Duration_Strings.To_String (Result);
   begin
      Run_Test ("To_ISO_Duration negative",
        Str = "-PT5M");
   end;

   --  ========================================================================
   --  Test: To_Human_Duration
   --  ========================================================================

   Put_Line ("Test: To_Human_Duration");

   declare
      D : constant VOD.Duration_Type := VOD.From_Seconds (3661);
      Result : constant Duration_String := To_Human_Duration (D);
      Str    : constant String := Duration_Strings.To_String (Result);
   begin
      Run_Test ("To_Human_Duration 1h 1m 1s",
        Str = "1h 1m 1s");
   end;

   declare
      D : constant VOD.Duration_Type :=
        VOD.From_Seconds (86400 + 43200);  --  1d 12h
      Result : constant Duration_String := To_Human_Duration (D);
      Str    : constant String := Duration_Strings.To_String (Result);
   begin
      Run_Test ("To_Human_Duration 1d 12h",
        Str = "1d 12h");
   end;

   declare
      D : constant VOD.Duration_Type := VOD.Zero;
      Result : constant Duration_String := To_Human_Duration (D);
      Str    : constant String := Duration_Strings.To_String (Result);
   begin
      Run_Test ("To_Human_Duration zero",
        Str = "0s");
   end;

   --  ========================================================================
   --  Test: Format_Offset
   --  ========================================================================

   Put_Line ("Test: Format_Offset");

   declare
      Offset : constant VOD.Duration_Type := VOD.Zero;
      Result : constant Datetime_String := Format_Offset (Offset);
      Str    : constant String := Datetime_Strings.To_String (Result);
   begin
      Run_Test ("Format_Offset UTC",
        Str = "Z");
   end;

   declare
      Offset : constant VOD.Duration_Type := VOD.From_Seconds (5 * 3600);
      Result : constant Datetime_String := Format_Offset (Offset);
      Str    : constant String := Datetime_Strings.To_String (Result);
   begin
      Run_Test ("Format_Offset +05:00",
        Str = "+05:00");
   end;

   declare
      Offset : constant VOD.Duration_Type := VOD.From_Seconds (-8 * 3600);
      Result : constant Datetime_String := Format_Offset (Offset);
      Str    : constant String := Datetime_Strings.To_String (Result);
   begin
      Run_Test ("Format_Offset -08:00",
        Str = "-08:00");
   end;

   --  Print summary
   New_Line;
   Put_Line ("========================================");
   Put_Line ("Test Summary: Zoneinfo.API.Format");
   Put_Line ("========================================");
   Put_Line ("Total tests: " & Total_Tests'Image);
   Put_Line ("Passed:      " & Passed_Tests'Image);
   Put_Line ("Failed:      " & Natural'Image (Total_Tests - Passed_Tests));
   New_Line;

   Test_Framework.Register_Results (Total_Tests, Passed_Tests);

end Test_API_Format;
