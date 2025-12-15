pragma Ada_2022;
--  ======================================================================
--  Test_API_Parse
--  ======================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--  Purpose:
--    Unit tests for Zoneinfo.API.Parse - datetime string parsing.
--  ======================================================================
with Ada.Text_IO; use Ada.Text_IO;
with Interfaces; use Interfaces;
with Zoneinfo.API.Parse;
with Domain.Value_Object.Civil;
with Domain.Value_Object.Duration_Type;
with Domain.Value_Object.Zone_ID;
with Test_Framework;

procedure Test_API_Parse is

   use Zoneinfo.API.Parse;

   --  Aliases to avoid ambiguity with API re-exports
   package VOC renames Domain.Value_Object.Civil;
   package VOD renames Domain.Value_Object.Duration_Type;
   package VOZ renames Domain.Value_Object.Zone_ID;

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
   Put_Line ("Testing: Zoneinfo.API.Parse");
   Put_Line ("========================================");
   New_Line;

   --  ========================================================================
   --  Test: From_ISO_8601 - Basic datetime parsing
   --  ========================================================================

   Put_Line ("Test: From_ISO_8601");

   declare
      R : constant VOC.Civil_Result.Result :=
        From_ISO_8601 ("2025-12-04T14:30:45");
   begin
      Run_Test ("From_ISO_8601 valid datetime",
        VOC.Civil_Result.Is_Ok (R));
      if VOC.Civil_Result.Is_Ok (R) then
         declare
            C : constant VOC.Civil := VOC.Civil_Result.Value (R);
         begin
            Run_Test ("From_ISO_8601 year", VOC.Get_Year (C) = 2025);
            Run_Test ("From_ISO_8601 month", VOC.Get_Month (C) = 12);
            Run_Test ("From_ISO_8601 day", VOC.Get_Day (C) = 4);
            Run_Test ("From_ISO_8601 hour", VOC.Get_Hour (C) = 14);
            Run_Test ("From_ISO_8601 minute", VOC.Get_Minute (C) = 30);
            Run_Test ("From_ISO_8601 second", VOC.Get_Second (C) = 45);
         end;
      end if;
   end;

   declare
      R : constant VOC.Civil_Result.Result :=
        From_ISO_8601 ("2025-12-04T14:30:45.123456789");
   begin
      Run_Test ("From_ISO_8601 with nanos",
        VOC.Civil_Result.Is_Ok (R));
      if VOC.Civil_Result.Is_Ok (R) then
         declare
            C : constant VOC.Civil := VOC.Civil_Result.Value (R);
         begin
            Run_Test ("From_ISO_8601 nanosecond",
              VOC.Get_Nanosecond (C) = 123_456_789);
         end;
      end if;
   end;

   declare
      R : constant VOC.Civil_Result.Result :=
        From_ISO_8601 ("2025-01-01T00:00:00");
   begin
      Run_Test ("From_ISO_8601 midnight",
        VOC.Civil_Result.Is_Ok (R));
   end;

   --  Error cases
   declare
      R : constant VOC.Civil_Result.Result := From_ISO_8601 ("invalid");
   begin
      Run_Test ("From_ISO_8601 rejects invalid",
        VOC.Civil_Result.Is_Error (R));
   end;

   declare
      R : constant VOC.Civil_Result.Result :=
        From_ISO_8601 ("2025-13-01T00:00:00");
   begin
      Run_Test ("From_ISO_8601 rejects invalid month",
        VOC.Civil_Result.Is_Error (R));
   end;

   declare
      R : constant VOC.Civil_Result.Result :=
        From_ISO_8601 ("2025-12-04T25:00:00");
   begin
      Run_Test ("From_ISO_8601 rejects invalid hour",
        VOC.Civil_Result.Is_Error (R));
   end;

   --  ========================================================================
   --  Test: From_ISO_8601_With_Offset
   --  ========================================================================

   Put_Line ("Test: From_ISO_8601_With_Offset");

   declare
      R : constant Civil_Offset_Result.Result :=
        From_ISO_8601_With_Offset ("2025-12-04T14:30:00-05:00");
   begin
      Run_Test ("From_ISO_8601_With_Offset valid",
        Civil_Offset_Result.Is_Ok (R));
      if Civil_Offset_Result.Is_Ok (R) then
         declare
            CWO : constant Civil_With_Offset := Civil_Offset_Result.Value (R);
         begin
            Run_Test ("From_ISO_8601_With_Offset has_offset",
              CWO.Has_Offset = True);
            Run_Test ("From_ISO_8601_With_Offset offset value",
              VOD.Get_Seconds (CWO.Offset) = -5 * 3600);
         end;
      end if;
   end;

   declare
      R : constant Civil_Offset_Result.Result :=
        From_ISO_8601_With_Offset ("2025-12-04T14:30:00Z");
   begin
      Run_Test ("From_ISO_8601_With_Offset UTC (Z)",
        Civil_Offset_Result.Is_Ok (R));
      if Civil_Offset_Result.Is_Ok (R) then
         declare
            CWO : constant Civil_With_Offset := Civil_Offset_Result.Value (R);
         begin
            Run_Test ("From_ISO_8601_With_Offset Z offset is zero",
              VOD.Get_Seconds (CWO.Offset) = 0);
         end;
      end if;
   end;

   declare
      R : constant Civil_Offset_Result.Result :=
        From_ISO_8601_With_Offset ("2025-12-04T14:30:00+09:30");
   begin
      Run_Test ("From_ISO_8601_With_Offset +09:30",
        Civil_Offset_Result.Is_Ok (R));
      if Civil_Offset_Result.Is_Ok (R) then
         declare
            CWO : constant Civil_With_Offset := Civil_Offset_Result.Value (R);
         begin
            Run_Test ("From_ISO_8601_With_Offset +09:30 value",
              VOD.Get_Seconds (CWO.Offset) = 9 * 3600 + 30 * 60);
         end;
      end if;
   end;

   --  ========================================================================
   --  Test: From_ISO_8601_With_Zone
   --  ========================================================================

   Put_Line ("Test: From_ISO_8601_With_Zone");

   declare
      R : constant Civil_Zone_Result.Result :=
        From_ISO_8601_With_Zone ("2025-12-04T14:30:00[America/New_York]");
   begin
      Run_Test ("From_ISO_8601_With_Zone valid",
        Civil_Zone_Result.Is_Ok (R));
      if Civil_Zone_Result.Is_Ok (R) then
         declare
            CWZ : constant Civil_With_Zone := Civil_Zone_Result.Value (R);
         begin
            Run_Test ("From_ISO_8601_With_Zone zone",
              VOZ.To_String (CWZ.Zone) = "America/New_York");
         end;
      end if;
   end;

   declare
      R : constant Civil_Zone_Result.Result :=
        From_ISO_8601_With_Zone ("2025-12-04T14:30:00[UTC]");
   begin
      Run_Test ("From_ISO_8601_With_Zone UTC",
        Civil_Zone_Result.Is_Ok (R));
   end;

   declare
      R : constant Civil_Zone_Result.Result :=
        From_ISO_8601_With_Zone ("2025-12-04T14:30:00");
   begin
      Run_Test ("From_ISO_8601_With_Zone rejects no zone",
        Civil_Zone_Result.Is_Error (R));
   end;

   --  ========================================================================
   --  Test: From_ISO_8601_Full
   --  ========================================================================

   Put_Line ("Test: From_ISO_8601_Full");

   declare
      R : constant Civil_Full_Result.Result :=
        From_ISO_8601_Full ("2025-12-04T14:30:00-05:00[America/New_York]");
   begin
      Run_Test ("From_ISO_8601_Full offset+zone",
        Civil_Full_Result.Is_Ok (R));
      if Civil_Full_Result.Is_Ok (R) then
         declare
            CF : constant Civil_Full := Civil_Full_Result.Value (R);
         begin
            Run_Test ("From_ISO_8601_Full has_offset", CF.Has_Offset);
            Run_Test ("From_ISO_8601_Full has_zone", CF.Has_Zone);
         end;
      end if;
   end;

   declare
      R : constant Civil_Full_Result.Result :=
        From_ISO_8601_Full ("2025-12-04T14:30:00Z");
   begin
      Run_Test ("From_ISO_8601_Full Z only",
        Civil_Full_Result.Is_Ok (R));
   end;

   declare
      R : constant Civil_Full_Result.Result :=
        From_ISO_8601_Full ("2025-12-04T14:30:00");
   begin
      Run_Test ("From_ISO_8601_Full no offset/zone",
        Civil_Full_Result.Is_Ok (R));
      if Civil_Full_Result.Is_Ok (R) then
         declare
            CF : constant Civil_Full := Civil_Full_Result.Value (R);
         begin
            Run_Test ("From_ISO_8601_Full no has_offset",
              not CF.Has_Offset);
            Run_Test ("From_ISO_8601_Full no has_zone",
              not CF.Has_Zone);
         end;
      end if;
   end;

   --  ========================================================================
   --  Test: From_ISO_Date
   --  ========================================================================

   Put_Line ("Test: From_ISO_Date");

   declare
      R : constant VOC.Civil_Result.Result := From_ISO_Date ("2025-12-04");
   begin
      Run_Test ("From_ISO_Date valid",
        VOC.Civil_Result.Is_Ok (R));
      if VOC.Civil_Result.Is_Ok (R) then
         declare
            C : constant VOC.Civil := VOC.Civil_Result.Value (R);
         begin
            Run_Test ("From_ISO_Date year", VOC.Get_Year (C) = 2025);
            Run_Test ("From_ISO_Date month", VOC.Get_Month (C) = 12);
            Run_Test ("From_ISO_Date day", VOC.Get_Day (C) = 4);
            Run_Test ("From_ISO_Date hour is 0", VOC.Get_Hour (C) = 0);
         end;
      end if;
   end;

   --  ========================================================================
   --  Test: From_ISO_Time
   --  ========================================================================

   Put_Line ("Test: From_ISO_Time");

   declare
      R : constant VOC.Civil_Result.Result := From_ISO_Time ("14:30:45");
   begin
      Run_Test ("From_ISO_Time valid",
        VOC.Civil_Result.Is_Ok (R));
      if VOC.Civil_Result.Is_Ok (R) then
         declare
            C : constant VOC.Civil := VOC.Civil_Result.Value (R);
         begin
            Run_Test ("From_ISO_Time hour", VOC.Get_Hour (C) = 14);
            Run_Test ("From_ISO_Time minute", VOC.Get_Minute (C) = 30);
            Run_Test ("From_ISO_Time second", VOC.Get_Second (C) = 45);
            Run_Test ("From_ISO_Time year is epoch", VOC.Get_Year (C) = 1970);
         end;
      end if;
   end;

   declare
      R : constant VOC.Civil_Result.Result := From_ISO_Time ("14:30:45.123");
   begin
      Run_Test ("From_ISO_Time with nanos",
        VOC.Civil_Result.Is_Ok (R));
   end;

   --  ========================================================================
   --  Test: From_ISO_Duration
   --  ========================================================================

   Put_Line ("Test: From_ISO_Duration");

   declare
      R : constant Duration_Result.Result := From_ISO_Duration ("PT1H30M45S");
   begin
      Run_Test ("From_ISO_Duration PT1H30M45S",
        Duration_Result.Is_Ok (R));
      if Duration_Result.Is_Ok (R) then
         declare
            D : constant VOD.Duration_Type := Duration_Result.Value (R);
            Expected : constant Integer_64 := 1 * 3600 + 30 * 60 + 45;
         begin
            Run_Test ("From_ISO_Duration value",
              VOD.Get_Seconds (D) = Expected);
         end;
      end if;
   end;

   declare
      R : constant Duration_Result.Result := From_ISO_Duration ("P1DT12H");
   begin
      Run_Test ("From_ISO_Duration P1DT12H",
        Duration_Result.Is_Ok (R));
      if Duration_Result.Is_Ok (R) then
         declare
            D : constant VOD.Duration_Type := Duration_Result.Value (R);
            Expected : constant Integer_64 := 86400 + 12 * 3600;
         begin
            Run_Test ("From_ISO_Duration days+hours",
              VOD.Get_Seconds (D) = Expected);
         end;
      end if;
   end;

   declare
      R : constant Duration_Result.Result := From_ISO_Duration ("-PT5M");
   begin
      Run_Test ("From_ISO_Duration negative",
        Duration_Result.Is_Ok (R));
      if Duration_Result.Is_Ok (R) then
         declare
            D : constant VOD.Duration_Type := Duration_Result.Value (R);
         begin
            Run_Test ("From_ISO_Duration negative value",
              VOD.Get_Seconds (D) = -300);
         end;
      end if;
   end;

   declare
      R : constant Duration_Result.Result := From_ISO_Duration ("PT0S");
   begin
      Run_Test ("From_ISO_Duration zero",
        Duration_Result.Is_Ok (R));
   end;

   declare
      R : constant Duration_Result.Result := From_ISO_Duration ("");
   begin
      Run_Test ("From_ISO_Duration rejects empty",
        Duration_Result.Is_Error (R));
   end;

   declare
      R : constant Duration_Result.Result := From_ISO_Duration ("1H30M");
   begin
      Run_Test ("From_ISO_Duration rejects no P",
        Duration_Result.Is_Error (R));
   end;

   --  ========================================================================
   --  Test: From_Human_Duration
   --  ========================================================================

   Put_Line ("Test: From_Human_Duration");

   declare
      R : constant Duration_Result.Result :=
        From_Human_Duration ("1h 30m 45s");
   begin
      Run_Test ("From_Human_Duration 1h 30m 45s",
        Duration_Result.Is_Ok (R));
      if Duration_Result.Is_Ok (R) then
         declare
            D : constant VOD.Duration_Type := Duration_Result.Value (R);
            Expected : constant Integer_64 := 3600 + 30 * 60 + 45;
         begin
            Run_Test ("From_Human_Duration value",
              VOD.Get_Seconds (D) = Expected);
         end;
      end if;
   end;

   declare
      R : constant Duration_Result.Result := From_Human_Duration ("2d 12h");
   begin
      Run_Test ("From_Human_Duration 2d 12h",
        Duration_Result.Is_Ok (R));
   end;

   declare
      R : constant Duration_Result.Result := From_Human_Duration ("-5m 30s");
   begin
      Run_Test ("From_Human_Duration negative",
        Duration_Result.Is_Ok (R));
      if Duration_Result.Is_Ok (R) then
         declare
            D : constant VOD.Duration_Type := Duration_Result.Value (R);
         begin
            Run_Test ("From_Human_Duration negative value",
              VOD.Get_Seconds (D) = -(5 * 60 + 30));
         end;
      end if;
   end;

   --  ========================================================================
   --  Test: Parse_Offset
   --  ========================================================================

   Put_Line ("Test: Parse_Offset");

   declare
      R : constant Duration_Result.Result := Parse_Offset ("Z");
   begin
      Run_Test ("Parse_Offset Z",
        Duration_Result.Is_Ok (R));
      if Duration_Result.Is_Ok (R) then
         Run_Test ("Parse_Offset Z is zero",
           VOD.Get_Seconds (Duration_Result.Value (R)) = 0);
      end if;
   end;

   declare
      R : constant Duration_Result.Result := Parse_Offset ("+05:00");
   begin
      Run_Test ("Parse_Offset +05:00",
        Duration_Result.Is_Ok (R));
      if Duration_Result.Is_Ok (R) then
         Run_Test ("Parse_Offset +05:00 value",
           VOD.Get_Seconds (Duration_Result.Value (R)) = 5 * 3600);
      end if;
   end;

   declare
      R : constant Duration_Result.Result := Parse_Offset ("-08:30");
   begin
      Run_Test ("Parse_Offset -08:30",
        Duration_Result.Is_Ok (R));
      if Duration_Result.Is_Ok (R) then
         Run_Test
           ("Parse_Offset -08:30 value",
            VOD.Get_Seconds (Duration_Result.Value (R)) =
              -(8 * 3600 + 30 * 60));
      end if;
   end;

   declare
      R : constant Duration_Result.Result := Parse_Offset ("+0530");
   begin
      Run_Test ("Parse_Offset +0530 compact",
        Duration_Result.Is_Ok (R));
   end;

   declare
      R : constant Duration_Result.Result := Parse_Offset ("");
   begin
      Run_Test ("Parse_Offset rejects empty",
        Duration_Result.Is_Error (R));
   end;

   declare
      R : constant Duration_Result.Result := Parse_Offset ("05:00");
   begin
      Run_Test ("Parse_Offset rejects no sign",
        Duration_Result.Is_Error (R));
   end;

   --  Print summary
   New_Line;
   Put_Line ("========================================");
   Put_Line ("Test Summary: Zoneinfo.API.Parse");
   Put_Line ("========================================");
   Put_Line ("Total tests: " & Total_Tests'Image);
   Put_Line ("Passed:      " & Passed_Tests'Image);
   Put_Line ("Failed:      " & Natural'Image (Total_Tests - Passed_Tests));
   New_Line;

   Test_Framework.Register_Results (Total_Tests, Passed_Tests);

end Test_API_Parse;
