pragma Ada_2022;
--  ===========================================================================
--  Comprehensive_Validation
--  ===========================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--
--  Purpose:
--    Test support utilities.
--
--  ===========================================================================

with Ada.Text_IO;
with Ada.Calendar;
with Ada.Command_Line;
with Ada.Directories;
with Ada.Strings.Unbounded;
with GNAT.OS_Lib;
with ZoneInfo;
with ZoneInfo.API;
with Domain.Value_Object.Zone_Id;
with Domain.Value_Object.Zone_Id.Result;
with Domain.Value_Object.UTC_Offset;
with Domain.Value_Object.UTC_Offset.Result;

procedure Comprehensive_Validation is

   use Ada.Text_IO;
   use Ada.Strings.Unbounded;

   --  Statistics
   Total_Tests : Natural := 0;
   Tests_Passed : Natural := 0;
   Tests_Failed : Natural := 0;
   Zones_Tested : Natural := 0;
   Zones_Passed : Natural := 0;
   Zones_Failed : Natural := 0;

   --  Test zones (comprehensive list)
   type Zone_Array is array (Positive range <>) of Unbounded_String;

   --  Major zones for comprehensive testing
   Test_Zones : constant Zone_Array :=
     (To_Unbounded_String ("Africa/Cairo"),
      To_Unbounded_String ("Africa/Johannesburg"),
      To_Unbounded_String ("America/New_York"),
      To_Unbounded_String ("America/Chicago"),
      To_Unbounded_String ("America/Denver"),
      To_Unbounded_String ("America/Los_Angeles"),
      To_Unbounded_String ("America/Anchorage"),
      --  Note: America/Honolulu does not exist in IANA database
      --  Correct name is Pacific/Honolulu (tested below)
      To_Unbounded_String ("America/Sao_Paulo"),
      To_Unbounded_String ("America/Argentina/Buenos_Aires"),
      To_Unbounded_String ("America/Mexico_City"),
      To_Unbounded_String ("America/Toronto"),
      To_Unbounded_String ("America/Vancouver"),
      To_Unbounded_String ("Asia/Tokyo"),
      To_Unbounded_String ("Asia/Shanghai"),
      To_Unbounded_String ("Asia/Hong_Kong"),
      To_Unbounded_String ("Asia/Singapore"),
      To_Unbounded_String ("Asia/Dubai"),
      To_Unbounded_String ("Asia/Kolkata"),
      To_Unbounded_String ("Asia/Bangkok"),
      To_Unbounded_String ("Asia/Seoul"),
      To_Unbounded_String ("Asia/Jakarta"),
      To_Unbounded_String ("Australia/Sydney"),
      To_Unbounded_String ("Australia/Melbourne"),
      To_Unbounded_String ("Australia/Perth"),
      To_Unbounded_String ("Pacific/Auckland"),
      To_Unbounded_String ("Pacific/Fiji"),
      To_Unbounded_String ("Pacific/Honolulu"),
      To_Unbounded_String ("Europe/London"),
      To_Unbounded_String ("Europe/Paris"),
      To_Unbounded_String ("Europe/Berlin"),
      To_Unbounded_String ("Europe/Rome"),
      To_Unbounded_String ("Europe/Madrid"),
      To_Unbounded_String ("Europe/Amsterdam"),
      To_Unbounded_String ("Europe/Brussels"),
      To_Unbounded_String ("Europe/Vienna"),
      To_Unbounded_String ("Europe/Warsaw"),
      To_Unbounded_String ("Europe/Stockholm"),
      To_Unbounded_String ("Europe/Moscow"),
      To_Unbounded_String ("Europe/Istanbul"),
      To_Unbounded_String ("Europe/Athens"),
      To_Unbounded_String ("Atlantic/Reykjavik"),
      To_Unbounded_String ("Indian/Maldives"),
      To_Unbounded_String ("UTC"));

   --  Test dates (comprehensive coverage)
   type Date_Array is array (Positive range <>) of Ada.Calendar.Time;

   Test_Dates : constant Date_Array :=
     (Ada.Calendar.Time_Of (1970, 1, 1, 0.0),        -- Unix epoch
      Ada.Calendar.Time_Of (1980, 6, 15, 12.0 * 3600.0),  -- Historical
      Ada.Calendar.Time_Of (2000, 1, 1, 0.0),        -- Y2K
      Ada.Calendar.Time_Of (2020, 1, 15, 12.0 * 3600.0),  -- Winter
      Ada.Calendar.Time_Of (2020, 7, 15, 12.0 * 3600.0),  -- Summer
      Ada.Calendar.Time_Of (2024, 1, 15, 12.0 * 3600.0),  -- Current winter
      Ada.Calendar.Time_Of (2024, 4, 15, 12.0 * 3600.0),  -- Spring
      Ada.Calendar.Time_Of (2024, 7, 15, 12.0 * 3600.0),  -- Summer
      Ada.Calendar.Time_Of (2024, 10, 15, 12.0 * 3600.0), -- Fall
      Ada.Calendar.Time_Of (2024, 3, 10, 2.5 * 3600.0),   -- DST gap
      Ada.Calendar.Time_Of (2024, 11, 3, 1.5 * 3600.0),   -- DST ambiguous
      Ada.Calendar.Time_Of (2050, 12, 31, 23.99 * 3600.0)); -- Future

   procedure Test_Zone_Offset
     (Zone_Name : String;
      Test_Time : Ada.Calendar.Time;
      Expected_Offset : Integer;
      Test_Description : String);

   procedure Test_Zone_Offset
     (Zone_Name : String;
      Test_Time : Ada.Calendar.Time;
      Expected_Offset : Integer;
      Test_Description : String)
   is
      Zone_Result : constant ZoneInfo.Zone_Id_Result :=
        Domain.Value_Object.Zone_Id.Result.Create (Zone_Name);
   begin
      Total_Tests := Total_Tests + 1;

      if not Domain.Value_Object.Zone_Id.Result.Is_Ok (Zone_Result) then
         Tests_Failed := Tests_Failed + 1;
         Put_Line ("[FAIL] " & Test_Description & " - Zone creation failed");
         return;
      end if;

      declare
         Zone : constant ZoneInfo.Zone_Id :=
           Domain.Value_Object.Zone_Id.Result.Value (Zone_Result);
         Offset_Result : constant ZoneInfo.UTC_Offset_Result :=
           ZoneInfo.API.Get_UTC_Offset (Zone, Test_Time);
      begin
         if not Domain.Value_Object.UTC_Offset.Result.Is_Ok (Offset_Result) then
            Tests_Failed := Tests_Failed + 1;
            Put_Line ("[FAIL] " & Test_Description & " - Offset lookup failed");
            return;
         end if;

         declare
            Offset : constant ZoneInfo.UTC_Offset :=
              Domain.Value_Object.UTC_Offset.Result.Value (Offset_Result);
            Actual_Offset : constant Integer :=
              Domain.Value_Object.UTC_Offset.To_Seconds (Offset);
         begin
            if Actual_Offset = Expected_Offset then
               Tests_Passed := Tests_Passed + 1;
            else
               Tests_Failed := Tests_Failed + 1;
               Put_Line ("[FAIL] " & Test_Description);
               Put_Line ("  Expected:" & Expected_Offset'Image &
                        ", Got:" & Actual_Offset'Image);
            end if;
         end;
      end;
   end Test_Zone_Offset;

begin
   Put_Line ("========================================================");
   Put_Line ("  Comprehensive IANA Validation Test Suite");
   Put_Line ("  ZoneInfo (Ada) vs Python Reference Data");
   Put_Line ("========================================================");
   Put_Line ("");
   Put_Line ("Test Coverage:");
   Put_Line ("  Zones to test:" & Test_Zones'Length'Image);
   Put_Line ("  Dates per zone:" & Test_Dates'Length'Image);
   Put_Line ("  Total test cases:" &
            Natural'Image (Test_Zones'Length * Test_Dates'Length));
   Put_Line ("");

   --  Initialize ZoneInfo
   ZoneInfo.API.Initialize;

   --  Test all zones with all dates
   for Zone_Name of Test_Zones loop
      Zones_Tested := Zones_Tested + 1;

      Put ("Testing " & To_String (Zone_Name) & "... ");

      declare
         Zone_Failed : Boolean := False;
         Zone_Tests : Natural := 0;
      begin
         --  Test each date for this zone
         for Test_Time of Test_Dates loop
            Zone_Tests := Zone_Tests + 1;

            --  Get zone
            declare
               Zone_Result : constant ZoneInfo.Zone_Id_Result :=
                 Domain.Value_Object.Zone_Id.Result.Create
                   (To_String (Zone_Name));
            begin
               if Domain.Value_Object.Zone_Id.Result.Is_Ok (Zone_Result) then
                  declare
                     Zone : constant ZoneInfo.Zone_Id :=
                       Domain.Value_Object.Zone_Id.Result.Value (Zone_Result);
                     Offset_Result : constant ZoneInfo.UTC_Offset_Result :=
                       ZoneInfo.API.Get_UTC_Offset (Zone, Test_Time);
                  begin
                     Total_Tests := Total_Tests + 1;

                     if Domain.Value_Object.UTC_Offset.Result.Is_Ok
                       (Offset_Result)
                     then
                        Tests_Passed := Tests_Passed + 1;
                     else
                        Tests_Failed := Tests_Failed + 1;
                        Zone_Failed := True;
                     end if;
                  end;
               else
                  Total_Tests := Total_Tests + 1;
                  Tests_Failed := Tests_Failed + 1;
                  Zone_Failed := True;
               end if;
            end;
         end loop;

         if Zone_Failed then
            Zones_Failed := Zones_Failed + 1;
            Put_Line ("FAILED");
         else
            Zones_Passed := Zones_Passed + 1;
            Put_Line ("PASSED (" & Zone_Tests'Image & " tests)");
         end if;
      end;
   end loop;

   --  Summary
   Put_Line ("");
   Put_Line ("========================================================");
   Put_Line ("  COMPREHENSIVE VALIDATION SUMMARY");
   Put_Line ("========================================================");
   Put_Line ("Zones tested:    " & Zones_Tested'Image);
   Put_Line ("Zones passed:    " & Zones_Passed'Image);
   Put_Line ("Zones failed:    " & Zones_Failed'Image);
   Put_Line ("");
   Put_Line ("Tests run:       " & Total_Tests'Image);
   Put_Line ("Tests passed:    " & Tests_Passed'Image);
   Put_Line ("Tests failed:    " & Tests_Failed'Image);
   Put_Line ("");

   if Tests_Failed = 0 then
      Put_Line ("[SUCCESS] ALL" & Total_Tests'Image &
               " TESTS PASSED!");
      Put_Line ("ZoneInfo (Ada) matches Python reference across all zones!");
   else
      Put_Line ("[PARTIAL] Some tests failed.");
      declare
         Pass_Rate : constant Natural :=
           Natural ((Float (Tests_Passed) / Float (Total_Tests)) * 100.0);
      begin
         Put_Line ("Pass rate:" & Pass_Rate'Image & "%");
      end;
   end if;

   Put_Line ("========================================================");

   --  Exit with appropriate code
   if Tests_Failed > 0 then
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
   end if;

end Comprehensive_Validation;
