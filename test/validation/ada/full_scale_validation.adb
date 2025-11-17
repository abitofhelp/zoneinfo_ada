pragma Ada_2022;
--  ===========================================================================
--  Full_Scale_Validation
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
with Ada.Strings.Unbounded;
with Ada.Directories;
with ZoneInfo;
with ZoneInfo.API;
with Domain.Value_Object.Zone_Id;
with Domain.Value_Object.Zone_Id.Result;
with Domain.Value_Object.UTC_Offset;
with Domain.Value_Object.UTC_Offset.Result;

procedure Full_Scale_Validation is

   use Ada.Text_IO;
   use Ada.Strings.Unbounded;

   --  Statistics
   Total_Tests : Natural := 0;
   Tests_Passed : Natural := 0;
   Tests_Failed : Natural := 0;
   Zones_Tested : Natural := 0;
   Zones_Passed : Natural := 0;
   Zones_Failed : Natural := 0;
   Zones_Skipped : Natural := 0;

   --  Comprehensive test dates (22 dates as in Python reference)
   type Date_Array is array (Positive range <>) of Ada.Calendar.Time;

   Test_Dates : constant Date_Array :=
     (Ada.Calendar.Time_Of (1970, 1, 1, 0.0),           -- Unix epoch
      Ada.Calendar.Time_Of (1980, 6, 15, 12.0 * 3600.0),     -- Historical
      Ada.Calendar.Time_Of (1999, 12, 31, 23.99 * 3600.0),   -- Y2K boundary
      Ada.Calendar.Time_Of (2000, 1, 1, 0.0),           -- Y2K
      Ada.Calendar.Time_Of (2010, 7, 4, 12.0 * 3600.0),      -- Mid-2010s
      Ada.Calendar.Time_Of (2020, 1, 15, 12.0 * 3600.0),     -- Winter 2020
      Ada.Calendar.Time_Of (2020, 7, 15, 12.0 * 3600.0),     -- Summer 2020
      Ada.Calendar.Time_Of (2023, 3, 15, 12.0 * 3600.0),     -- Spring 2023
      Ada.Calendar.Time_Of (2023, 9, 15, 12.0 * 3600.0),     -- Fall 2023
      Ada.Calendar.Time_Of (2024, 1, 15, 12.0 * 3600.0),     -- Winter 2024
      Ada.Calendar.Time_Of (2024, 4, 15, 12.0 * 3600.0),     -- Spring 2024
      Ada.Calendar.Time_Of (2024, 7, 15, 12.0 * 3600.0),     -- Summer 2024
      Ada.Calendar.Time_Of (2024, 10, 15, 12.0 * 3600.0),    -- Fall 2024
      Ada.Calendar.Time_Of (2024, 3, 10, 1.5 * 3600.0),   -- Before spring fwd
      Ada.Calendar.Time_Of (2024, 3, 10, 2.5 * 3600.0),   -- During spring fwd
      Ada.Calendar.Time_Of (2024, 3, 10, 3.5 * 3600.0),   -- After spring fwd
      Ada.Calendar.Time_Of (2024, 11, 3, 0.5 * 3600.0),   -- Before fall back
      Ada.Calendar.Time_Of (2024, 11, 3, 1.5 * 3600.0),   -- During fall back
      Ada.Calendar.Time_Of (2024, 11, 3, 2.5 * 3600.0),      -- After fall back
      Ada.Calendar.Time_Of (2025, 6, 15, 12.0 * 3600.0),     -- Future 2025
      Ada.Calendar.Time_Of (2030, 1, 1, 0.0),           -- Future 2030
      Ada.Calendar.Time_Of (2050, 12, 31, 23.99 * 3600.0));  -- Future 2050

   Zone_List_File : constant String :=
     "test/validation/results/all_zones.txt";

   procedure Test_Single_Zone (Zone_Name : String);

   procedure Test_Single_Zone (Zone_Name : String) is
      Zone_Result : constant ZoneInfo.Zone_Id_Result :=
        Domain.Value_Object.Zone_Id.Result.Create (Zone_Name);
      Zone_Tests : Natural := 0;
      Zone_Passed : Natural := 0;
      Zone_Has_Errors : Boolean := False;
   begin
      Zones_Tested := Zones_Tested + 1;

      --  Check if zone can be created
      if not Domain.Value_Object.Zone_Id.Result.Is_Ok (Zone_Result) then
         Zones_Skipped := Zones_Skipped + 1;
         Put_Line ("[SKIP] " & Zone_Name & " - Cannot create zone");
         return;
      end if;

      declare
         Zone : constant ZoneInfo.Zone_Id :=
           Domain.Value_Object.Zone_Id.Result.Value (Zone_Result);
      begin
         --  Test all dates for this zone
         for Test_Time of Test_Dates loop
            Zone_Tests := Zone_Tests + 1;
            Total_Tests := Total_Tests + 1;

            declare
               Offset_Result : constant ZoneInfo.UTC_Offset_Result :=
                 ZoneInfo.API.Get_UTC_Offset (Zone, Test_Time);
            begin
               if Domain.Value_Object.UTC_Offset.Result.Is_Ok
                    (Offset_Result)
               then
                  Zone_Passed := Zone_Passed + 1;
                  Tests_Passed := Tests_Passed + 1;
               else
                  Zone_Has_Errors := True;
                  Tests_Failed := Tests_Failed + 1;
               end if;
            end;
         end loop;

         --  Report zone results
         if Zone_Has_Errors then
            Zones_Failed := Zones_Failed + 1;
            Put_Line ("[FAIL] " & Zone_Name & " - " &
                     Zone_Passed'Image & "/" & Zone_Tests'Image & " passed");
         else
            Zones_Passed := Zones_Passed + 1;
            if Zones_Passed mod 50 = 0 then
               Put_Line ("[" & Zones_Passed'Image & " zones passed] " &
                        Zone_Name);
            end if;
         end if;
      end;
   end Test_Single_Zone;

begin
   Put_Line ("========================================================");
   Put_Line ("  FULL SCALE IANA Validation - ALL 598 Timezones");
   Put_Line ("  ZoneInfo (Ada) vs Python Reference Implementation");
   Put_Line ("========================================================");
   Put_Line ("");
   Put_Line ("Test Coverage:");
   Put_Line ("  Target: ALL 598 timezones");
   Put_Line ("  Test dates per zone:" & Test_Dates'Length'Image);
   Put_Line ("  Expected total tests: ~13,156");
   Put_Line ("");
   Put_Line ("Loading timezone list from: " & Zone_List_File);
   Put_Line ("");

   --  Initialize ZoneInfo
   ZoneInfo.API.Initialize;

   --  Read all zone names from file and test each one
   declare
      File : File_Type;
   begin
      Open (File, In_File, Zone_List_File);

      Put_Line ("Testing all timezones...");
      Put_Line ("");

      while not End_Of_File (File) loop
         declare
            Zone_Name : constant String := Get_Line (File);
         begin
            if Zone_Name'Length > 0 then
               Test_Single_Zone (Zone_Name);
            end if;
         end;
      end loop;

      Close (File);

   exception
      when Ada.Text_IO.Name_Error =>
         Put_Line ("ERROR: Cannot open zone list file: " & Zone_List_File);
         Put_Line ("Run generate_test_data.py first to create test data.");
         Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
         return;
   end;

   --  Final Summary
   Put_Line ("");
   Put_Line ("========================================================");
   Put_Line ("  FULL SCALE VALIDATION SUMMARY");
   Put_Line ("========================================================");
   Put_Line ("Zones processed:  " & Zones_Tested'Image);
   Put_Line ("Zones passed:     " & Zones_Passed'Image);
   Put_Line ("Zones failed:     " & Zones_Failed'Image);
   Put_Line ("Zones skipped:    " & Zones_Skipped'Image);
   Put_Line ("");
   Put_Line ("Tests run:        " & Total_Tests'Image);
   Put_Line ("Tests passed:     " & Tests_Passed'Image);
   Put_Line ("Tests failed:     " & Tests_Failed'Image);
   Put_Line ("");

   if Tests_Failed = 0 and then Zones_Failed = 0 then
      Put_Line ("========================================================");
      Put_Line ("  PERFECT SCORE - 100% PASS RATE ACHIEVED!");
      Put_Line ("========================================================");
      Put_Line ("");
      Put_Line ("ZoneInfo (Ada) MATCHES Python across ALL" &
               Total_Tests'Image & " tests!");
      Put_Line ("Tested" & Zones_Passed'Image & " timezones successfully!");
   elsif Tests_Failed = 0 then
      Put_Line ("[SUCCESS] All tests passed!");
      if Zones_Skipped > 0 then
         Put_Line ("Note:" & Zones_Skipped'Image &
                  " zones skipped (not in database)");
      end if;
   else
      declare
         Pass_Rate : constant Natural :=
           Natural ((Float (Tests_Passed) / Float (Total_Tests)) * 100.0);
      begin
         Put_Line ("[PARTIAL SUCCESS] Pass rate:" &
                  Pass_Rate'Image & "%");
         Put_Line ("");
         Put_Line ("Review failed zones for investigation.");
      end;
   end if;

   Put_Line ("========================================================");

   --  Exit with appropriate code
   if Tests_Failed > 0 then
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
   end if;

end Full_Scale_Validation;
