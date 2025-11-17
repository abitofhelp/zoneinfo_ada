pragma Ada_2022;
--  ===========================================================================
--  Comprehensive ZoneInfo.API Test
--  ===========================================================================
--  Demonstrates:
--    - Get_UTC_Offset for multiple timezones
--    - Convert_Time between timezones
--    - Error handling for invalid zones
--    - Railway-oriented programming patterns
--  ===========================================================================

with Ada.Text_IO;
with Ada.Calendar;
with ZoneInfo;
with ZoneInfo.API;
with Domain.Value_Object.Zone_Id;
with Domain.Value_Object.Zone_Id.Result;
with Domain.Value_Object.UTC_Offset;
with Domain.Value_Object.UTC_Offset.Result;
with Domain.Time_Conversion;

procedure Comprehensive_Test is

   use Ada.Text_IO;

   --  Test counter
   Tests_Passed : Natural := 0;
   Tests_Failed : Natural := 0;

   procedure Test_Header (Name : String) is
   begin
      Put_Line ("");
      Put_Line ("========================================");
      Put_Line ("  TEST: " & Name);
      Put_Line ("========================================");
   end Test_Header;

   procedure Test_Result (Passed : Boolean; Message : String) is
   begin
      if Passed then
         Put_Line ("[PASS] " & Message);
         Tests_Passed := Tests_Passed + 1;
      else
         Put_Line ("[FAIL] " & Message);
         Tests_Failed := Tests_Failed + 1;
      end if;
   end Test_Result;

   --  ========================================================================
   --  Test 1: Get UTC Offset for New York
   --  ========================================================================
   procedure Test_NYC_Offset is
      Zone_Result : constant ZoneInfo.Zone_Id_Result :=
        Domain.Value_Object.Zone_Id.Result.Create ("America/New_York");
      Now : constant Ada.Calendar.Time := Ada.Calendar.Clock;
   begin
      Test_Header ("Get UTC Offset - America/New_York");

      if Domain.Value_Object.Zone_Id.Result.Is_Ok (Zone_Result) then
         declare
            NYC_Zone : constant ZoneInfo.Zone_Id :=
              Domain.Value_Object.Zone_Id.Result.Value (Zone_Result);
            Offset_Result : constant ZoneInfo.UTC_Offset_Result :=
              ZoneInfo.API.Get_UTC_Offset (NYC_Zone, Now);
         begin
            if Domain.Value_Object.UTC_Offset.Result.Is_Ok (Offset_Result) then
               declare
                  Offset : constant ZoneInfo.UTC_Offset :=
                    Domain.Value_Object.UTC_Offset.Result.Value (Offset_Result);
                  Offset_Str : constant String :=
                    Domain.Value_Object.UTC_Offset.To_String (Offset);
               begin
                  Test_Result (True, "Got offset: " & Offset_Str);
                  Test_Result
                    (Offset_Str = "-05:00" or Offset_Str = "-04:00",
                     "Offset is valid for NYC (EST/EDT)");
               end;
            else
               Test_Result (False, "Failed to get offset");
            end if;
         end;
      else
         Test_Result (False, "Failed to create zone");
      end if;
   end Test_NYC_Offset;

   --  ========================================================================
   --  Test 2: Get UTC Offset for London
   --  ========================================================================
   procedure Test_London_Offset is
      Zone_Result : constant ZoneInfo.Zone_Id_Result :=
        Domain.Value_Object.Zone_Id.Result.Create ("Europe/London");
      Now : constant Ada.Calendar.Time := Ada.Calendar.Clock;
   begin
      Test_Header ("Get UTC Offset - Europe/London");

      if Domain.Value_Object.Zone_Id.Result.Is_Ok (Zone_Result) then
         declare
            London_Zone : constant ZoneInfo.Zone_Id :=
              Domain.Value_Object.Zone_Id.Result.Value (Zone_Result);
            Offset_Result : constant ZoneInfo.UTC_Offset_Result :=
              ZoneInfo.API.Get_UTC_Offset (London_Zone, Now);
         begin
            if Domain.Value_Object.UTC_Offset.Result.Is_Ok (Offset_Result) then
               declare
                  Offset : constant ZoneInfo.UTC_Offset :=
                    Domain.Value_Object.UTC_Offset.Result.Value (Offset_Result);
                  Offset_Str : constant String :=
                    Domain.Value_Object.UTC_Offset.To_String (Offset);
               begin
                  Test_Result (True, "Got offset: " & Offset_Str);
                  Test_Result
                    (Offset_Str = "+00:00" or Offset_Str = "+01:00",
                     "Offset is valid for London (GMT/BST)");
               end;
            else
               Test_Result (False, "Failed to get offset");
            end if;
         end;
      else
         Test_Result (False, "Failed to create zone");
      end if;
   end Test_London_Offset;

   --  ========================================================================
   --  Test 3: Get UTC Offset for UTC
   --  ========================================================================
   procedure Test_UTC_Offset is
      Zone_Result : constant ZoneInfo.Zone_Id_Result :=
        Domain.Value_Object.Zone_Id.Result.Create ("UTC");
      Now : constant Ada.Calendar.Time := Ada.Calendar.Clock;
   begin
      Test_Header ("Get UTC Offset - UTC");

      if Domain.Value_Object.Zone_Id.Result.Is_Ok (Zone_Result) then
         declare
            UTC_Zone : constant ZoneInfo.Zone_Id :=
              Domain.Value_Object.Zone_Id.Result.Value (Zone_Result);
            Offset_Result : constant ZoneInfo.UTC_Offset_Result :=
              ZoneInfo.API.Get_UTC_Offset (UTC_Zone, Now);
         begin
            if Domain.Value_Object.UTC_Offset.Result.Is_Ok (Offset_Result) then
               declare
                  Offset : constant ZoneInfo.UTC_Offset :=
                    Domain.Value_Object.UTC_Offset.Result.Value (Offset_Result);
                  Offset_Str : constant String :=
                    Domain.Value_Object.UTC_Offset.To_String (Offset);
               begin
                  Test_Result (True, "Got offset: " & Offset_Str);
                  Test_Result (Offset_Str = "+00:00", "UTC offset is +00:00");
               end;
            else
               Test_Result (False, "Failed to get offset");
            end if;
         end;
      else
         Test_Result (False, "Failed to create zone");
      end if;
   end Test_UTC_Offset;

   --  ========================================================================
   --  Test 4: Convert Time from NYC to London
   --  ========================================================================
   procedure Test_Time_Conversion is
      NYC_Result : constant ZoneInfo.Zone_Id_Result :=
        Domain.Value_Object.Zone_Id.Result.Create ("America/New_York");
      London_Result : constant ZoneInfo.Zone_Id_Result :=
        Domain.Value_Object.Zone_Id.Result.Create ("Europe/London");
      Now : constant Ada.Calendar.Time := Ada.Calendar.Clock;
   begin
      Test_Header ("Convert Time - NYC to London");

      if Domain.Value_Object.Zone_Id.Result.Is_Ok (NYC_Result) and then
        Domain.Value_Object.Zone_Id.Result.Is_Ok (London_Result)
      then
         declare
            NYC_Zone : constant ZoneInfo.Zone_Id :=
              Domain.Value_Object.Zone_Id.Result.Value (NYC_Result);
            London_Zone : constant ZoneInfo.Zone_Id :=
              Domain.Value_Object.Zone_Id.Result.Value (London_Result);
            Result : constant ZoneInfo.Time_Result :=
              ZoneInfo.API.Convert_Time (Now, NYC_Zone, London_Zone);
         begin
            if Domain.Time_Conversion.Time_Result.Is_Ok (Result) then
               Test_Result (True, "Successfully converted time");
            else
               Test_Result (False, "Failed to convert time");
            end if;
         end;
      else
         Test_Result (False, "Failed to create zones");
      end if;
   end Test_Time_Conversion;

   --  ========================================================================
   --  Test 5: Error Handling - Invalid Zone Format
   --  ========================================================================
   procedure Test_Invalid_Zone_Format is
      --  Zone ID with invalid characters
      Zone_Result : constant ZoneInfo.Zone_Id_Result :=
        Domain.Value_Object.Zone_Id.Result.Create ("Invalid@Zone!");
   begin
      Test_Header ("Error Handling - Invalid Zone Format");

      Test_Result
        (Domain.Value_Object.Zone_Id.Result.Is_Error (Zone_Result),
         "Invalid zone format correctly rejected");
   end Test_Invalid_Zone_Format;

   --  ========================================================================
   --  Test 6: Error Handling - Non-existent Zone
   --  ========================================================================
   procedure Test_Nonexistent_Zone is
      --  Valid format, but doesn't exist in IANA database
      Zone_Result : constant ZoneInfo.Zone_Id_Result :=
        Domain.Value_Object.Zone_Id.Result.Create ("Invalid/Zone");
      Now : constant Ada.Calendar.Time := Ada.Calendar.Clock;
   begin
      Test_Header ("Error Handling - Non-existent Zone");

      if Domain.Value_Object.Zone_Id.Result.Is_Ok (Zone_Result) then
         declare
            Invalid_Zone : constant ZoneInfo.Zone_Id :=
              Domain.Value_Object.Zone_Id.Result.Value (Zone_Result);
            Offset_Result : constant ZoneInfo.UTC_Offset_Result :=
              ZoneInfo.API.Get_UTC_Offset (Invalid_Zone, Now);
         begin
            Test_Result
              (Domain.Value_Object.UTC_Offset.Result.Is_Error (Offset_Result),
               "Non-existent zone returns error on lookup");
         end;
      else
         Test_Result (False, "Zone creation unexpectedly failed");
      end if;
   end Test_Nonexistent_Zone;

   --  ========================================================================
   --  Test 7: DST Detection Functions
   --  ========================================================================
   procedure Test_DST_Functions is
      Zone_Result : constant ZoneInfo.Zone_Id_Result :=
        Domain.Value_Object.Zone_Id.Result.Create ("America/New_York");
   begin
      Test_Header ("DST Detection Functions");

      if Domain.Value_Object.Zone_Id.Result.Is_Ok (Zone_Result) then
         declare
            NYC_Zone : constant ZoneInfo.Zone_Id :=
              Domain.Value_Object.Zone_Id.Result.Value (Zone_Result);

            --  Test normal time (not in gap or ambiguous)
            --  January 15, 2024 at 12:00 PM - Standard Time
            Normal_Time : constant Ada.Calendar.Time :=
              Ada.Calendar.Time_Of (2024, 1, 15, 12.0 * 3600.0);

            --  Test ambiguous time (fall back)
            --  November 3, 2024 at 1:30 AM - occurs twice (EDT and EST)
            Ambiguous_Time : constant Ada.Calendar.Time :=
              Ada.Calendar.Time_Of (2024, 11, 3, 1.5 * 3600.0);

            --  Test gap time (spring forward)
            --  March 10, 2024 at 2:30 AM - never exists (jumps from 2:00 to 3:00)
            Gap_Time : constant Ada.Calendar.Time :=
              Ada.Calendar.Time_Of (2024, 3, 10, 2.5 * 3600.0);
         begin
            --  Test normal time
            Test_Result
              (not ZoneInfo.API.Is_Ambiguous_Time (NYC_Zone, Normal_Time),
               "Normal time is not ambiguous");
            Test_Result
              (not ZoneInfo.API.Is_Gap_Time (NYC_Zone, Normal_Time),
               "Normal time is not in gap");

            --  Test ambiguous time (fall back creates overlapping hour)
            Test_Result
              (ZoneInfo.API.Is_Ambiguous_Time (NYC_Zone, Ambiguous_Time),
               "Fall back time is detected as ambiguous");
            Test_Result
              (not ZoneInfo.API.Is_Gap_Time (NYC_Zone, Ambiguous_Time),
               "Fall back time is not in gap");

            --  Test gap time (spring forward creates missing hour)
            Test_Result
              (not ZoneInfo.API.Is_Ambiguous_Time (NYC_Zone, Gap_Time),
               "Spring forward time is not ambiguous");
            Test_Result
              (ZoneInfo.API.Is_Gap_Time (NYC_Zone, Gap_Time),
               "Spring forward time is detected as gap");
         end;
      else
         Test_Result (False, "Failed to create zone");
      end if;
   end Test_DST_Functions;

   --  ========================================================================
   --  Test 8: Find Local Timezone
   --  ========================================================================
   procedure Test_Local_Timezone is
      Local_Result : constant ZoneInfo.Zone_Id_Result :=
        ZoneInfo.API.Find_Local_Timezone;
   begin
      Test_Header ("Find Local Timezone");

      Test_Result
        (Domain.Value_Object.Zone_Id.Result.Is_Ok (Local_Result),
         "Successfully detected local timezone");

      if Domain.Value_Object.Zone_Id.Result.Is_Ok (Local_Result) then
         declare
            Local_Zone : constant ZoneInfo.Zone_Id :=
              Domain.Value_Object.Zone_Id.Result.Value (Local_Result);
            Zone_Str : constant String :=
              Domain.Value_Object.Zone_Id.To_String (Local_Zone);
         begin
            Test_Result
              (Zone_Str'Length > 0,
               "Local timezone has valid name: " & Zone_Str);
         end;
      end if;
   end Test_Local_Timezone;

   --  ========================================================================
   --  Test 9: Timezone Abbreviations
   --  ========================================================================
   procedure Test_Abbreviations is
      Zone_Result : constant ZoneInfo.Zone_Id_Result :=
        Domain.Value_Object.Zone_Id.Result.Create ("America/New_York");
      Now : constant Ada.Calendar.Time := Ada.Calendar.Clock;
   begin
      Test_Header ("Timezone Abbreviations");

      if Domain.Value_Object.Zone_Id.Result.Is_Ok (Zone_Result) then
         declare
            NYC_Zone : constant ZoneInfo.Zone_Id :=
              Domain.Value_Object.Zone_Id.Result.Value (Zone_Result);
            Abbr : constant String :=
              ZoneInfo.API.Get_Timezone_Abbreviation (NYC_Zone, Now);
         begin
            Test_Result
              (Abbr = "EST" or Abbr = "EDT",
               "NYC abbreviation is valid: " & Abbr);
         end;
      else
         Test_Result (False, "Failed to create zone");
      end if;
   end Test_Abbreviations;

   --  ========================================================================
   --  Test 10: DST Active Detection
   --  ========================================================================
   procedure Test_DST_Active is
      --  UTC never has DST
      UTC_Result : constant ZoneInfo.Zone_Id_Result :=
        Domain.Value_Object.Zone_Id.Result.Create ("UTC");
      Now : constant Ada.Calendar.Time := Ada.Calendar.Clock;
   begin
      Test_Header ("DST Active Detection");

      if Domain.Value_Object.Zone_Id.Result.Is_Ok (UTC_Result) then
         declare
            UTC_Zone : constant ZoneInfo.Zone_Id :=
              Domain.Value_Object.Zone_Id.Result.Value (UTC_Result);
            DST_Active : constant Boolean :=
              ZoneInfo.API.Is_DST_Active (UTC_Zone, Now);
         begin
            Test_Result (not DST_Active, "UTC never has DST active");
         end;
      else
         Test_Result (False, "Failed to create UTC zone");
      end if;
   end Test_DST_Active;

begin
   Put_Line ("========================================================");
   Put_Line ("  ZoneInfo.API Comprehensive Test Suite");
   Put_Line ("========================================================");
   Put_Line ("");
   Put_Line ("Testing TZif integration and timezone operations...");

   --  Initialize the timezone database
   ZoneInfo.API.Initialize;

   --  Run all tests
   Test_NYC_Offset;
   Test_London_Offset;
   Test_UTC_Offset;
   Test_Time_Conversion;
   Test_Invalid_Zone_Format;
   Test_Nonexistent_Zone;
   Test_DST_Functions;
   Test_Local_Timezone;
   Test_Abbreviations;
   Test_DST_Active;

   --  Summary
   Put_Line ("");
   Put_Line ("========================================================");
   Put_Line ("  TEST SUMMARY");
   Put_Line ("========================================================");
   Put_Line ("Assertions passed: " & Tests_Passed'Image);
   Put_Line ("Assertions failed: " & Tests_Failed'Image);
   Put_Line ("");

   if Tests_Failed = 0 then
      Put_Line ("[SUCCESS] All assertions passed!");
   else
      Put_Line ("[FAILURE] Some assertions failed.");
   end if;

   Put_Line ("========================================================");

end Comprehensive_Test;
