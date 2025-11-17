pragma Ada_2022;
--  ===========================================================================
--  Dst_Disambiguation_Validation
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
with ZoneInfo;
with ZoneInfo.API;
with Domain.Value_Object.Zone_Id.Result;
with Domain.Value_Object.UTC_Offset;
with Domain.Value_Object.UTC_Offset.Result;
with Domain.Value_Object.DST_Info.Result;

procedure DST_Disambiguation_Validation is

   use Ada.Text_IO;
   use Ada.Calendar;

   --  Statistics
   Total_Tests : Natural := 0;
   Tests_Passed : Natural := 0;
   Tests_Failed : Natural := 0;

   --  Test result reporting
   procedure Report_Pass (Test_Name : String) is
   begin
      Tests_Passed := Tests_Passed + 1;
      Put_Line ("[PASS] " & Test_Name);
   end Report_Pass;

   procedure Report_Fail (Test_Name : String; Reason : String) is
   begin
      Tests_Failed := Tests_Failed + 1;
      Put_Line ("[FAIL] " & Test_Name);
      Put_Line ("       Reason: " & Reason);
   end Report_Fail;

   --  Helper to format offsets
   function Format_Offset (Offset : ZoneInfo.UTC_Offset) return String is
      use Domain.Value_Object.UTC_Offset;
      Secs : constant Integer := To_Seconds (Offset);
      Hours : constant Integer := abs Secs / 3600;
      Mins : constant Integer := (abs Secs rem 3600) / 60;
      Sign : constant String := (if Secs < 0 then "-" else "+");
   begin
      return Sign & Hours'Image & "h" & Mins'Image & "m";
   end Format_Offset;

   --  ===========================================================================
   --  Test 1: Ambiguous Time Detection (Fall-Back)
   --  ===========================================================================
   procedure Test_Ambiguous_Time_Detection is
      use Domain.Value_Object.Zone_Id.Result;

      NYC_Result : constant ZoneInfo.Zone_Id_Result :=
        Create ("America/New_York");
      NYC : ZoneInfo.Zone_Id;

      --  Nov 3, 2024, 01:30:00 - Ambiguous (occurs twice)
      Ambiguous_Time : constant Time := Time_Of (2024, 11, 3, 1.5 * 3600.0);
   begin
      Total_Tests := Total_Tests + 1;

      if not Is_Ok (NYC_Result) then
         Report_Fail ("Ambiguous Time Detection",
           "Could not create America/New_York timezone");
         return;
      end if;

      NYC := Value (NYC_Result);

      if ZoneInfo.API.Is_Ambiguous_Time (NYC, Ambiguous_Time) then
         Report_Pass ("Ambiguous Time Detection - NYC Fall-Back");
      else
         Report_Fail ("Ambiguous Time Detection - NYC Fall-Back",
           "Expected time to be ambiguous but was not");
      end if;
   end Test_Ambiguous_Time_Detection;

   --  ===========================================================================
   --  Test 2: Gap Time Detection (Spring-Forward)
   --  ===========================================================================
   procedure Test_Gap_Time_Detection is
      use Domain.Value_Object.Zone_Id.Result;

      NYC_Result : constant ZoneInfo.Zone_Id_Result :=
        Create ("America/New_York");
      NYC : ZoneInfo.Zone_Id;

      --  Mar 10, 2024, 02:30:00 - Gap (never exists)
      Gap_Time : constant Time := Time_Of (2024, 3, 10, 2.5 * 3600.0);
   begin
      Total_Tests := Total_Tests + 1;

      if not Is_Ok (NYC_Result) then
         Report_Fail ("Gap Time Detection",
           "Could not create America/New_York timezone");
         return;
      end if;

      NYC := Value (NYC_Result);

      if ZoneInfo.API.Is_Gap_Time (NYC, Gap_Time) then
         Report_Pass ("Gap Time Detection - NYC Spring-Forward");
      else
         Report_Fail ("Gap Time Detection - NYC Spring-Forward",
           "Expected time to be in gap but was not");
      end if;
   end Test_Gap_Time_Detection;

   --  ===========================================================================
   --  Test 3: Normal Time Detection
   --  ===========================================================================
   procedure Test_Normal_Time_Detection is
      use Domain.Value_Object.Zone_Id.Result;

      NYC_Result : constant ZoneInfo.Zone_Id_Result :=
        Create ("America/New_York");
      NYC : ZoneInfo.Zone_Id;

      --  Jul 15, 2024, 14:00:00 - Normal (no transition)
      Normal_Time : constant Time := Time_Of (2024, 7, 15, 14.0 * 3600.0);
   begin
      Total_Tests := Total_Tests + 1;

      if not Is_Ok (NYC_Result) then
         Report_Fail ("Normal Time Detection",
           "Could not create America/New_York timezone");
         return;
      end if;

      NYC := Value (NYC_Result);

      if not ZoneInfo.API.Is_Ambiguous_Time (NYC, Normal_Time) and then
        not ZoneInfo.API.Is_Gap_Time (NYC, Normal_Time)
      then
         Report_Pass ("Normal Time Detection");
      else
         Report_Fail ("Normal Time Detection",
           "Expected normal time but was ambiguous or gap");
      end if;
   end Test_Normal_Time_Detection;

   --  ===========================================================================
   --  Test 4: Get_Ambiguity_Info for Ambiguous Time
   --  ===========================================================================
   procedure Test_Ambiguity_Info_Ambiguous is
      use Domain.Value_Object.Zone_Id.Result;
      use Domain.Value_Object.DST_Info.Result;
      use Domain.Value_Object.UTC_Offset;

      NYC_Result : constant ZoneInfo.Zone_Id_Result :=
        Create ("America/New_York");
      NYC : ZoneInfo.Zone_Id;

      Ambiguous_Time : constant Time := Time_Of (2024, 11, 3, 1.5 * 3600.0);
      Info_Result : ZoneInfo.Ambiguity_Info_Result;
      Info : ZoneInfo.Ambiguity_Info;
   begin
      Total_Tests := Total_Tests + 1;

      if not Is_Ok (NYC_Result) then
         Report_Fail ("Get_Ambiguity_Info (Ambiguous)",
           "Could not create America/New_York timezone");
         return;
      end if;

      NYC := Value (NYC_Result);
      Info_Result := ZoneInfo.API.Get_Ambiguity_Info (NYC, Ambiguous_Time);

      if not Is_Ok (Info_Result) then
         Report_Fail ("Get_Ambiguity_Info (Ambiguous)",
           "Failed to get ambiguity info");
         return;
      end if;

      Info := Value (Info_Result);

      if Info.Is_Ambiguous and not Info.Is_Gap then
         --  Verify EDT (-4h) and EST (-5h) offsets
         if To_Seconds (Info.Earlier_Offset) = -14400 and
           To_Seconds (Info.Later_Offset) = -18000
         then
            Report_Pass ("Get_Ambiguity_Info (Ambiguous) - Correct Offsets");
         else
            Report_Fail ("Get_Ambiguity_Info (Ambiguous)",
              "Incorrect offsets: Earlier=" &
              Format_Offset (Info.Earlier_Offset) &
              " Later=" & Format_Offset (Info.Later_Offset));
         end if;
      else
         Report_Fail ("Get_Ambiguity_Info (Ambiguous)",
           "Expected Is_Ambiguous=True, Is_Gap=False");
      end if;
   end Test_Ambiguity_Info_Ambiguous;

   --  ===========================================================================
   --  Test 5: Get_Ambiguity_Info for Gap Time
   --  ===========================================================================
   procedure Test_Ambiguity_Info_Gap is
      use Domain.Value_Object.Zone_Id.Result;
      use Domain.Value_Object.DST_Info.Result;
      use Domain.Value_Object.UTC_Offset;

      NYC_Result : constant ZoneInfo.Zone_Id_Result :=
        Create ("America/New_York");
      NYC : ZoneInfo.Zone_Id;

      Gap_Time : constant Time := Time_Of (2024, 3, 10, 2.5 * 3600.0);
      Info_Result : ZoneInfo.Ambiguity_Info_Result;
      Info : ZoneInfo.Ambiguity_Info;
   begin
      Total_Tests := Total_Tests + 1;

      if not Is_Ok (NYC_Result) then
         Report_Fail ("Get_Ambiguity_Info (Gap)",
           "Could not create America/New_York timezone");
         return;
      end if;

      NYC := Value (NYC_Result);
      Info_Result := ZoneInfo.API.Get_Ambiguity_Info (NYC, Gap_Time);

      if not Is_Ok (Info_Result) then
         Report_Fail ("Get_Ambiguity_Info (Gap)",
           "Failed to get ambiguity info");
         return;
      end if;

      Info := Value (Info_Result);

      if Info.Is_Gap and not Info.Is_Ambiguous then
         --  Verify EST (-5h) before gap and EDT (-4h) after gap
         if To_Seconds (Info.Earlier_Offset) = -18000 and
           To_Seconds (Info.Post_Gap_Offset) = -14400
         then
            Report_Pass ("Get_Ambiguity_Info (Gap) - Correct Offsets");
         else
            Report_Fail ("Get_Ambiguity_Info (Gap)",
              "Incorrect offsets: Pre-gap=" &
              Format_Offset (Info.Earlier_Offset) &
              " Post-gap=" & Format_Offset (Info.Post_Gap_Offset));
         end if;
      else
         Report_Fail ("Get_Ambiguity_Info (Gap)",
           "Expected Is_Gap=True, Is_Ambiguous=False");
      end if;
   end Test_Ambiguity_Info_Gap;

   --  ===========================================================================
   --  Test 6: Disambiguation Strategy - Prefer_Earlier
   --  ===========================================================================
   procedure Test_Strategy_Prefer_Earlier is
      use Domain.Value_Object.Zone_Id.Result;
      use Domain.Value_Object.UTC_Offset.Result;
      use Domain.Value_Object.UTC_Offset;

      NYC_Result : constant ZoneInfo.Zone_Id_Result :=
        Create ("America/New_York");
      NYC : ZoneInfo.Zone_Id;

      Ambiguous_Time : constant Time := Time_Of (2024, 11, 3, 1.5 * 3600.0);
      Offset_Result : ZoneInfo.UTC_Offset_Result;
   begin
      Total_Tests := Total_Tests + 1;

      if not Is_Ok (NYC_Result) then
         Report_Fail ("Strategy Prefer_Earlier",
           "Could not create America/New_York timezone");
         return;
      end if;

      NYC := Value (NYC_Result);
      Offset_Result := ZoneInfo.API.Get_UTC_Offset_With_Disambiguation
        (NYC, Ambiguous_Time, ZoneInfo.Prefer_Earlier);

      if Is_Ok (Offset_Result) then
         --  Should return EDT (-4h)
         if To_Seconds (Value (Offset_Result)) = -14400 then
            Report_Pass ("Strategy Prefer_Earlier - Returns EDT");
         else
            Report_Fail ("Strategy Prefer_Earlier",
              "Expected EDT (-4h), got " &
              Format_Offset (Value (Offset_Result)));
         end if;
      else
         Report_Fail ("Strategy Prefer_Earlier",
           "Failed to get offset with Prefer_Earlier strategy");
      end if;
   end Test_Strategy_Prefer_Earlier;

   --  ===========================================================================
   --  Test 7: Disambiguation Strategy - Prefer_Later
   --  ===========================================================================
   procedure Test_Strategy_Prefer_Later is
      use Domain.Value_Object.Zone_Id.Result;
      use Domain.Value_Object.UTC_Offset.Result;
      use Domain.Value_Object.UTC_Offset;

      NYC_Result : constant ZoneInfo.Zone_Id_Result :=
        Create ("America/New_York");
      NYC : ZoneInfo.Zone_Id;

      Ambiguous_Time : constant Time := Time_Of (2024, 11, 3, 1.5 * 3600.0);
      Offset_Result : ZoneInfo.UTC_Offset_Result;
   begin
      Total_Tests := Total_Tests + 1;

      if not Is_Ok (NYC_Result) then
         Report_Fail ("Strategy Prefer_Later",
           "Could not create America/New_York timezone");
         return;
      end if;

      NYC := Value (NYC_Result);
      Offset_Result := ZoneInfo.API.Get_UTC_Offset_With_Disambiguation
        (NYC, Ambiguous_Time, ZoneInfo.Prefer_Later);

      if Is_Ok (Offset_Result) then
         --  Should return EST (-5h)
         if To_Seconds (Value (Offset_Result)) = -18000 then
            Report_Pass ("Strategy Prefer_Later - Returns EST");
         else
            Report_Fail ("Strategy Prefer_Later",
              "Expected EST (-5h), got " &
              Format_Offset (Value (Offset_Result)));
         end if;
      else
         Report_Fail ("Strategy Prefer_Later",
           "Failed to get offset with Prefer_Later strategy");
      end if;
   end Test_Strategy_Prefer_Later;

   --  ===========================================================================
   --  Test 8: Disambiguation Strategy - Raise_On_Ambiguous
   --  ===========================================================================
   procedure Test_Strategy_Raise_On_Ambiguous is
      use Domain.Value_Object.Zone_Id.Result;
      use Domain.Value_Object.UTC_Offset.Result;

      NYC_Result : constant ZoneInfo.Zone_Id_Result :=
        Create ("America/New_York");
      NYC : ZoneInfo.Zone_Id;

      Ambiguous_Time : constant Time := Time_Of (2024, 11, 3, 1.5 * 3600.0);
      Offset_Result : ZoneInfo.UTC_Offset_Result;
   begin
      Total_Tests := Total_Tests + 1;

      if not Is_Ok (NYC_Result) then
         Report_Fail ("Strategy Raise_On_Ambiguous",
           "Could not create America/New_York timezone");
         return;
      end if;

      NYC := Value (NYC_Result);
      Offset_Result := ZoneInfo.API.Get_UTC_Offset_With_Disambiguation
        (NYC, Ambiguous_Time, ZoneInfo.Raise_On_Ambiguous);

      if Is_Error (Offset_Result) then
         Report_Pass ("Strategy Raise_On_Ambiguous - Returns Error");
      else
         Report_Fail ("Strategy Raise_On_Ambiguous",
           "Expected error but got success");
      end if;
   end Test_Strategy_Raise_On_Ambiguous;

   --  ===========================================================================
   --  Test 9: All Strategies Return Same Offset for Normal Time
   --  ===========================================================================
   procedure Test_Strategies_Normal_Time is
      use Domain.Value_Object.Zone_Id.Result;
      use Domain.Value_Object.UTC_Offset.Result;
      use Domain.Value_Object.UTC_Offset;

      NYC_Result : constant ZoneInfo.Zone_Id_Result :=
        Create ("America/New_York");
      NYC : ZoneInfo.Zone_Id;

      Normal_Time : constant Time := Time_Of (2024, 7, 15, 14.0 * 3600.0);
      Earlier_Result : ZoneInfo.UTC_Offset_Result;
      Later_Result : ZoneInfo.UTC_Offset_Result;
      Raise_Result : ZoneInfo.UTC_Offset_Result;
   begin
      Total_Tests := Total_Tests + 1;

      if not Is_Ok (NYC_Result) then
         Report_Fail ("Strategies Normal Time",
           "Could not create America/New_York timezone");
         return;
      end if;

      NYC := Value (NYC_Result);

      Earlier_Result := ZoneInfo.API.Get_UTC_Offset_With_Disambiguation
        (NYC, Normal_Time, ZoneInfo.Prefer_Earlier);
      Later_Result := ZoneInfo.API.Get_UTC_Offset_With_Disambiguation
        (NYC, Normal_Time, ZoneInfo.Prefer_Later);
      Raise_Result := ZoneInfo.API.Get_UTC_Offset_With_Disambiguation
        (NYC, Normal_Time, ZoneInfo.Raise_On_Ambiguous);

      if Is_Ok (Earlier_Result) and
        Is_Ok (Later_Result) and
        Is_Ok (Raise_Result)
      then
         if To_Seconds (Value (Earlier_Result)) =
           To_Seconds (Value (Later_Result)) and
           To_Seconds (Value (Later_Result)) =
           To_Seconds (Value (Raise_Result))
         then
            Report_Pass ("Strategies Normal Time - All Return Same Offset");
         else
            Report_Fail ("Strategies Normal Time",
              "Strategies returned different offsets");
         end if;
      else
         Report_Fail ("Strategies Normal Time",
           "One or more strategies failed");
      end if;
   end Test_Strategies_Normal_Time;

   --  ===========================================================================
   --  Test 10: Multiple Timezones with DST
   --  ===========================================================================
   procedure Test_Multiple_Timezones is
      use Domain.Value_Object.Zone_Id.Result;

      --  Test London DST (GMT to BST)
      London_Result : constant ZoneInfo.Zone_Id_Result :=
        Create ("Europe/London");
      London : ZoneInfo.Zone_Id;
      --  Mar 31, 2024, 01:30:00 - Gap in London
      London_Gap : constant Time := Time_Of (2024, 3, 31, 1.5 * 3600.0);

      --  Test Sydney DST
      Sydney_Result : constant ZoneInfo.Zone_Id_Result :=
        Create ("Australia/Sydney");
      Sydney : ZoneInfo.Zone_Id;
      --  Apr 7, 2024, 02:30:00 - Ambiguous in Sydney
      Sydney_Ambiguous : constant Time := Time_Of (2024, 4, 7, 2.5 * 3600.0);

      Passed : Natural := 0;
   begin
      Total_Tests := Total_Tests + 1;

      --  Test London
      if Is_Ok (London_Result) then
         London := Value (London_Result);
         if ZoneInfo.API.Is_Gap_Time (London, London_Gap) then
            Passed := Passed + 1;
         end if;
      end if;

      --  Test Sydney
      if Is_Ok (Sydney_Result) then
         Sydney := Value (Sydney_Result);
         if ZoneInfo.API.Is_Ambiguous_Time (Sydney, Sydney_Ambiguous) then
            Passed := Passed + 1;
         end if;
      end if;

      if Passed = 2 then
         Report_Pass ("Multiple Timezones - London & Sydney DST");
      else
         Report_Fail ("Multiple Timezones",
           "Expected 2 passed, got" & Passed'Image);
      end if;
   end Test_Multiple_Timezones;

   --  ===========================================================================
   --  Main Test Runner
   --  ===========================================================================
begin
   ZoneInfo.API.Initialize;

   Put_Line ("===========================================================");
   Put_Line ("  DST Disambiguation Validation Test Suite");
   Put_Line ("  Validating FR-10.3, FR-10.4, FR-10.5");
   Put_Line ("===========================================================");
   New_Line;

   --  Run all tests
   Test_Ambiguous_Time_Detection;
   Test_Gap_Time_Detection;
   Test_Normal_Time_Detection;
   Test_Ambiguity_Info_Ambiguous;
   Test_Ambiguity_Info_Gap;
   Test_Strategy_Prefer_Earlier;
   Test_Strategy_Prefer_Later;
   Test_Strategy_Raise_On_Ambiguous;
   Test_Strategies_Normal_Time;
   Test_Multiple_Timezones;

   --  Print summary
   New_Line;
   Put_Line ("===========================================================");
   Put_Line ("  Test Summary");
   Put_Line ("===========================================================");
   Put_Line ("Total Tests:  " & Total_Tests'Image);
   Put_Line ("Tests Passed: " & Tests_Passed'Image);
   Put_Line ("Tests Failed: " & Tests_Failed'Image);

   if Tests_Failed = 0 then
      Put_Line ("");
      Put_Line ("[SUCCESS] All DST disambiguation tests passed!");
      Put_Line ("Requirements FR-10.3, FR-10.4, FR-10.5 validated.");
   else
      Put_Line ("");
      Put_Line ("[FAILURE] Some tests failed.");
   end if;

   Put_Line ("===========================================================");

end DST_Disambiguation_Validation;
