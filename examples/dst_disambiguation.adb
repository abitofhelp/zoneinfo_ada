pragma Ada_2022;
--  ===========================================================================
--  DST Disambiguation Example
--  ===========================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--
--  Purpose:
--    Demonstrates handling of ambiguous times during DST transitions.
--
--  Requirements:
--    - FR-10.3: Disambiguation strategies
--    - FR-10.4: Detailed ambiguity information
--    - FR-10.5: Explicit user choice
--
--  Description:
--    This example shows how to:
--    1. Detect ambiguous times (DST fall-back when clocks go backwards)
--    2. Detect gap times (DST spring-forward when clocks jump ahead)
--    3. Use disambiguation strategies to choose between multiple offsets
--    4. Get detailed information about DST transitions
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

procedure DST_Disambiguation is

   use Ada.Text_IO;
   use Ada.Calendar;

   --  ========================================================================
   --  Helper Functions
   --  ========================================================================

   function Format_Offset (Offset : ZoneInfo.UTC_Offset) return String is
      use Domain.Value_Object.UTC_Offset;
      Secs : constant Integer := To_Seconds (Offset);
      Hours : constant Integer := abs Secs / 3600;
      Mins : constant Integer := (abs Secs rem 3600) / 60;
      Sign : constant String := (if Secs < 0 then "-" else "+");
   begin
      return Sign & Hours'Image & "h" & Mins'Image & "m";
   end Format_Offset;

   procedure Print_Section (Title : String) is
   begin
      New_Line;
      Put_Line ("===========================================================");
      Put_Line ("  " & Title);
      Put_Line ("===========================================================");
      New_Line;
   end Print_Section;

   --  ========================================================================
   --  Example Functions
   --  ========================================================================

   procedure Example_1_Ambiguous_Time is
      use Domain.Value_Object.Zone_Id.Result;
      use Domain.Value_Object.DST_Info.Result;
      use Domain.Value_Object.UTC_Offset.Result;

      --  America/New_York falls back on first Sunday in November
      --  In 2024: Nov 3, 2:00 AM EDT to 1:00 AM EST
      --  Times between 1:00-2:00 AM occur TWICE (ambiguous)

      NYC_Result : constant ZoneInfo.Zone_Id_Result :=
        Create ("America/New_York");
      NYC : ZoneInfo.Zone_Id;

      --  Create time: 2024-11-03 01:30:00 (ambiguous - occurs twice!)
      Ambiguous_Time : constant Time := Time_Of (2024, 11, 3, 1.5 * 3600.0);
   begin
      if not Is_Ok (NYC_Result) then
         Put_Line ("Error: Could not create NYC timezone");
         return;
      end if;

      NYC := Value (NYC_Result);

      Put_Line ("Scenario: DST Fall-Back in New York");
      Put_Line ("Date: November 3, 2024, 01:30:00");
      Put_Line ("Event: Clocks fall back from 02:00 EDT to 01:00 EST");
      New_Line;

      --  Check if time is ambiguous
      if ZoneInfo.API.Is_Ambiguous_Time (NYC, Ambiguous_Time) then
         Put_Line ("[OK] Time is AMBIGUOUS (occurs twice)");
         New_Line;

         --  Get detailed ambiguity information
         declare
            Info_Result : constant ZoneInfo.Ambiguity_Info_Result :=
              ZoneInfo.API.Get_Ambiguity_Info (NYC, Ambiguous_Time);
         begin
            if Is_Ok (Info_Result) then
               declare
                  Info : constant ZoneInfo.Ambiguity_Info :=
                    Value (Info_Result);
               begin
                  Put_Line ("Ambiguity Details:");
                  Put_Line ("  First occurrence (EDT):  " &
                    Format_Offset (Info.Earlier_Offset) & " (Daylight Time)");
                  Put_Line ("  Second occurrence (EST): " &
                    Format_Offset (Info.Later_Offset) & " (Standard Time)");
                  Put_Line ("  Difference: 1 hour");
                  New_Line;

                  --  Demonstrate all three disambiguation strategies
                  Put_Line ("Disambiguation Strategies:");
                  New_Line;

                  --  Strategy 1: Prefer Earlier (default - choose DST)
                  declare
                     Offset_Result : constant ZoneInfo.UTC_Offset_Result :=
                       ZoneInfo.API.Get_UTC_Offset_With_Disambiguation
                         (NYC, Ambiguous_Time, ZoneInfo.Prefer_Earlier);
                  begin
                     if Is_Ok (Offset_Result) then
                        Put_Line ("1. Prefer_Earlier: " &
                          Format_Offset (Value (Offset_Result)) &
                          " (chooses first occurrence - EDT)");
                     end if;
                  end;

                  --  Strategy 2: Prefer Later (choose standard time)
                  declare
                     Offset_Result : constant ZoneInfo.UTC_Offset_Result :=
                       ZoneInfo.API.Get_UTC_Offset_With_Disambiguation
                         (NYC, Ambiguous_Time, ZoneInfo.Prefer_Later);
                  begin
                     if Is_Ok (Offset_Result) then
                        Put_Line ("2. Prefer_Later:   " &
                          Format_Offset (Value (Offset_Result)) &
                          " (chooses second occurrence - EST)");
                     end if;
                  end;

                  --  Strategy 3: Raise on ambiguous (returns error)
                  declare
                     Offset_Result : constant ZoneInfo.UTC_Offset_Result :=
                       ZoneInfo.API.Get_UTC_Offset_With_Disambiguation
                         (NYC, Ambiguous_Time, ZoneInfo.Raise_On_Ambiguous);
                  begin
                     if Is_Error (Offset_Result) then
                        Put_Line ("3. Raise_On_Ambiguous: ERROR " &
                          "(requires explicit user choice)");
                     end if;
                  end;
               end;
            end if;
         end;
      else
         Put_Line ("[FAIL] Time is not ambiguous (unexpected!)");
      end if;
   end Example_1_Ambiguous_Time;

   procedure Example_2_Gap_Time is
      use Domain.Value_Object.Zone_Id.Result;
      use Domain.Value_Object.DST_Info.Result;

      --  America/New_York springs forward on second Sunday in March
      --  In 2024: Mar 10, 2:00 AM EST to 3:00 AM EDT
      --  Times between 2:00-3:00 AM NEVER EXIST (gap)

      NYC_Result : constant ZoneInfo.Zone_Id_Result :=
        Create ("America/New_York");
      NYC : ZoneInfo.Zone_Id;

      --  Create time: 2024-03-10 02:30:00 (in gap - doesn't exist!)
      Gap_Time : constant Time := Time_Of (2024, 3, 10, 2.5 * 3600.0);
   begin
      if not Is_Ok (NYC_Result) then
         Put_Line ("Error: Could not create NYC timezone");
         return;
      end if;

      NYC := Value (NYC_Result);

      Put_Line ("Scenario: DST Spring-Forward in New York");
      Put_Line ("Date: March 10, 2024, 02:30:00");
      Put_Line ("Event: Clocks spring forward from 02:00 EST to 03:00 EDT");
      New_Line;

      --  Check if time is in gap
      if ZoneInfo.API.Is_Gap_Time (NYC, Gap_Time) then
         Put_Line ("[OK] Time is in GAP (never exists)");
         New_Line;

         --  Get detailed gap information
         declare
            Info_Result : constant ZoneInfo.Ambiguity_Info_Result :=
              ZoneInfo.API.Get_Ambiguity_Info (NYC, Gap_Time);
         begin
            if Is_Ok (Info_Result) then
               declare
                  Info : constant ZoneInfo.Ambiguity_Info :=
                    Value (Info_Result);
               begin
                  Put_Line ("Gap Details:");
                  Put_Line ("  Before gap (EST): " &
                    Format_Offset (Info.Earlier_Offset) & " (Standard Time)");
                  Put_Line ("  After gap (EDT):  " &
                    Format_Offset (Info.Post_Gap_Offset) & " (Daylight Time)");
                  Put_Line ("  Gap duration: 1 hour " &
                    "(these times don't exist)");
                  New_Line;

                  Put_Line ("Note: Any disambiguation strategy will use the " &
                    "post-gap offset");
                  Put_Line ("      since the requested time never existed.");
               end;
            end if;
         end;
      else
         Put_Line ("[FAIL] Time is not in gap (unexpected!)");
      end if;
   end Example_2_Gap_Time;

   procedure Example_3_Normal_Time is
      use Domain.Value_Object.Zone_Id.Result;
      use Domain.Value_Object.DST_Info.Result;

      NYC_Result : constant ZoneInfo.Zone_Id_Result :=
        Create ("America/New_York");
      NYC : ZoneInfo.Zone_Id;

      --  Create time: 2024-07-15 14:00:00 (normal summer time)
      Normal_Time : constant Time := Time_Of (2024, 7, 15, 14.0 * 3600.0);
   begin
      if not Is_Ok (NYC_Result) then
         Put_Line ("Error: Could not create NYC timezone");
         return;
      end if;

      NYC := Value (NYC_Result);

      Put_Line ("Scenario: Normal Time (No DST Transition)");
      Put_Line ("Date: July 15, 2024, 14:00:00 (2 PM)");
      New_Line;

      --  Check if time is normal
      if not ZoneInfo.API.Is_Ambiguous_Time (NYC, Normal_Time) and then
        not ZoneInfo.API.Is_Gap_Time (NYC, Normal_Time)
      then
         Put_Line ("[OK] Time is NORMAL (not ambiguous, not in gap)");
         New_Line;

         --  Get information - should show single unambiguous offset
         declare
            Info_Result : constant ZoneInfo.Ambiguity_Info_Result :=
              ZoneInfo.API.Get_Ambiguity_Info (NYC, Normal_Time);
         begin
            if Is_Ok (Info_Result) then
               declare
                  Info : constant ZoneInfo.Ambiguity_Info :=
                    Value (Info_Result);
               begin
                  Put_Line ("Offset: " &
                    Format_Offset (Info.Earlier_Offset) &
                    " (EDT - Daylight Time)");
                  Put_Line ("All disambiguation strategies " &
                    "return same offset");
               end;
            end if;
         end;
      else
         Put_Line ("[FAIL] Time is not normal (unexpected!)");
      end if;
   end Example_3_Normal_Time;

   procedure Example_4_Production_Pattern is
      use Domain.Value_Object.Zone_Id.Result;
      use Domain.Value_Object.DST_Info.Result;
      use Domain.Value_Object.UTC_Offset.Result;

      NYC_Result : constant ZoneInfo.Zone_Id_Result :=
        Create ("America/New_York");
      NYC : ZoneInfo.Zone_Id;

      User_Time : constant Time := Time_Of (2024, 11, 3, 1.5 * 3600.0);
   begin
      if not Is_Ok (NYC_Result) then
         Put_Line ("Error: Could not create NYC timezone");
         return;
      end if;

      NYC := Value (NYC_Result);

      Put_Line ("Production Pattern: Safe Timezone Handling");
      Put_Line ("=========================================");
      New_Line;
      Put_Line ("1. Always check for ambiguity before converting times");
      Put_Line ("2. Use Get_Ambiguity_Info to understand the situation");
      Put_Line ("3. Choose appropriate disambiguation strategy");
      Put_Line ("4. Handle errors gracefully");
      New_Line;

      Put_Line ("Example Code Pattern:");
      Put_Line ("---------------------");
      New_Line;

      --  Get ambiguity information
      declare
         Info_Result : constant ZoneInfo.Ambiguity_Info_Result :=
           ZoneInfo.API.Get_Ambiguity_Info (NYC, User_Time);
      begin
         if not Is_Ok (Info_Result) then
            Put_Line ("ERROR: Failed to get ambiguity info");
            return;
         end if;

         declare
            Info : constant ZoneInfo.Ambiguity_Info := Value (Info_Result);
            Strategy : ZoneInfo.Disambiguation_Strategy;
         begin
            --  Decide strategy based on situation
            if Info.Is_Ambiguous then
               Put_Line ("[->] Time is ambiguous, prompting user...");
               --  In real app: prompt user to choose earlier/later
               Strategy := ZoneInfo.Prefer_Earlier;
               Put_Line ("  User chose: Prefer_Earlier (first occurrence)");

            elsif Info.Is_Gap then
               Put_Line ("[->] Time is in gap, using post-gap offset...");
               --  Doesn't matter for gaps
               Strategy := ZoneInfo.Prefer_Earlier;

            else
               Put_Line ("[->] Time is normal, using standard lookup...");
               Strategy := ZoneInfo.Prefer_Earlier;  --  Default is fine
            end if;

            --  Get offset with chosen strategy
            declare
               Offset_Result : constant ZoneInfo.UTC_Offset_Result :=
                 ZoneInfo.API.Get_UTC_Offset_With_Disambiguation
                   (NYC, User_Time, Strategy);
            begin
               if Is_Ok (Offset_Result) then
                  Put_Line ("[OK] Successfully resolved: " &
                    Format_Offset (Value (Offset_Result)));
               else
                  Put_Line ("[FAIL] Failed to resolve offset");
               end if;
            end;
         end;
      end;
   end Example_4_Production_Pattern;

begin
   ZoneInfo.API.Initialize;

   Put_Line ("===========================================================");
   Put_Line ("  DST Disambiguation Examples");
   Put_Line ("  ZoneInfo Library - Ada 2022");
   Put_Line ("===========================================================");

   Print_Section ("Example 1: Ambiguous Time (DST Fall-Back)");
   Example_1_Ambiguous_Time;

   Print_Section ("Example 2: Gap Time (DST Spring-Forward)");
   Example_2_Gap_Time;

   Print_Section ("Example 3: Normal Time (No Transition)");
   Example_3_Normal_Time;

   Print_Section ("Example 4: Production Pattern");
   Example_4_Production_Pattern;

   New_Line;
   Put_Line ("===========================================================");
   Put_Line ("  All examples completed successfully!");
   Put_Line ("===========================================================");

end DST_Disambiguation;
